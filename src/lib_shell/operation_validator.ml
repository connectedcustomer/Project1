(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

type limits = {
  mempool_worker_limits : Mempool_worker.limits ;
  mempool_peer_worker_limits : Mempool_peer_worker.limits ;
}


type identifier = (Chain_id.t * Protocol_hash.t)

module type T = sig

  val identifier: identifier
  val parameters: (limits * Distributed_db.chain_db)

  type t
  val create: unit -> t tzresult Lwt.t
  val shutdown: t -> unit Lwt.t

  val timestamp: t -> Time.t

  val inject: t -> Operation.t -> unit tzresult Lwt.t
  val validate: t -> P2p_peer.Id.t -> Mempool.t -> unit tzresult Lwt.t
  val head: t -> Block_hash.t -> unit tzresult Lwt.t

  val forget: t -> P2p_peer.Id.t -> unit Lwt.t

end

type 'mt pre_t = {
  m : (module T with type t = 'mt) ;
  t : 'mt
}
and t = T : 'mt pre_t -> t


(* Global state *)
module Registry : sig
  (* A restricted HashTbl with a unique value *)
  val add: identifier -> t -> unit
  val find_opt: identifier -> t option
  val remove: identifier -> unit
  val fold: (identifier -> t -> 'a -> 'a) -> 'a -> 'a
end = struct

  module ChainProto_Table =
    Hashtbl.Make(struct
      type t = identifier
      let equal (cid1, ph1) (cid2, ph2) =
        Chain_id.equal cid1 cid2 && Protocol_hash.equal ph1 ph2
      let hash = Hashtbl.hash
    end)

  let chain_proto_table = ChainProto_Table.create 2

  let add = ChainProto_Table.add chain_proto_table
  let find_opt = ChainProto_Table.find_opt chain_proto_table
  let remove = ChainProto_Table.remove chain_proto_table
  let fold f acc = ChainProto_Table.fold f chain_proto_table acc
end


module type ARG = sig
  val limits: limits
  val chain_db: Distributed_db.chain_db
  val chain_id: Chain_id.t
end

module Make(Proto: Registered_protocol.T)(Arg: ARG): T = struct

  (** Underlying modules to delegate low-level ops to *)
  module Proto = Proto
  module Mempool_worker
    : Mempool_worker.T with module Proto = Proto
    = Mempool_worker.Make(Proto)
  module Mempool_peer_worker
    : Mempool_peer_worker.T with module Mempool_worker = Mempool_worker
    = Mempool_peer_worker.Make(Mempool_worker)

  (** Misc data that is better to remember *)
  let identifier = (Arg.chain_id, Proto.hash)
  let parameters = (Arg.limits, Arg.chain_db)

  (** State that the system relies on *)
  type t = {
    mutable timestamp: Time.t;
    mutable mempool_worker: Mempool_worker.t;
    mempool_peer_workers: Mempool_peer_worker.t P2p_peer.Id.Table.t;
  }
  let timestamp { timestamp } = timestamp

  (* NOTE: Only create a single value. *)
  let create () =
    Mempool_worker.create Arg.limits.mempool_worker_limits Arg.chain_db
    >>=? fun mempool_worker ->
    return {
      timestamp = Time.now () ;
      mempool_worker ;
      (* TODO: what's a good default for the init size of the table? *)
      mempool_peer_workers = P2p_peer.Id.Table.create 16 ;
    }

  let shutdown t =
    let mpws =
      P2p_peer.Id.Table.fold
        (fun _ mpw acc -> mpw :: acc)
        t.mempool_peer_workers
        [] in
    Lwt.join (
      List.map
        (fun mpw ->
           Mempool_peer_worker.shutdown mpw >>= fun (_: Mempool_peer_worker.input) ->
           Lwt.return_unit)
        mpws) >>= fun () ->
    Mempool_worker.shutdown t.mempool_worker >>= fun () ->
    Lwt.return_unit

  let inject t operation =
    Mempool_worker.parse t.mempool_worker operation >>=? fun op ->
    Mempool_worker.validate t.mempool_worker op >>=? fun (_: Mempool_worker.result) ->
    return_unit

  let validate t peer mempool =
    begin
      match P2p_peer.Id.Table.find_opt t.mempool_peer_workers peer with
      | None ->
          Mempool_peer_worker.create
            Arg.limits.mempool_peer_worker_limits
            peer
            t.mempool_worker
            [] >>= fun mpw ->
          P2p_peer.Id.Table.add t.mempool_peer_workers peer mpw;
          Lwt.return mpw
      | Some mpw -> Lwt.return mpw
    end >>= fun mempool_peer_worker ->
    Mempool_peer_worker.validate
      t.mempool_worker
      mempool_peer_worker
      (mempool.Mempool.known_valid
       @ (Operation_hash.Set.fold (fun h acc -> h :: acc) mempool.Mempool.pending []))

  let head t _ =
    let recycling =
      P2p_peer.Id.Table.fold
        (fun peer mpw acc ->
           (Mempool_peer_worker.shutdown mpw >>= fun input -> Lwt.return (peer, input))
           :: acc)
        t.mempool_peer_workers
        [] in
    Lwt.join (List.map (fun p -> p >>= fun _ -> Lwt.return_unit) recycling) >>= fun () ->
    P2p_peer.Id.Table.clear t.mempool_peer_workers;
    Mempool_worker.shutdown t.mempool_worker >>= fun () ->
    Mempool_worker.create Arg.limits.mempool_worker_limits Arg.chain_db >>=? fun mempool_worker ->
    t.mempool_worker <- mempool_worker;
    t.timestamp <- Time.now ();
    Lwt_list.iter_p
      (fun pi ->
         pi >>= fun (peer, input) ->
         Mempool_peer_worker.create
           Arg.limits.mempool_peer_worker_limits
           peer
           t.mempool_worker
           input >>= fun mpw ->
         P2p_peer.Id.Table.add t.mempool_peer_workers peer mpw;
         Lwt.return_unit)
      recycling >>= fun () ->
    return_unit

  let forget t peer =
    match P2p_peer.Id.Table.find_opt t.mempool_peer_workers peer with
    | None -> Lwt.return_unit
    | Some mpw ->
        Mempool_peer_worker.shutdown mpw >>= fun (_: Mempool_peer_worker.input) ->
        Lwt.return_unit

end


let create limits (module Proto: Registered_protocol.T) chain_db =
  let chain_state = Distributed_db.chain_state chain_db in
  let chain_id = State.Chain.id chain_state in
  match Registry.find_opt (chain_id, Proto.hash) with
  | None ->
      let module M =
        Make(Proto)(struct
          let limits = limits
          let chain_db = chain_db
          let chain_id = chain_id
        end) in
      M.create () >>=? fun (t: M.t) ->
      let pre_t: M.t pre_t = { m = (module M) ; t } in
      let t: t = T pre_t in
      Registry.add M.identifier t;
      return t
  | Some t ->
      return t

let shutdown_wrap: type mt. mt pre_t -> unit Lwt.t
  = fun pre_t ->
    let module M: T with type t = mt = (val pre_t.m) in
    Registry.remove M.identifier;
    M.shutdown pre_t.t
let shutdown (t:t) =
  let T pre_t = t in
  shutdown_wrap pre_t

let flush_wrap: type mt. mt pre_t -> Block_hash.t -> unit tzresult Lwt.t
  = fun pre_t head ->
    let module M: T with type t = mt = (val pre_t.m) in
    M.head pre_t.t head
let flush (t:t) head =
  let T pre_t = t in
  flush_wrap pre_t head

let notify_wrap: type mt. mt pre_t -> P2p_peer.Id.t -> Mempool.t -> unit tzresult Lwt.t
  = fun pre_t peer mempool ->
    let module M: T with type t = mt = (val pre_t.m) in
    M.validate pre_t.t peer mempool
let notify_operations (t:t) peer mempool =
  let T pre_t = t in
  notify_wrap pre_t peer mempool

let inject_wrap: type mt. mt pre_t -> Operation.t -> unit tzresult Lwt.t
  = fun pre_t op ->
    let module M: T with type t = mt = (val pre_t.m) in
    M.inject pre_t.t op
let inject_operation (t:t) op =
  let T pre_t = t in
  inject_wrap pre_t op

let timestamp_wrap: type mt. mt pre_t -> Time.t
  = fun pre_t ->
    let module M: T with type t = mt = (val pre_t.m) in
    M.timestamp pre_t.t
let timestamp (t:t) =
  let T pre_t = t in
  timestamp_wrap pre_t


(* TODO: [operations] and [pending]: forward request to [t] which internally
 * forwards it to [mempool_worker]. Need to expose some primitives in
 * [mempool_worker]. *)
let operations (t:t) =
  ignore t;
  Pervasives.failwith "TODO"
let pending ?block (t:t) =
  ignore block;
  ignore t;
  Pervasives.failwith "TODO"

(* Expose [mempool_worker] and [mempool_peer_worker]s? *)
let running_workers () =
  Registry.fold
    (fun (chain_id, proto) t acc -> (chain_id, proto, t) :: acc)
    []

(* Expose requests of [mempool_worker] and [mempool_peer_worker]s? *)
let pending_requests (t:t) =
  ignore t;
  Pervasives.failwith "TODO"

let current_request (t:t) =
  ignore t;
  Pervasives.failwith "TODO"

(* Expose events of [mempool_worker] and [mempool_peer_worker]s? *)
let last_events (t:t) =
  ignore t;
  Pervasives.failwith "TODO"

let protocol_hash (T pre_t : t) =
  let module M: T = (val pre_t.m) in
  snd M.identifier

let parameters (T pre_t : t) =
  let module M: T = (val pre_t.m) in
  M.parameters
