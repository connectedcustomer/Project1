(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Log = Tezos_stdlib.Logging.Make(struct
    let name = "node.mempool.validator"
  end)

type limits = {
  worker_limits: Mempool_worker.limits ;
  worker_max_size_parsed_cache: int ;
  peer_worker_limits: Mempool_peer_worker.limits ;
  peer_worker_max_pending_requests: int ;
}

(* This is equivalent to `Mempool_peer_worker.input P2p_peer.Id.Map.t` but
 * independent of any instantiation of `Mempool_peer_worker`. *)
type recycling = Operation_hash.t list P2p_peer.Id.Map.t


(** Types for the mempool_validator instantiated values: first-class module that
    wraps in the Proto and derived modules and holds the needed instantiated
    workers. *)

module type T = sig

  (* instantiated worker management modules *)
  module Proto: Registered_protocol.T
  module Mempool_worker: Mempool_worker.T
    with module Proto = Proto
  module Mempool_peer_worker: Mempool_peer_worker.T
    with module Proto = Proto
     and module Mempool_worker = Mempool_worker

  (* worker values and registries *)
  val get_mempool_worker: unit -> Mempool_worker.t
  val get_mempool_peer_worker: P2p_peer.Id.t -> Mempool_peer_worker.t tzresult Lwt.t
  val clear_mempool_peer_workers: unit -> unit
  val map_mempool_peer_workers:
    (Mempool_peer_worker.t -> 'a) -> 'a P2p_peer.Id.Map.t

  val recycle : recycling -> unit Lwt.t
  val shutdown : unit -> recycling Lwt.t
  val new_head : 'a -> unit tzresult Lwt.t

  (* boilerplate configuration variables *)
  val limits: limits
  val chain_db: Distributed_db.chain_db

end

type t = (module T)



(** To create T modules (which become t values) we use a functor that takes PRE.
    This functor is then wrapped in a function for initialization purpose. The
    whole thing goes like this

    1. `create`
    1.1. instantiates the `Mempool_worker` module
    1.2. initializes a `mempool_worker`
    1.3. puts it all in a PRE module
    1.4. calls the `Create` functor
    2. `Create`
    2.1 instantiates the `Mempool_peer_worker` module
    2.2 initializes basic values
    3. `create`
    3.1 wraps the module returned by `Create` as a first-class value

    This dance is necessary because 1.2 can fail slowly (i.e., carries the type
    `tzresult Lwt.t`). Thus, it cannot be part of the `Create` functor and must
    be part of the `create` function.
*)
module type PRE = sig
  module Proto: Registered_protocol.T
  module Mempool_worker: Mempool_worker.T
    with module Proto = Proto
  val mempool_worker: Mempool_worker.t
  val limits: limits
  val chain_db: Distributed_db.chain_db
end

module Create (Pre : PRE) : T = struct

  include Pre

  module Mempool_peer_worker =
    Mempool_peer_worker.Make
      (struct let max_pending_requests = limits.peer_worker_max_pending_requests end)
      (Proto)
      (Mempool_worker)

  let mempool_worker_ref = ref mempool_worker
  let get_mempool_worker () = !mempool_worker_ref

  (* Typical number of connected peers: 10-100 *)
  let mempool_peer_workers = P2p_peer.Id.Table.create 64

  let get_mempool_peer_worker peer_id =
    match P2p_peer.Id.Table.find_opt mempool_peer_workers peer_id with
    | Some mpw -> return mpw
    | None ->
        assert (not (P2p_peer.Id.Table.mem mempool_peer_workers peer_id)) ;
        Mempool_peer_worker.create
          limits.peer_worker_limits
          peer_id
          !mempool_worker_ref >>=? fun mpw ->
        P2p_peer.Id.Table.add mempool_peer_workers peer_id mpw ;
        return mpw
  let clear_mempool_peer_workers () =
    P2p_peer.Id.Table.clear mempool_peer_workers
  let map_mempool_peer_workers f =
    P2p_peer.Id.Table.fold
      (fun peer_id mpw acc ->
         assert (not (P2p_peer.Id.Map.mem peer_id acc)) ;
         P2p_peer.Id.Map.add peer_id (f mpw) acc)
      mempool_peer_workers
      P2p_peer.Id.Map.empty

  let shutdown () =
    begin
      (* collect recycling, first as a list of promises, then as a promise of map *)
      P2p_peer.Id.Table.fold
        (fun peer_id mpw acc ->
           (Mempool_peer_worker.shutdown mpw >>= fun r ->
            Lwt.return (peer_id, r))
           :: acc)
        mempool_peer_workers
        [] |>
      Lwt_list.fold_left_s
        (fun acc p ->
           p >>= fun (peer_id, r) ->
           assert (not (P2p_peer.Id.Map.mem peer_id acc)) ;
           Lwt.return (P2p_peer.Id.Map.add peer_id r acc))
        P2p_peer.Id.Map.empty
    end >>= fun recycling ->
    clear_mempool_peer_workers ();
    Mempool_worker.shutdown !mempool_worker_ref >>= fun () ->
    Lwt.return recycling

  let recycle recycling =
    Lwt.join begin
      P2p_peer.Id.Map.fold
        (fun peer_id input acc ->
           (begin
             get_mempool_peer_worker peer_id >>=? fun mpw ->
             Mempool_peer_worker.validate mpw input
           end >>= fun _ -> Lwt.return_unit) :: acc)
        recycling
        []
    end

  let new_head _head =
    shutdown () >>= fun recycling ->
    Mempool_worker.create limits.worker_limits chain_db >>=? fun mw ->
    mempool_worker_ref := mw;
    recycle recycling >>= fun () ->
    return_unit

end

(* The "first-class module to first-class module" `create` function wraps around
   the `Create` functor. As part of the process, it instantiates a
   `mempool_worker`. This instantiation (1) can fail and (2) is within Lwt. For
   these reasons, a function (rather than a functor) is necessary. *)
let create limits (module Proto: Registered_protocol.T) chain_db =

  let module Mempool_worker =
    Mempool_worker.Make
      (struct let max_size_parsed_cache = limits.worker_max_size_parsed_cache end)
      (Proto) in
  Mempool_worker.create limits.worker_limits chain_db >>=? fun mempool_worker ->

  let module Pre : PRE = struct
    module Proto = Proto
    module Mempool_worker = Mempool_worker
    let mempool_worker = mempool_worker
    let limits = limits
    let chain_db = chain_db
  end in
  let module M : T = Create(Pre) in

  return (module M : T)


let shutdown (module M : T) = M.shutdown ()

(* NOTE: we must ignore errors because recycling is an optimistic otpimization
 * that pretend operations don't change between protocols (which is presumably
 * true in many cases, but also definitely not true in all cases) *)
let recycle (module M : T) recycling = M.recycle recycling

let validate (module M : T) peer_id mempool =
  M.get_mempool_peer_worker peer_id >>= function
  | Error errs ->
      Log.lwt_warn
        "Error initializing mempool peer worker for peer %a: %a"
        P2p_peer.Id.pp_short peer_id
        Error_monad.pp_print_error errs
  | Ok mpw ->
      let input =
        mempool.Mempool.known_valid @
        Operation_hash.Set.elements mempool.Mempool.pending in
      M.Mempool_peer_worker.validate mpw input >>= function
      | Error errs ->
          Log.lwt_warn
            "Error validating mempool from peer %a: %a"
            P2p_peer.Id.pp_short peer_id
            Error_monad.pp_print_error errs
      | Ok () -> Lwt.return_unit


let inject (module M : T) operation =
  match M.Mempool_worker.parse operation with
  | Error errs ->
      Log.lwt_log_notice "Error whilst parsing injected operation: %a"
        Error_monad.pp_print_error errs >>= fun () ->
      Lwt.return (Error errs)
  | Ok op ->
      M.Mempool_worker.validate (M.get_mempool_worker ()) op >>= function
      | Error errs ->
          Log.lwt_log_notice "Error whilst validating injected operation: %a"
            Error_monad.pp_print_error errs >>= fun () ->
          Lwt.return (Error errs)
      | Ok _ ->
          return_unit


let new_head (module M : T) _head =
  M.new_head _head

let fitness (module M : T) = M.Mempool_worker.fitness (M.get_mempool_worker ())
let protocol_hash (module M : T) = M.Proto.hash
let limits (module M : T) = M.limits
let chain_db (module M : T) = M.chain_db


let status (module M : T) =
  let mws = M.Mempool_worker.status (M.get_mempool_worker ())in
  let mpwss = M.map_mempool_peer_workers M.Mempool_peer_worker.status in
  (mws, mpwss)
