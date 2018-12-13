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
  operation_validator_limits: Operation_validator.limits ;
  operation_validator_max_size_parsed_cache: int ;
  peer_worker_limits: Mempool_peer_worker.limits ;
  peer_worker_max_pending_requests: int ;
  advertiser_limits: Mempool_advertiser.limits ;
}

(* This is equivalent to `Mempool_peer_worker.input P2p_peer.Id.Map.t` but
 * independent of any instantiation of `Mempool_peer_worker`. *)
type recycling = {
  operations: Operation_hash.t list P2p_peer.Id.Map.t ;
  filter_config: Data_encoding.json ;
}


(** Types for the mempool_validator instantiated values: first-class module that
    wraps in the Proto and derived modules and holds the needed instantiated
    workers. *)

module type T = sig

  (* instantiated worker management modules *)
  module Proto: Registered_protocol.T
  module Mempool_advertiser: Mempool_advertiser.T
  module Operation_validator: Operation_validator.T
    with module Proto = Proto
  module Mempool_peer_worker: Mempool_peer_worker.T
    with module Proto = Proto
     and module Operation_validator = Operation_validator

  (* worker values and registries *)
  val map_mempool_advertiser: (Mempool_advertiser.t -> 'a) -> 'a

  val get_mempool_worker: unit -> Operation_validator.t
  val map_mempool_worker: (Operation_validator.t -> 'a) -> 'a

  val get_mempool_peer_worker: P2p_peer.Id.t -> Mempool_peer_worker.t tzresult Lwt.t
  val clear_mempool_peer_workers: unit -> unit
  val map_mempool_peer_workers:
    (Mempool_peer_worker.t -> 'a) -> 'a P2p_peer.Id.Map.t

  (* Basic operations *)
  val recycle : recycling -> unit Lwt.t
  val shutdown : unit -> recycling Lwt.t
  val new_head : Block_hash.t -> unit tzresult Lwt.t
  val advertise: unit -> unit Lwt.t

  (* boilerplate configuration variables *)
  val limits: limits
  val chain: Mempool_helpers.chain

  val rpc_directory: unit RPC_directory.t

end

type t = (module T)


type Error_monad.error += Closed of unit
let () =
  register_error_kind `Permanent
    ~id:("mempool_validator.closed")
    ~title:("Mempool_validator closed")
    ~description:
      ("The mempool validator has been closed. Operations on it now cause errors.")
    ~pp:(fun ppf () -> Format.fprintf ppf "Mempool_validator has been shutdown.")
    Data_encoding.unit
    (function Closed () -> Some () | _ -> None)
    (fun () -> Closed ())



(** To create T modules (which become t values) we use a functor that takes PRE.
    This functor is then wrapped in a function for initialization purpose. The
    whole thing goes like this

    1. `create`
    1.1. instantiates the `Operation_validator` module
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
  module Mempool_advertiser: Mempool_advertiser.T
  module Mempool_filters: Mempool_filters.T
    with module Proto = Proto
  module Operation_validator: Operation_validator.T
    with module Proto = Proto
     and module Mempool_filters = Mempool_filters
     and module Mempool_advertiser = Mempool_advertiser
  val mempool_advertiser: Mempool_advertiser.t
  val mempool_worker: Operation_validator.t
  val limits: limits
  val chain: Mempool_helpers.chain
end

module Create (Pre : PRE) : T = struct

  include Pre

  module Mempool_peer_worker =
    Mempool_peer_worker.Make
      (struct let max_pending_requests = limits.peer_worker_max_pending_requests end)
      (Proto)
      (Operation_validator)

  let mempool_advertiser_ref = ref mempool_advertiser
  let map_mempool_advertiser f = f !mempool_advertiser_ref

  let mempool_worker_ref = ref mempool_worker
  let get_mempool_worker () = !mempool_worker_ref
  let map_mempool_worker f = f !mempool_worker_ref

  let filter_config = ref Mempool_filters.default_config

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
    end >>= fun operations ->
    clear_mempool_peer_workers ();
    let filter_config = Operation_validator.filter_config !mempool_worker_ref in
    let filter_config =
      Data_encoding.Json.construct
        Mempool_filters.config_encoding filter_config in
    Operation_validator.shutdown !mempool_worker_ref >>= fun () ->
    Mempool_advertiser.shutdown !mempool_advertiser_ref >>= fun () ->
    Lwt.return { operations ; filter_config }

  let recycle { operations ; filter_config } =
    begin
      try
        let filter_config =
          Data_encoding.Json.destruct Mempool_filters.config_encoding filter_config in
        Operation_validator.update_filter_config !mempool_worker_ref filter_config
      with
      | _ -> () (* TODO: only catch JSON decoding errors *)
    end ;
    Lwt.join begin
      P2p_peer.Id.Map.fold
        (fun peer_id input acc ->
           (begin
             get_mempool_peer_worker peer_id >>=? fun mpw ->
             Mempool_peer_worker.validate mpw input
           end >>= fun _ -> Lwt.return_unit) :: acc)
        operations
        []
    end

  let new_head _head =
    let filter_config = Operation_validator.filter_config !mempool_worker_ref in
    Mempool_helpers.head_info_of_chain chain >>= fun head_info ->
    shutdown () >>= fun recycling ->
    Mempool_advertiser.create limits.advertiser_limits chain head_info >>=? fun ma ->
    mempool_advertiser_ref := ma ;
    Operation_validator.create
      limits.operation_validator_limits
      chain head_info
      filter_config
      ma >>=? fun mw ->
    mempool_worker_ref := mw;
    recycle recycling >>= fun () ->
    return_unit

  module Proto_services = Block_services.Make(Proto)(Proto)

  let rpc_directory =
    RPC_directory.empty |> fun dir ->
    RPC_directory.register
      dir
      (Proto_services.S.Mempool.get_filter RPC_path.open_root)
      (fun () () () ->
         let obj =
           Data_encoding.Json.construct
             Mempool_filters.config_encoding !filter_config in
         return obj) |> fun dir ->
    RPC_directory.register
      dir
      (Proto_services.S.Mempool.set_filter RPC_path.open_root)
      (fun () () obj ->
         let config = Data_encoding.Json.destruct Mempool_filters.config_encoding obj in
         filter_config := config ;
         return_unit)

  let advertise () =
    Mempool_advertiser.advertise !mempool_advertiser_ref

end

(* The "first-class module to first-class module" `create` function wraps around
   the `Create` functor. As part of the process, it instantiates a
   `mempool_worker`. This instantiation (1) can fail and (2) is within Lwt. For
   these reasons, a function (rather than a functor) is necessary. *)
let create
    limits
    (module Mempool_filters: Mempool_filters.T)
    chain_db =

  let module Proto = Mempool_filters.Proto in

  let chain = Mempool_helpers.chain chain_db in
  Mempool_helpers.head_info_of_chain chain >>= fun head_info ->

  let module Mempool_advertiser = Mempool_advertiser.Make(Proto) in
  Mempool_advertiser.create
    limits.advertiser_limits chain head_info >>=? fun mempool_advertiser ->

  let module Operation_validator =
    Operation_validator.Make
      (struct
        let max_size_parsed_cache =
          limits.operation_validator_max_size_parsed_cache
      end)
      (Proto)
      (Mempool_filters)
      (Mempool_advertiser) in
  let filter_config =
    (* NOTE: recycling overrules this, but we should still load a custom default *)
    Mempool_filters.default_config in
  Operation_validator.create
    limits.operation_validator_limits
    chain
    head_info
    filter_config
    mempool_advertiser >>=? fun mempool_worker ->

  let module Pre : PRE = struct
    module Proto = Proto
    module Mempool_advertiser = Mempool_advertiser
    module Operation_validator = Operation_validator
    module Mempool_filters = Mempool_filters
    let mempool_advertiser = mempool_advertiser
    let mempool_worker = mempool_worker
    let limits = limits
    let chain = chain
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
      | Ok () ->
          M.advertise ()


let inject (module M : T) operation =
  match M.Operation_validator.parse operation with
  | Error errs ->
      Log.lwt_log_notice "Error whilst parsing injected operation: %a"
        Error_monad.pp_print_error errs >>= fun () ->
      Lwt.return (Error errs)
  | Ok op ->
      M.map_mempool_worker (fun mempool_worker ->
          M.Operation_validator.validate mempool_worker op >>= function
          | Error errs ->
              Log.lwt_log_notice "Error whilst validating injected operation: %a"
                Error_monad.pp_print_error errs >>= fun () ->
              Lwt.return (Error errs)
          | Ok result ->
              match result with
              | Branch_delayed _
              | Branch_refused _
              | Refused _
              | Refused_by_pre_filter
              | Refused_by_post_filter
              | Duplicate
              | Not_in_branch ->
                  return_unit
              | Applied _ ->
                  M.advertise () >>= fun () ->
                  return_unit)


let new_head (module M : T) head =
  M.new_head head

let fitness (module M : T) = M.map_mempool_worker M.Operation_validator.fitness
let protocol_hash (module M : T) = M.Proto.hash
let limits (module M : T) = M.limits
let chain (module M : T) = M.chain


type status = {
  advertiser : Worker_types.worker_status ;
  worker : Worker_types.worker_status ;
  peer_workers : Worker_types.worker_status P2p_peer.Id.Map.t ;
}

let status (module M : T) = {
  advertiser = M.map_mempool_advertiser M.Mempool_advertiser.status ;
  worker = M.map_mempool_worker M.Operation_validator.status ;
  peer_workers = M.map_mempool_peer_workers M.Mempool_peer_worker.status ;
}
(* TODO: introspection RPC for status *)

let empty_rpc_directory : unit RPC_directory.t =
  RPC_directory.register
    RPC_directory.empty
    (Block_services.Empty.S.Mempool.pending_operations RPC_path.open_root)
    (fun _pv () () ->
       return {
         Block_services.Empty.Mempool.applied = [] ;
         refused = Operation_hash.Map.empty ;
         branch_refused = Operation_hash.Map.empty ;
         branch_delayed = Operation_hash.Map.empty ;
         unprocessed = Operation_hash.Map.empty ;
       })

let rpc_directory : t option RPC_directory.t =
  RPC_directory.register_dynamic_directory
    RPC_directory.empty
    (Block_services.mempool_path RPC_path.open_root)
    (function
      | None ->
          Lwt.return (RPC_directory.map (fun _ -> Lwt.return_unit) empty_rpc_directory)
      | Some (module M : T) ->
          Lwt.return
            (RPC_directory.merge
               (RPC_directory.map (fun _ -> Lwt.return_unit) M.rpc_directory)
               (RPC_directory.map
                  (fun _ -> Lwt.return (M.get_mempool_worker ()))
                  M.Operation_validator.rpc_directory)))
