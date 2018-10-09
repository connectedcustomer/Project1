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

(* All the protocol-independent stuff is here. *)

module Log = Logging.Make(struct let name = "Prevalidator" end)


(* # MEMORY USAGE LIMITS *)

(* The type [limits] is used by the instantiator to communicate limits for the
 * workers and the worker pool.
 *
 * NOTE: Currently, some of these limits are ignored. *)
type limits = {
  max_refused_operations : int ;
  max_queued_operations : int ;
  operation_timeout : float ;
  worker_limits : Worker_types.limits ;
}


(* # LOGGING EVENTS
 * NOTE: we declare the logging events independent of the protocol so they can
 * be parsed/used outside of the protocol-specific context. This may change in
 * the future. A possible development is to provide protocol-specific events and
 * a protocol-indenpendent encoding that reads the values and replaces the
 * protocol-specific parts by placeholders. *)

type event =
  | Hash of Operation_hash.t
  | Drop of Operation_hash.t * error list
  | Download_start of Operation_hash.t
  | Download_end of Operation_hash.t
  | Apply_start of Operation_hash.t
  | Apply_end of Operation_hash.t
  (* TODO[SCORE]: with the score, we should log
     | Penalty
     | Reward
  *)
  | Op_done of Operation_hash.t
  | Batch_done (* TODO[LOG,RPC]: success statistic (total-hash, accepted-hash, etc.) *)
  | Debug of string

(* Wrapping the events with some boilerplate code to provide the required
 * [Worker.EVENT] interface. *)
module Event = struct

  type t = event

  let level = function
    | Hash _ -> Logging.Debug
    | Drop _ -> Logging.Info
    | Download_start _ -> Logging.Debug
    | Download_end _ -> Logging.Debug
    | Apply_start _ -> Logging.Debug
    | Apply_end _ -> Logging.Debug
    | Batch_done -> Logging.Debug
    | Op_done _ -> Logging.Debug
    | Debug _ -> Logging.Debug

  let encoding =
    let open Data_encoding in
    union
      [ case (Tag 0)
          ~title:"Hash"
          (obj2
             (req "event" (constant "hash"))
             (req "hash" Operation_hash.encoding))
          (function Hash hash -> Some ((), hash) | _ -> None)
          (fun ((), hash) -> Hash hash) ;
        case (Tag 1)
          ~title:"Drop"
          (obj3
             (req "event" (constant "drop"))
             (req "hash" Operation_hash.encoding)
             (req "hash" (list Error_monad.error_encoding)))
          (function Drop (hash, errs) -> Some ((), hash, errs) | _ -> None)
          (fun ((), hash, errs) -> Drop (hash, errs)) ;
        case (Tag 2)
          ~title:"DownloadStart"
          (obj2
             (req "event" (constant "downloadstart"))
             (req "hash" Operation_hash.encoding))
          (function Download_start hash -> Some ((), hash) | _ -> None)
          (fun ((), hash) -> Download_start hash) ;
        case (Tag 3)
          ~title:"DownloadEnd"
          (obj2
             (req "event" (constant "downloadend"))
             (req "hash" Operation_hash.encoding))
          (function Download_end hash -> Some ((), hash) | _ -> None)
          (fun ((), hash) -> Download_end hash) ;
        case (Tag 4)
          ~title:"Op_done"
          (obj2
             (req "event" (constant "opdone"))
             (req "hash" Operation_hash.encoding))
          (function Op_done hash -> Some ((), hash) | _ -> None)
          (fun ((), hash) -> Op_done hash) ;
        case (Tag 5)
          ~title:"Batch_done"
          (obj1
             (req "event" (constant "batchdone")))
          (function Batch_done -> Some () | _ -> None)
          (fun () -> Batch_done) ;
        case (Tag 6)
          ~title:"Debug"
          (obj2
             (req "event" (constant "debug"))
             (req "payload" string))
          (function Debug s -> Some ((), s) | _ -> None)
          (fun ((), s) -> Debug s) ;
      ]

  let pp_op ppf op = Format.fprintf ppf "operation hash %a" Operation_hash.pp op
  let pp ppf = function
    | Hash hash -> Format.fprintf ppf "Received %a" pp_op hash
    | Drop (hash, errs) ->
        Format.fprintf ppf "Abandonning %a because %a"
          pp_op hash
          Error_monad.pp_print_error errs
    | Download_start hash -> Format.fprintf ppf "Download started for %a" pp_op hash
    | Download_end hash -> Format.fprintf ppf "Download completed for %a" pp_op hash
    | Apply_start hash -> Format.fprintf ppf "Application started for %a" pp_op hash
    | Apply_end hash -> Format.fprintf ppf "Application completed for %a" pp_op hash
    | Batch_done -> Format.fprintf ppf "Processed operation batch"
    | Op_done hash -> Format.fprintf ppf "Porcessed %a" pp_op hash
    | Debug s -> Format.fprintf ppf "Debug: %s" s

end


(* # VIEWS
 * NOTE: We define the view types (required by [Worker.REQUEST] and
 * [Worker.TYPES]) outside of the protocol-dependent context. This is to allow
 * protocol-independent introspection. *)

(* The type to inspect requests from the outside. Each request carries one
 * mempool. The Worker framework takes care of the timestamp and peer_id. *)
type req_view = View of Mempool.t
let req_view_encoding =
  let open Data_encoding in
  union
    [ case (Tag 0)
        ~title:"Mempool"
        Mempool.encoding
        (function View mempool -> Some mempool)
        (fun mempool -> View mempool) ;
    ]
type state_view = unit (* TODO[RPC]: interesting parts for introspection *)
let state_view_encoding =
  let open Data_encoding in
  null


type error += Initialization_error
exception Uninitialized of error list


(* # FUNCTOR ARGUMENT
 * The protocol, the filter that fits this protocol, memory use limits and some
 * metadata for logging and such.
 *
 * These are used to instantiate a worker pool. It is only used by
 * [prevalidator] (when necessary to satisfy the instantiator). *)

module type PROTO_AND_SUCH = sig
  module Proto: Registered_protocol.T
  module Filter: Prevalidator_filters.FILTER with module Proto = Proto
  val limits: limits
  val chain_state: State.Chain.t (* to find the head block *)
  val chain_db: Distributed_db.chain_db (* to fetch ops in DistributedDB*)
end


(* # FUNCTOR RESULT *)

module type PREVALIDATOR_CONTEXT = sig

  (* It is the RESPONSIBILITY of [prevalidator] to monitor this thread when
   * applying the functor. If it resolves as [Ok ()] then it is safe to
   * continue. If it resolves as [Error _] then it is unsafe and the result of
   * functor application should be ignored. *)
  val init_errors: unit tzresult Lwt.t (* To communicate error in initialisation *)

  (* Inner modules that carry types that appear below *)
  module Proto: Registered_protocol.T
  module Filter: Prevalidator_filters.FILTER with module Proto = Proto
  module Preval: Prevalidation.T with module Proto = Proto

  (* The ops handled by the mempool (as a batch, or as a stream) *)
  val get_mempool: unit -> Mempool.t tzresult Lwt.t
  val preapplied_operations:
    unit ->
    (Operation.shell_header * Proto.operation_data) Lwt_stream.t

  (* the filter configuration *)
  val filter_config: Filter.config ref


  (* Types to communicate with the workers *)

  (* [inclusion_result] is the output of the workers' work: they return a report
   * indicating what happened to each operation. *)
  type inclusion_result =
    | Cannot_download of error list
    | Cannot_parse of error list
    | Refused_by_prefilter
    | Refused_by_postfilter
    | Prevalidation_result of Preval.result

  (* [request] is how work is given to a worker *)
  type _ request =
    | Req_mempool: Mempool.t -> (Operation_hash.t * inclusion_result) list request

  (**)
  type state
  type parameters = {
    initial_op_hashes: Operation_hash.t list;
  }


  (* an instantiated worker pool *)
  module Name: Worker.NAME with type t = P2p_peer.Id.t
  module Request: Worker.REQUEST with type 'a t = 'a request
  module Types: Worker.TYPES
    with type state = state
     and type parameters = parameters
     and type view = state_view
  module type WORKERS =
    Worker.WORKER
    with type name_t := Name.t
     and type event_t := Event.t
     and type 'a request_t := 'a request
     and type request_view := req_view
     and type types_state := state
     and type types_parameters := parameters
     and type types_view := Types.view
  module Worker_pool: WORKERS

  (* a registry of worker associated to each known peer *)
  type worker = Worker_pool.bounded Worker_pool.queue Worker_pool.t
  module Peer_level_registry:
    Registry.S
    with type k = P2p_peer.Id.t
     and type v = worker

  val launch_and_register_worker: P2p_peer.Id.t -> worker Lwt.t

  (* For injections only. When dealing with mempools sent by a peer, use requests *)
  val prevalidate_injection: Operation.t -> inclusion_result Lwt.t

  (* indicate that a new head (with the same protocol) is available *)
  val notify_head: Block_hash.t -> unit tzresult Lwt.t

  val rpc_directory: unit RPC_directory.t tzresult Lwt.t

end

