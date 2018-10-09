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

module Log: Logging.LOG

type limits = {
  max_refused_operations : int ;
  max_queued_operations : int ;
  operation_timeout : float ;
  worker_limits : Worker_types.limits ;
}

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

module Event: Worker.EVENT with type t = event


type req_view = View of Mempool.t
val req_view_encoding: req_view Data_encoding.t

type state_view = unit
val state_view_encoding: state_view Data_encoding.t

type error += Initialization_error
exception Uninitialized of error list


module type PROTO_AND_SUCH = sig
  module Proto: Registered_protocol.T
  module Filter: Prevalidator_filters.FILTER with module Proto = Proto
  val limits: limits
  val chain_state: State.Chain.t (* to find the head block *)
  val chain_db: Distributed_db.chain_db (* to fetch ops in DistributedDB*)
end

module type PREVALIDATOR_CONTEXT = sig
  val init_errors: unit tzresult Lwt.t (* To communicate error in initialisation *)
  module Proto: Registered_protocol.T
  module Filter: Prevalidator_filters.FILTER with module Proto = Proto
  module Preval: Prevalidation.T with module Proto = Proto
  val get_mempool: unit -> Mempool.t tzresult Lwt.t
  val preapplied_operations:
    unit ->
    (Operation.shell_header * Proto.operation_data) Lwt_stream.t
  val filter_config: Filter.config ref
  type inclusion_result =
    | Cannot_download of error list
    | Cannot_parse of error list
    | Refused_by_prefilter
    | Refused_by_postfilter
    | Prevalidation_result of Preval.result
  type _ request =
    | Req_mempool: Mempool.t -> (Operation_hash.t * inclusion_result) list request
  type state
  type parameters = {
    initial_op_hashes: Operation_hash.t list;
  }
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
  type worker = Worker_pool.bounded Worker_pool.queue Worker_pool.t
  module Peer_level_registry:
    Registry.S
    with type k = P2p_peer.Id.t
     and type v = worker
  val launch_and_register_worker: P2p_peer.Id.t -> worker Lwt.t
  (* For injections only. When dealing with mempools sent by a peer, use requests *)
  val prevalidate_injection: Operation.t -> inclusion_result Lwt.t
  val notify_head: Block_hash.t -> unit tzresult Lwt.t
  val rpc_directory: unit RPC_directory.t tzresult Lwt.t
end

