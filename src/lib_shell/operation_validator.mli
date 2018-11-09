(*****************************************************************************)
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

(* An interface close-enough to prevalidator.mli *)

type t

type recycling

type limits = {
  mempool_worker_limits : Mempool_worker.limits ;
  mempool_peer_worker_limits : Mempool_peer_worker.limits ;
}

val create:
  ?recycling:recycling ->
  limits ->
  (module Registered_protocol.T) ->
  Distributed_db.chain_db ->
  t tzresult Lwt.t
val shutdown: t -> recycling Lwt.t

val notify_operations: t -> P2p_peer.Id.t -> Mempool.t -> unit tzresult Lwt.t

val inject_operation: t -> Operation.t -> unit tzresult Lwt.t

val flush: t -> Block_hash.t -> unit tzresult Lwt.t

val timestamp: t -> Time.t

val protocol_hash: t -> Protocol_hash.t
val parameters: t -> limits * Distributed_db.chain_db


val operations: t -> error Preapply_result.t * Operation.t Operation_hash.Map.t
val pending: ?block:State.Block.t -> t -> Operation.t Operation_hash.Map.t Lwt.t
val running_workers: unit -> (Chain_id.t * Protocol_hash.t * t) list
val pending_requests : t -> (Time.t * Prevalidator_worker_state.Request.view) list
val current_request : t -> (Time.t * Time.t * Prevalidator_worker_state.Request.view) option
val last_events : t -> (Lwt_log_core.level * Prevalidator_worker_state.Event.t list) list
