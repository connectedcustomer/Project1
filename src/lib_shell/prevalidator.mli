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

(** Tezos Shell - Prevalidation of pending operations (a.k.a Mempool) *)

(** The prevalidator is in charge of the "mempool" (a.k.a. the
    set of known not-invalid-for-sure operations that are not yet
    included in the blockchain).

    The prevalidator also maintains a sorted subset of the mempool that
    might correspond to a valid block on top of the current head. The
    "in-progress" context produced by the application of those
    operations is called the (pre)validation context.

    Before including an operation into the mempool, the prevalidation
    worker tries to append the operation the prevalidation context. If
    the operation is (strongly) refused, it will not be added into the
    mempool and then it will be ignored by the node and never
    broadcast. If the operation is only "branch_refused" or
    "branch_delayed", the operation won't be appended in the
    prevalidation context, but still broadcast.

*)



(** An (abstract) prevalidator context. Separate prevalidator contexts should be
 * used for disjoint chains (e.g., mainchain vs testchain).
 * The prevalidator context maintains its own set of workers (one worker for
 * each peer it is connected to) and additional necessary state.
 * *)
type t

type limits = {
  max_refused_operations : int ;
  max_queued_operations : int ;
  operation_timeout : float ;
  worker_limits : Worker_types.limits ;
}

type error += Closed of Chain_id.t


(** {!2 Interface for the chain validator}: it is responsible for notifying of
 * advertised operations and heads. *)

(** Creates/tear-down a new prevalidator context. *)
val create:
  limits ->
  (module Prevalidator_filters.FILTER) ->
  Distributed_db.chain_db ->
  t tzresult Lwt.t
val shutdown: t -> unit Lwt.t

(** Notify the prevalidator that the identified peer has sent a bunch of
 * operations relevant to the specified context. *)
val notify_operations: t -> P2p_peer.Id.t -> Mempool.t -> unit

(** Notify the prevalidator that a new head has been selected.
 * NOTE: It is the chain_validator's responsibility to shutdown/restart the
 * prevalidator when a new protocol starts. *)
val flush: t -> Block_hash.t -> unit tzresult Lwt.t



(** {!2 Interface for the node to inject operations}. This actually happens
 * through multiple indirection/abstraction layers. *)

(** Notify the prevalidator of a new locally-injected operation. *)
(* TODO[?] How to return a receipt (it depends on the protocol's types)
 * OPTION: require additional Proto argument, the function checks hash equality
 * and coerce return value if safe. *)
val inject_operation: t -> Operation.t -> unit Lwt.t

val rpc_directory : t option RPC_directory.t
