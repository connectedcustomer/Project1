(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Validating batches of operations with some peer-based
    compartimentatilsation. *)

(** [max_promises_per_request] is the number of high-level promises that can be
    unresolved at one time whilst processing one batch of operation by one peer.
    The high-level promises include "retreiving an operation from the
    distributed db," or "validating the operation on top of the currently
    maintained context." Note that each of these pending promises may create
    (and wait upon the resolving of) more promises. As such,
    [max_promises_per_request] is not a true limit of the concurrency of the
    process, but it nonetheless is helpful in throttling peer workers to ensure
    each has a chance to make progress.

    [worker_limits] limits the number of pending request and other such generic
    limitations on workers.
*)
type limits = {
  max_promises_per_request : int ;
  worker_limits : Worker_types.limits ;
}

(** A module [T] can be used to distribute work to different workers depending
    on which peer the work has been triggered by. Note that the module does not
    enforce that work is performed by the appropriate peer. It is up to the user
    of a [T] module to manage a set of peer worker and select the appropriate
    one to give work to. *)
module type T = sig

  (** Underlying modules. Validation of individual operations is delegated to
      [Operation_validator]. *)
  module Proto: Registered_protocol.T
  module Operation_validator: Operation_validator.T
    with module Proto = Proto

  (** The type of a peer worker. Each peer worker should be used for treating
      all the operations from a given peer. *)
  type t

  (** Types for calls into this module *)

  (** [input] are the batches of operations that are given to a peer worker to
      validate. These hashes are gossiped on the network, and the mempool checks
      their validity before gossiping them furhter. *)
  type input = Operation_hash.t list

  (** [create limits peer_id op_validator] is, unless an error occurs durring
      initialization, a peer worker meant to handle the work provided by peer
      [peer_id]. The underlying validation of individual operation is delegated
      to [op_validator]. *)
  val create: limits -> P2p_peer.Id.t -> Operation_validator.t -> t tzresult Lwt.t

  (** [shutdown worker] cancels ongoing work, clears internal state and returns
      operations that have not been validated yet. This lets the user of this
      module retry some of the validation work if appropriate. *)
  val shutdown: t -> input Lwt.t

  (** [validate worker op_hashes] makes [worker] validate [op_hashes]. The
      [worker] validates one batch of operations at a time. During the
      validation of one batch, at most [limits.max_promises_per_request]
      high-level promises are pending at any one time. *)
  val validate: t -> input -> unit tzresult Lwt.t

  (** [status worker] gives generic information about the worker state and its
      requests. *)
  val status: t -> Worker_types.worker_status

end


module type STATIC = sig
  val max_pending_requests : int
end

module Make
    (Static: STATIC)
    (Proto: Registered_protocol.T)
    (Operation_validator: Operation_validator.T with module Proto = Proto)
  : T
    with module Proto = Proto
     and module Operation_validator = Operation_validator
