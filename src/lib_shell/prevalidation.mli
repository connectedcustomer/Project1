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

(** A newly received block is validated by replaying locally the block
    creation, applying each operation and its finalization to ensure their
    consistency. This module is stateless and creates and manupulates the
    prevalidation_state. *)

module type T = sig

  module Proto : Registered_protocol.T
  (* The protocol that this `Prevalidation.T` uses. The protocol provides
   * primitives for parsing operation, checking the result of operations, etc.
   * A functor (below) can create a `T` from a protocol. *)

  type t
  (* A stateless (functional) representation of a prevalidation context. *)

  type operation = private {
    hash: Operation_hash.t ;
    raw: Operation.t ;
    protocol_data: Proto.operation_data ;
  }
  (* Decorated operation with protocol-specific data (to avoid having to parse
   * mutliple times) *)

  val parse: Operation.t -> operation tzresult
  (* Wraps the Protocol-specific parsing to return a result in a nicer form *)

  val compare: operation -> operation -> int
  (* Total order *)

  val parse_list:
    Operation.t list ->
    operation list * (Operation.t * error list) list
  (* parse all operations in the argument and sort thems into two lists based on
   * success/failure. *)

  (** Creates a new prevalidation context w.r.t. the protocol associate to the
      predecessor block . When ?protocol_data is passed to this function, it will
      be used to create the new block *)
  val start:
    ?protocol_data: MBytes.t ->
    predecessor: State.Block.t ->
    timestamp: Time.t ->
    unit -> t tzresult Lwt.t

  type result =
    | Applied of t * operation * Proto.operation_receipt
    (** the operation is applied succesfully *)
    | Branch_delayed of operation * error list
    (** the operation cannot be applied in this branch now, but it might be applied
        in the future. Since this is a temporary error, we keep the raw operation.
        Ex. endorsement for a unknown block, or operation without funds *)
    | Branch_refused of error list
    (** the operation cannot be applied in this branch, but it might be applied in
        a different branch *)
    | Refused of error list
    (** the operation is invalid. Ex. Parse error *)
    | Duplicate
    (** the operation was already included *)
    | Outdated
    (** the operation cannot be applied because too old *)

  val apply_operation: t -> operation -> result Lwt.t
  (* Applies an operation on top of a prevalidation context. A new prevalidation
   * context is returned inlined with the resulting variant when appropriate. *)

  val apply_operation_list:
    error Preapply_result.t -> t -> Operation.t list ->
    (error Preapply_result.t * t) Lwt.t
  (* Folds over the list of operations to apply all of the operations in the
   * list *)

  val finalize:
    t ->
    ( Tezos_protocol_environment_shell.validation_result
      * (operation * Proto.operation_receipt) list
      * Proto.block_header_metadata ) tzresult Lwt.t

end

module Make(Proto : Registered_protocol.T) : T with module Proto = Proto

(** Pre-apply creates a new block and returns it. *)
val preapply :
  predecessor:State.Block.t ->
  timestamp:Time.t ->
  protocol_data:MBytes.t ->
  (module T) ->
  Operation.t list list ->
  ( Block_header.shell_header
    (* FIXME here the type should be Tezos_protocol_environment_shell.validation_result *)
    * error Preapply_result.t list) tzresult Lwt.t
