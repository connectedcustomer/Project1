(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Development. <contact@tezcore.com>             *)
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

(** Type of a protocol-specific mempool filter plug-in. *)
module type FILTER = sig

  (** Type of protocol-specific mempool configuration, as specifiable
      in the node's configuration file, and updatable via RPCs. *)
  type config

  (** Formatting of {!config} for the configuration file and RPCs. *)
  val config_encoding : config Data_encoding.t

  (** Default configuration value, used when none is specified. *)
  val default_config : config

  (** The protocol this plug-in understands. *)
  module Proto : Registered_protocol.T

  (** Tells if an operation should be kept and propagated before even running it. *)
  val pre_filter : config -> Proto.operation_data -> bool

  (** Tells if an operation should be kept and propagated considering its result. *)
  val post_filter : config -> Proto.operation_data * Proto.operation_receipt -> bool
end

(** Registers a mempool plug-in for a specific protocol (according to its [Proto.hash]). *)
val register : (module FILTER) -> unit

(** Looks for a mempool plug-in for a specific protocol. *)

type ('operation_data, 'operation_receipt) filter =
  (module FILTER
    with type Proto.P.operation_data = 'operation_data
     and type Proto.P.operation_receipt = 'operation_receipt)
type ('operation_data, 'operation_receipt) proto =
  (module Registered_protocol.T
    with type P.operation_data = 'operation_data
     and type P.operation_receipt = 'operation_receipt)

val find :
  ('operation_data, 'operation_receipt) proto ->
  ('operation_data, 'operation_receipt) filter option
