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

module type FILTER = sig
  type config
  val config_encoding : config Data_encoding.t
  val default_config : config
  module Proto : Registered_protocol.T
  val pre_filter : config -> Proto.operation_data -> bool
  val post_filter : config -> Proto.operation_data * Proto.operation_receipt -> bool
end

type ('operation_data, 'operation_receipt) filter =
  (module FILTER
    with type Proto.P.operation_data = 'operation_data
     and type Proto.P.operation_receipt = 'operation_receipt)
type ('operation_data, 'operation_receipt) proto =
  (module Registered_protocol.T
    with type P.operation_data = 'operation_data
     and type P.operation_receipt = 'operation_receipt)


let table: (Protocol_hash.t, (module FILTER)) Hashtbl.t = Hashtbl.create 8

let register (f: (module FILTER)) =
  let module F = (val f) in
  Hashtbl.add table (F.Proto.hash) f

let find (type op_data) (type op_receipt) (p: (op_data, op_receipt) proto)
  : (op_data, op_receipt) filter option
  =
  let module P = (val p) in
  let f = Hashtbl.find_opt table P.hash in
  match f with
  | None -> None
  | Some (f: (module FILTER)) ->
      let module F = (val f) in
      assert (P.hash = F.Proto.hash);
      Obj.magic f
