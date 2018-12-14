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

(** A type to pack the multiple bits of information about a chain that parts of
    the mempool need. This aims to reduce
    1. the amount of decoding happening: a [chain] value is generated once (by
    the [chain] function below) and then shared amongst the different part of
    the mempool, and
    2. reduce the number of parameters/arguments for several functions of the
    mempool: a single argument that packs multiple values replaces multiple
    arguments. *)
type chain = {
  id : Chain_id.t ;
  db : Distributed_db.chain_db ;
  state : State.Chain.t ;
}

(** [chain chain_db] is a [chain] value (see type definition above) derived from
    the given [chain_db]. *)
val chain : Distributed_db.chain_db -> chain

(** A type to pack the multiple bits of information about a head of a chain that
    parts of the mempool need. It has the same intended use as [chain] but for
    the level of block. *)
type head_info = {
  current_head : State.Block.t ;
  current_head_hash : Block_hash.t ;
  current_head_header : Block_header.t ;
  live_blocks: Block_hash.Set.t ;
  live_operations: Operation_hash.Set.t ;
}

(** [head_info_of_chain chain] is a [head_info] value packing information about
    the head of [chain]. *)
val head_info_of_chain : chain -> head_info Lwt.t
