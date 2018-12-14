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

type chain = {
  id : Chain_id.t ;
  db : Distributed_db.chain_db ;
  state : State.Chain.t ;
}

let chain db =
  let state = Distributed_db.chain_state db in
  let id = State.Chain.id state in
  { id ; db ; state }

type head_info = {
  current_head : State.Block.t ;
  current_head_hash : Block_hash.t ;
  current_head_header : Block_header.t ;
  live_blocks: Block_hash.Set.t ;
  live_operations: Operation_hash.Set.t ;
}

let head_info_of_chain chain =
  Chain.data chain.state >>= fun { current_head ; live_operations ; live_blocks } ->
  let current_head_header = State.Block.header current_head in
  let current_head_hash = State.Block.hash current_head in
  Lwt.return
    { current_head ; current_head_hash ; current_head_header ;
      live_blocks ; live_operations }
