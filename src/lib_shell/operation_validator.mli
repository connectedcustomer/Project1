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

type limits = {
  worker_limits : Worker_types.limits ;
}

module type T = sig

  module Proto: Registered_protocol.T
  module Mempool_filters: Mempool_filters.T with module Proto = Proto
  module Mempool_advertiser: Mempool_advertiser.T

  module Proto_services : (module type of Block_services.Make(Proto)(Proto))
  module Worker : Worker.T

  type t

  type operation = private {
    hash: Operation_hash.t ;
    raw: Operation.t ;
    protocol_data: Proto.operation_data ;
  }

  type result =
    | Applied of Proto.operation_receipt
    | Branch_delayed of error list
    | Branch_refused of error list
    | Refused of error list
    | Refused_by_pre_filter
    | Refused_by_post_filter
    | Duplicate
    | Not_in_branch
  val result_encoding : result Data_encoding.t

  (** Creates/tear-down a new mempool validator context. *)
  val create :
    limits ->
    Mempool_helpers.chain ->
    Mempool_helpers.head_info ->
    Mempool_filters.config ->
    Mempool_advertiser.t ->
    t tzresult Lwt.t
  val shutdown : t -> Operation_hash.t list Lwt.t

  (** parse a new operation *)
  val parse : Operation.t -> operation tzresult

  (** validate a new operation and add it to the mempool context *)
  val validate : t -> operation -> result tzresult Lwt.t

  val chain : t -> Mempool_helpers.chain

  val update_filter_config : t -> Mempool_filters.config -> unit
  val filter_config : t -> Mempool_filters.config

  val fitness : t -> Fitness.t tzresult Lwt.t

  val status : t -> Worker_types.worker_status

  val head : t -> State.Block.t

  val rpc_directory : t RPC_directory.t

end

module type STATIC = sig
  val max_size_parsed_cache: int
end

module Make
    (Static : STATIC)
    (Proto : Registered_protocol.T)
    (Mempool_filters: Mempool_filters.T with module Proto = Proto)
    (Mempool_advertiser : Mempool_advertiser.T with module Proto = Proto)
  : T
    with module Proto = Proto
     and module Mempool_filters = Mempool_filters
     and module Mempool_advertiser = Mempool_advertiser
