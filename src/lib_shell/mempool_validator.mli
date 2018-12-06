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

(** Tezos Shell - Validating mempool as they are gossiped on the network *)


type limits = {
  worker_limits: Mempool_worker.limits ;
  worker_max_size_parsed_cache: int ;
  peer_worker_limits: Mempool_peer_worker.limits ;
  peer_worker_max_pending_requests: int ;
  advertiser_limits: Mempool_advertiser.limits ;
}

(** A mempool validator. *)
type t


(** Recycling: left overs that might or might not be reusable across
    shutdown/restart. The main aim behind recycling is to let the chain_validator
    try to insert left over operations after a protocol change. Some operations
    might become unparsable or invalid, but others might be valid. *)
type recycling

(** Creates/tear-down a new prevalidator context. *)
val create:
  limits ->
  (module Mempool_filters.T) ->
  Distributed_db.chain_db ->
  t tzresult Lwt.t

val shutdown: t -> recycling Lwt.t

val recycle: t -> recycling -> unit Lwt.t


(** Common direct interactions *)

val validate: t -> P2p_peer.Id.t -> Mempool.t -> unit Lwt.t

val inject: t -> Operation.t -> unit tzresult Lwt.t

val new_head: t -> Block_hash.t -> unit tzresult Lwt.t


(** Some introspection useful to the chain_validator*)

(** Returns the fitness of the current prevalidation context *)
val fitness: t -> Fitness.t tzresult Lwt.t

(** Returns the hash of the protocol the prevalidator was instantiated with *)
val protocol_hash: t -> Protocol_hash.t

(** Returns the parameters the prevalidator was created with. *)
val limits: t -> limits
val chain: t -> Mempool_helpers.chain


type status = {
  advertiser : Worker_types.worker_status ;
  worker : Worker_types.worker_status ;
  peer_workers : Worker_types.worker_status P2p_peer.Id.Map.t ;
}

val status: t -> status

val rpc_directory : t option RPC_directory.t
