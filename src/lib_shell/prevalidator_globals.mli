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


(** TODO? package that neatly so it can be used separately for each chain?
 * The peer workers need it, but it's a bit more complicated for the result
 * cache: completely invalid (e.g., content not matching hash) blocks should be
 * globally banned, but otherly invalid ops shouldn't (they might be valid in a
 * different chain).
 * *)

(* The type for registering known hashes *)
type cache_entry =
  | Invalid
  | Valid
  | Being_processed

(** [query_result_cache hash] is
    - [None] if the operation is not known
    - [Some x] if the operation is known to be x
*)
val query_result_cache: Operation_hash.t -> cache_entry option

(** [insert_result_in_cache hash result] associates the [result] to the [hash]
    in the result cache.
*)
val insert_result_in_cache: Operation_hash.t -> cache_entry -> unit




(** Register the peer worker associated with the peer *)
val register_peer_worker: P2p_peer.Id.t -> Prevalidator_workers.worker -> unit

(** Replaces the worker registered by the score (TODO) of the peer. In the
 * future, this remembered score can be used to initialise a new peer worker. *)
val unregister_peer_worker: P2p_peer.Id.t -> Prevalidator_workers.worker -> unit

type peer_registration =
  | Active of Prevalidator_workers.worker
  | Inactive (* TODO to maintain score across disconnects: of score *)

val query_peer: P2p_peer.Id.t -> peer_registration option


