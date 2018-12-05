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

type limits = {
  worker_limits : Worker_types.limits ;
  minimum_wait_between_advertisments : float ;
}

module type T = sig

  module Proto : Registered_protocol.T

  type t

  (** Creates/tear-down a new mempool advertiser. *)
  val create : limits -> Mempool_helpers.chain -> Mempool_helpers.head_info -> t tzresult Lwt.t
  val shutdown : t -> unit Lwt.t

  (** add the given operation to the internal queue for later advertisement *)
  val applied : t -> Operation_hash.t -> Proto.operation -> unit Lwt.t
  val branch_delayed : t -> Operation_hash.t -> Proto.operation -> unit Lwt.t

  (** advertise the queued operations and clear the queue *)
  val advertise : t -> unit Lwt.t

  (** introspection *)
  val status : t -> Worker_types.worker_status

end

module Make (Proto : Registered_protocol.T) : T with module Proto = Proto
