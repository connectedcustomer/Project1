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


(* The prevalidator filters in operation hashes propagated by other peers to
 * integrate it into the node's mempool: "_the_ mempool".
 * "_A_ mempool" is a set of operations that are valid or at least not invalid. *)

(* The prevalidator is sectionned into the following components:
 *
 * - the present file [prevalidator]
 * - [prevalidator_workers]
 * - [prevalidation]
 * - [prevalidator_filters]
 * *)

type limits = Prevalidator_sigs.limits = {
  max_refused_operations : int ;
  max_queued_operations : int ;
  operation_timeout : float ;
  worker_limits : Worker_types.limits ;
}

type t = (module Prevalidator_sigs.PREVALIDATOR_CONTEXT)

module Chain_level_registry
  : Registry.S
    with type k = Chain_id.t
     and type v = t
  =
  Registry.Make(
  struct
    type v = t
    type t = Chain_id.t
    let compare = Chain_id.compare
  end)

type error += Closed of Chain_id.t

let create limits (module Filter: Prevalidator_filters.FILTER) chain_db =
  let chain_state = Distributed_db.chain_state chain_db in
  let chain_id = State.Chain.id chain_state in
  let module Proto = Filter.Proto in
  let module Context =
    Prevalidator_workers.Make
      (struct
        module Proto = Proto
        module Filter = Filter
        let limits = limits
        let chain_state = chain_state
        let chain_db = chain_db
      end) in
  (* Check that initialisation is ok *)
  Context.init_errors >>=? fun () -> (* catching init errors *)
  let t: t = (module Context: Prevalidator_sigs.PREVALIDATOR_CONTEXT) in
  Chain_level_registry.register chain_id t;
  return t

let notify_operations (t:t) peer_id mempool =
  let module Context = (val t: Prevalidator_sigs.PREVALIDATOR_CONTEXT) in
  let t =
    begin
      match Context.Peer_level_registry.query peer_id with
      | Some worker ->
          Lwt.return worker
      | None ->
          (* TODO[SCORE]: find score from peer metadata, set table bounds accordingly *)
          Context.launch_and_register_worker peer_id
    end >>= fun worker ->
    Context.Worker_pool.push_request_and_wait
      worker
      (Context.Req_mempool mempool)
      (* For now, we ignore the result. when returning to the chain validator. *)
      (* NOTE: the results are passed onto on_completion for logging and such *)
  in
  ignore (t: (Operation_list_hash.elt * Context.inclusion_result) list tzresult Lwt.t);
  ()


let flush (t:t) head =
  (* NOTE: it is the responsibility of the chain_validator to shutdown and
   * create a new prevalidator when the protocol changes. In this here function,
   * we assume the new head does *not* carry a change of protocol. *)
  let module Context = (val t: Prevalidator_sigs.PREVALIDATOR_CONTEXT) in
  Context.notify_head head

let shutdown (t:t) =
  let module Context = (val t: Prevalidator_sigs.PREVALIDATOR_CONTEXT) in
  Context.Peer_level_registry.iter_p
    (fun _ worker -> Context.Worker_pool.shutdown worker)

let inject_operation (t:t) op =
  let module Context = (val t: Prevalidator_sigs.PREVALIDATOR_CONTEXT) in
  Context.prevalidate_injection op >>= fun _ ->
  Lwt.return_unit



(* Helper functions for the RPC directory *)

let running_workers () =
  Chain_level_registry.fold
    (fun chain_id t acc ->
       let module Context = (val t: Prevalidator_sigs.PREVALIDATOR_CONTEXT) in
       (Context.Peer_level_registry.fold
          (fun peer_id worker acc ->
             let status = Context.Worker_pool.status worker in
             (chain_id, peer_id, status) :: acc)
          acc))
    []

let workers_of_t (t:t) =
  let module Context = (val t: Prevalidator_sigs.PREVALIDATOR_CONTEXT) in
  Context.Peer_level_registry.fold
    (fun _ w acc -> Context.Worker_pool.status w :: acc)
    []

let ts () = Chain_level_registry.fold (fun _ t acc -> t :: acc) []

let rpc_directory = Pervasives.failwith "TODO[RPC]"
