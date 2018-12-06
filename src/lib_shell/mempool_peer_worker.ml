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

(** Validating batches of operations with some peer-based
    compartimentatilsation. *)

(** [max_promises_per_request] is the number of high-level promises that can be
    unresolved at one time whilst processing one batch of operation by one peer.
    The high-level promises include "retreiving an operation from the
    distributed db," or "validating the operation on top of the currently
    maintained context." Note that each of these pending promises may create
    (and wait upon the resolving of) more promises. As such,
    [max_promises_per_request] is not a true limit of the concurrency of the
    process, but it nonetheless is helpful in throttling peer workers to ensure
    each has a chance to make progress.

    [worker_limits] limits the number of pending request and other such generic
    limitations on workers.
*)
type limits = {
  max_promises_per_request : int ;
  worker_limits : Worker_types.limits ;
}

(** A module [T] can be used to distribute work to different workers depending
    on which peer the work has been triggered by. Note that the module does not
    enforce that work is performed by the appropriate peer. It is up to the user
    of a [T] module to manage a set of peer worker and select the appropriate
    one to give work to. *)
module type T = sig

  (** Underlying modules. Validation of individual operations is delegated to
      [Operation_validator]. *)
  module Proto: Registered_protocol.T
  module Operation_validator: Operation_validator.T
    with module Proto = Proto
  module Mempool_batch_processor: Mempool_batch_processor.T
    with module Proto = Proto
     and module Operation_validator = Operation_validator

  (** A peer worker. *)
  type t

  (** Types for calls into this module *)

  (** [input] are the batches of operations that are given to a peer worker to
   * validate. These hashes are gossiped on the network, and the mempool checks
   * their validity before gossiping them furhter. *)
  type input = Operation_hash.t list

  (** [create limits peer_id op_validator] is, unless an error occurs durring
      initialization, a peer worker meant to handle the work provided by peer
      [peer_id]. The underlying validation of individual operation is delegated
      to [op_validator]. *)
  val create: limits -> P2p_peer.Id.t -> Operation_validator.t -> t tzresult Lwt.t

  (** [shutdown worker] cancels ongoing work, clears internal state and returns
      operations that have not been validated yet. This lets the user of this
      module retry some of the validation work if appropriate. *)
  val shutdown: t -> input Lwt.t

  (** [validate worker op_hashes] makes [worker] validate [op_hashes]. The
      [worker] validates batches of operations one at a time. During the
      validation of one batch, at most [limits.max_promises_per_request]
      high-level promises are pending at any one time. *)
  val validate: t -> input -> unit tzresult Lwt.t

  (** [status worker] gives generic information about the worker state and its
      requests. *)
  val status: t -> Worker_types.worker_status

end


module type STATIC = sig
  val max_pending_requests : int
end

module Make
    (Static: STATIC)
    (Proto: Registered_protocol.T)
    (Operation_validator: Operation_validator.T with module Proto = Proto)
    (Mempool_batch_processor: Mempool_batch_processor.T
     with module Proto = Proto
      and module Operation_validator = Operation_validator)
  : T
    with module Proto = Proto
     and module Operation_validator = Operation_validator
     and module Mempool_batch_processor = Mempool_batch_processor
= struct

  (* 0. Prelude: set up base modules and types *)
  (* See interface file for info if needed. *)

  module Proto = Operation_validator.Proto
  module Operation_validator = Operation_validator
  module Mempool_batch_processor = Mempool_batch_processor

  type input = Operation_hash.t list
  type result = Mempool_batch_processor.result =
    | Cannot_download of error list
    | Cannot_parse of error list
    | Cannot_validate of error list
    | Mempool_result of Operation_validator.result
  type output = result Operation_hash.Map.t

  let pp_input ppf input =
    Format.fprintf ppf
      "@[<v 0>%a@]"
      (Format.pp_print_list Operation_hash.pp)
      input
  let result_encoding =
    let open Data_encoding in
    union
      [ case (Tag 0)
          ~title:"Cannot download"
          (obj1 (req "download_errors" (list Error_monad.error_encoding)))
          (function Cannot_download errs -> Some errs | _ -> None)
          (fun errs -> Cannot_download errs) ;
        case (Tag 1)
          ~title:"Cannot parse"
          (obj1 (req "parse_errors" (list Error_monad.error_encoding)))
          (function Cannot_parse errs -> Some errs | _ -> None)
          (fun errs -> Cannot_parse errs) ;
        case (Tag 2)
          ~title:"Cannot validate"
          (obj1 (req "validation_errors" (list Error_monad.error_encoding)))
          (function Cannot_validate errs -> Some errs | _ -> None)
          (fun errs -> Cannot_validate errs) ;
        case (Tag 3)
          ~title:"Validation result"
          (obj1 (req "validation_result" Operation_validator.result_encoding))
          (function Mempool_result result -> Some result | _ -> None)
          (fun result -> Mempool_result result) ]

  module Log = Tezos_stdlib.Logging.Make(struct
      let name = "node.shell.mempool.peer_worker"
    end)

  (* 1. Boilerplate: the set up for the worker architecture *)

  module Name = struct
    type t = P2p_peer.Id.t
    let encoding = P2p_peer.Id.encoding
    let base =
      let proto_hash = Format.asprintf "%a" Protocol_hash.pp Proto.hash in
      [ "node" ; "shell" ; "mempool" ; "peer_worker" ; proto_hash ]
    let pp = P2p_peer.Id.pp
  end

  module Request = struct
    type 'a t = Batch : input -> output t
    type view = input
    let view
      : type a. a t -> view
      = fun (Batch os) -> os
    let encoding =
      let open Data_encoding in
      list Operation_hash.encoding
    let pp ppf os =
      Format.fprintf ppf
        "@[<v 2>Request:@,%a@]"
        (Format.pp_print_list Operation_hash.pp)
        os
  end

  module Event = struct
    type t =
      | Start of input
      | End_ok of (Request.view * Worker_types.request_status * output)
      | End_error of (Request.view * Worker_types.request_status * error list)

    let level req =
      match req with
      | Start _ -> Logging.Info
      | End_ok _ -> Logging.Info
      | End_error _ -> Logging.Error

    let encoding =
      let open Data_encoding in
      union
        [ case (Tag 0)
            ~title:"Start"
            (obj1 (req "input" (list Operation_hash.encoding)))
            (function Start input -> Some input | _ -> None)
            (fun input -> Start input) ;
          case (Tag 1)
            ~title:"End_ok"
            (obj3
               (req "request" Request.encoding)
               (req "status" Worker_types.request_status_encoding)
               (req "output" (Operation_hash.Map.encoding result_encoding)))
            (function End_ok (view, status, result) -> Some (view, status, result) | _ -> None)
            (fun (view, status, result) -> End_ok (view, status, result)) ;
          case (Tag 2)
            ~title:"End_error"
            (obj3
               (req "failed_request" Request.encoding)
               (req "status" Worker_types.request_status_encoding)
               (req "error" RPC_error.encoding))
            (function End_error (view, status, errs) -> Some (view, status, errs) | _ -> None)
            (fun (view, status, errs) -> End_error (view, status, errs)) ]

    let pp ppf = function
      | Start input ->
          Format.fprintf ppf
            "@[<v 0>Starting: %a@]"
            pp_input
            input
      | End_ok (view, _, _) ->
          Format.fprintf ppf
            "@[<v 0>Finished: %a@]"
            Request.pp view
      | End_error (view, _, errs) ->
          Format.fprintf ppf
            "@[<v 0>Errors: %a, Operations: %a@]"
            (Format.pp_print_list Error_monad.pp) errs
            Request.pp view
  end

  module Types = struct
    type parameters = Operation_validator.t * int
    type state = { operation_validator: Operation_validator.t ; pool_size: int }
    type view = unit
    let view _ _ = ()
    let encoding = Data_encoding.unit
    let pp _ _ = ()
  end

  module Worker = Worker.Make (Name) (Event) (Request) (Types)
  type t = Worker.bounded Worker.queue Worker.t
  let table =
    let open Worker in
    create_table (Bounded { size = Static.max_pending_requests })


  (* 2. Workers' work: setting workers' callbacks to perform core work *)

  module Handlers = struct

    type self = t

    let on_launch _ _ (operation_validator, pool_size) =
      return Types.{ operation_validator; pool_size }

    let on_request : type a. self -> a Request.t -> a tzresult Lwt.t
      = fun t (Request.Batch os) ->
        let st = Worker.state t in
        Worker.record_event t (Event.Start os) ;
        Mempool_batch_processor.batch
          st.operation_validator
          st.pool_size
          os >>= fun r ->
        return r

    let on_no_request _ = return_unit

    let on_close _ = Lwt.return_unit

    let on_error t view st errs =
      Worker.record_event t (Event.End_error (view, st, errs)) ;
      Lwt.return (Error errs)

    let on_completion
      : type a. self -> a Request.t -> a -> Worker_types.request_status -> unit Lwt.t
      = fun t req output st ->
        match req with
        | Request.Batch _ ->
            Worker.record_event t (Event.End_ok (Request.view req, st, output)) ;
            Lwt.return_unit

  end


  (* 3. Public interface: exporting a thin wrapper around workers and work. *)
  (* See interface file for documentation *)

  let validate t os =
    Worker.push_request_and_wait t (Request.Batch os)
    >>=? fun (_: output) -> return_unit

  let create limits peer_id operation_validator =
    Worker.launch
      table
      limits.worker_limits
      peer_id
      (operation_validator, limits.max_promises_per_request)
      (module Handlers)

  let shutdown w =
    let recycled = Operation_hash.Set.empty in
    let recycled =
      List.fold_left
        (fun recycled (_, input) ->
           List.fold_left
             (fun recycled op_h -> Operation_hash.Set.add op_h recycled)
             recycled
             input)
        recycled
        (Worker.pending_requests w)
    in
    let recycled =
      match Worker.current_request w with
      | Some (_, _, input) ->
          List.fold_left
            (fun recycled op_h -> Operation_hash.Set.add op_h recycled)
            recycled
            input
      | None -> recycled
    in
    let input = Operation_hash.Set.elements recycled in
    Worker.shutdown w >>= fun () ->
    Lwt.return input

  let status t = Worker.status t

end
