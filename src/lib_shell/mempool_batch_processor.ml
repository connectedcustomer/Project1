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

module type T = sig
  module Proto : Registered_protocol.T
  module Operation_validator : Operation_validator.T with module Proto = Proto
  type input = Operation_hash.t list
  type input_parsed = Operation_validator.operation list
  type result =
    | Cannot_download of error list
    | Cannot_parse of error list
    | Cannot_validate of error list
    | Mempool_result of Operation_validator.result
  type output = result Operation_hash.Map.t
  val batch: Operation_validator.t -> int -> input -> output Lwt.t
  val batch_parsed: Operation_validator.t -> int -> input_parsed -> output Lwt.t
end

module Make
    (Proto : Registered_protocol.T)
    (Operation_validator : Operation_validator.T with module Proto = Proto)
  : T
    with module Proto = Proto
     and module Operation_validator = Operation_validator
= struct

  module Proto = Proto
  module Operation_validator = Operation_validator

  type input = Operation_hash.t list
  type input_parsed = Operation_validator.operation list
  type result =
    | Cannot_download of error list
    | Cannot_parse of error list
    | Cannot_validate of error list
    | Mempool_result of Operation_validator.result
  type output = result Operation_hash.Map.t

  type t = {
    pool: unit Lwt_pool.t;
    received: Operation_hash.t Queue.t;
    downloading: (Operation_hash.t * Operation.t tzresult Lwt.t) Queue.t;
    parsing: (Operation_hash.t * Operation_validator.operation tzresult) Queue.t;
    applying: (Operation_validator.operation * Operation_validator.result tzresult Lwt.t) Queue.t;
    mutable results: result Operation_hash.Map.t
  }

  (* Primitives *)

  let is_empty t =
    Queue.is_empty t.received &&
    Queue.is_empty t.downloading &&
    Queue.is_empty t.parsing &&
    Queue.is_empty t.applying

  let has_resolved t = match Lwt.state t with
    | Lwt.Return _ | Lwt.Fail _ -> true
    | Lwt.Sleep -> false

  let head_is_resolved q =
    (not (Queue.is_empty q)) && has_resolved (snd (Queue.peek q))

  let select t =
    (* A `select`-like function to wait on any of the pipeline's buffers'
     * heads to resolve *)
    assert (not (Queue.is_empty t.downloading && Queue.is_empty t.applying));
    let first_task_or_never q =
      if Queue.is_empty q then
        Lwt_utils.never_ending ()
      else
        snd (Queue.peek q) >>= fun _ -> Lwt.return_unit
    in
    Lwt.choose (
      (first_task_or_never t.downloading) ::
      (first_task_or_never t.applying) ::
      []
    )

  let record_result pipeline op_hash result =
    pipeline.results <- Operation_hash.Map.add op_hash result pipeline.results

  let q_of_list l =
    let q = Queue.create () in
    List.iter (fun x -> Queue.add x q) l;
    q

  let create pool_size op_hashes =
    {
      pool = Lwt_pool.create pool_size Lwt.return;
      received = q_of_list op_hashes;
      downloading = Queue.create ();
      parsing = Queue.create ();
      applying = Queue.create ();
      results = Operation_hash.Map.empty;
    }

  let create_parsed pool_size ops =
    let parsing = List.map (fun op -> (op.Operation_validator.hash, Ok op)) ops in
    {
      pool = Lwt_pool.create pool_size Lwt.return;
      received = Queue.create () ;
      downloading = Queue.create ();
      parsing = q_of_list parsing ;
      applying = Queue.create ();
      results = Operation_hash.Map.empty;
    }

  let cancel pipeline =
    let cancel_snd (_, p) = Lwt.cancel p in
    Queue.iter cancel_snd pipeline.downloading;
    Queue.iter cancel_snd pipeline.applying


  (* Exported interactions *)

  let step operation_validator pipeline =
    (* Going through each buffer one by one. *)
    (* op_hash: Opertation_hash.t
     * op: Operation.t
     * mop: Operation_validator.operation *)

    if head_is_resolved pipeline.applying then begin
      let (op, p) = Queue.pop pipeline.applying in
      p >>= function
      | Error errs ->
          record_result pipeline op.hash (Cannot_validate errs);
          Lwt.return_unit
      | Ok mempool_result ->
          record_result pipeline op.hash (Mempool_result mempool_result);
          Lwt.return_unit
    end

    else if not (Queue.is_empty pipeline.parsing) then begin
      let (op_hash, p) = Queue.pop pipeline.parsing in
      match p with
      | Error errs ->
          record_result pipeline op_hash (Cannot_parse errs);
          Lwt.return_unit
      | Ok mop ->
          let p =
            Lwt_pool.use pipeline.pool (fun () ->
                Operation_validator.validate operation_validator mop) in
          Queue.push (mop, p) pipeline.applying;
          Lwt.return_unit
    end

    else if head_is_resolved pipeline.downloading then begin
      let (op_hash, p) = Queue.pop pipeline.downloading in
      p >>= function
      | Error errs ->
          record_result pipeline op_hash (Cannot_download errs);
          Lwt.return_unit
      | Ok op ->
          Queue.push (op_hash, Operation_validator.parse op) pipeline.parsing;
          Lwt.return_unit
    end

    else if (not (Queue.is_empty pipeline.received)) then begin
      let op_hash = Queue.pop pipeline.received in
      (* TODO[?] should we specify the current peer for fetching? *)
      let chain = Operation_validator.chain operation_validator in
      let p =
        Lwt_pool.use pipeline.pool (fun () ->
            Distributed_db.Operation.fetch chain.db op_hash ()) in
      Queue.push (op_hash, p) pipeline.downloading;
      Lwt.return_unit
    end

    else
      (* There are some pending operations, we need to wait on them *)
      select pipeline >>= fun () ->
      Lwt.return_unit

  let batch operation_validator max_concurrency input =
    let pipeline = create max_concurrency input in
    let rec loop () =
      if is_empty pipeline then
        Lwt.return pipeline.results
      else
        step operation_validator pipeline >>= fun () ->
        loop ()
    in
    let work = loop () in
    Lwt.on_cancel work (fun () -> cancel pipeline);
    work

  let batch_parsed operation_validator max_concurrency input =
    let pipeline = create_parsed max_concurrency input in
    let rec loop () =
      if is_empty pipeline then
        Lwt.return pipeline.results
      else
        step operation_validator pipeline >>= fun () ->
        loop ()
    in
    let work = loop () in
    Lwt.on_cancel work (fun () -> cancel pipeline);
    work

end

