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

open Validation_errors

module type T = sig

  module Proto : Registered_protocol.T

  type t

  type operation = private {
    hash: Operation_hash.t ;
    raw: Operation.t ;
    protocol_data: Proto.operation_data ;
  }

  val parse: Operation.t -> operation tzresult
  val compare: operation -> operation -> int

  val parse_list:
    Operation.t list ->
    operation list * (Operation.t * error list) list

  val start:
    ?protocol_data: MBytes.t ->
    predecessor: State.Block.t ->
    timestamp: Time.t ->
    unit -> t tzresult Lwt.t

  type result =
    | Applied of t * operation * Proto.operation_receipt
    | Branch_delayed of operation * error list
    | Branch_refused of error list
    | Refused of error list
    | Duplicate
    | Outdated

  val apply_operation: t -> operation -> result Lwt.t

  val apply_operation_list:
    error Preapply_result.t -> t -> Operation.t list ->
    (error Preapply_result.t * t) Lwt.t

  val finalize:
    t ->
    ( Tezos_protocol_environment_shell.validation_result
      * (operation * Proto.operation_receipt) list
      * Proto.block_header_metadata ) tzresult Lwt.t

end

module Make(Proto : Registered_protocol.T) = struct

  module Proto = Proto

  type operation = {
    hash: Operation_hash.t ;
    raw: Operation.t ;
    protocol_data: Proto.operation_data ;
  }

  type t = {
    state : Proto.validation_state ;
    applied : (operation * Proto.operation_receipt) list ;
    known : (operation * Proto.operation_receipt) Operation_hash.Map.t ;
    live_blocks : Block_hash.Set.t ;
    live_operations : Operation_hash.Set.t ;
  }

  type result =
    | Applied of t * operation * Proto.operation_receipt
    | Branch_delayed of operation * error list
    | Branch_refused of error list
    | Refused of error list
    | Duplicate
    | Outdated

  let parse (raw : Operation.t) =
    let hash = Operation.hash raw in
    let size = Data_encoding.Binary.length Operation.encoding raw in
    if size > Proto.max_operation_data_length then
      error
        (Oversized_operation
           { size ; max = Proto.max_operation_data_length })
    else
      match Data_encoding.Binary.of_bytes
              Proto.operation_data_encoding
              raw.Operation.proto with
      | None -> error Parse_error
      | Some protocol_data ->
          ok { hash ; raw ; protocol_data }

  let compare op1 op2 =
    Proto.compare_operations
      { shell = op1.raw.shell ; protocol_data = op1.protocol_data }
      { shell = op2.raw.shell ; protocol_data = op2.protocol_data }

  let parse_list ops =
    let ops = List.map (fun op -> op, parse op) ops in
    let invalid_ops =
      List.filter_map
        (function
          | _, Ok _ -> None
          | op, Error err -> Some (op, err))
        ops
    and parsed_ops =
      List.filter_map
        (function
          | _, Ok parsed_op -> Some parsed_op
          | _, Error _ -> None)
        ops in
    parsed_ops, invalid_ops

  let start ?protocol_data ~predecessor ~timestamp () =
    (* FIXME: the timestamp argument is immediately shadowed, and thus unused *)
    let { Block_header.shell =
            { fitness = predecessor_fitness ;
              timestamp = predecessor_timestamp ;
              level = predecessor_level } } =
      State.Block.header predecessor in
    let predecessor_hash = State.Block.hash predecessor in
    State.Block.context predecessor >>= fun predecessor_context ->
    Context.get_protocol predecessor_context >>= fun protocol ->
    assert (Protocol_hash.equal protocol Proto.hash) ;
    Chain_traversal.live_blocks
      predecessor
      (State.Block.max_operations_ttl predecessor)
    >>= fun (live_blocks, live_operations) ->
    Context.reset_test_chain
      predecessor_context predecessor_hash
      timestamp >>= fun predecessor_context ->
    begin
      match protocol_data with
      | None -> return_none
      | Some protocol_data ->
          match
            Data_encoding.Binary.of_bytes
              Proto.block_header_data_encoding
              protocol_data
          with
          | None -> failwith "Invalid block header"
          | Some protocol_data -> return_some protocol_data
    end >>=? fun protocol_data ->
    Proto.begin_construction
      ~chain_id: (State.Block.chain_id predecessor)
      ~predecessor_context
      ~predecessor_timestamp
      ~predecessor_fitness
      ~predecessor_level
      ~predecessor: predecessor_hash
      ~timestamp
      ?protocol_data
      () >>=? fun state ->
    return {
      state ;
      applied = [] ;
      known = Operation_hash.Map.empty ;
      live_blocks ;
      live_operations ;
    }

  let apply_operation pv op =
    if Operation_hash.Set.mem op.hash pv.live_operations then
      Lwt.return Outdated
    else
      Proto.apply_operation pv.state
        { shell = op.raw.shell ; protocol_data = op.protocol_data } >|= function
      | Ok (state, receipt) ->
          let pv =
            { state ;
              applied = (op, receipt) :: pv.applied ;
              known =
                Operation_hash.Map.add
                  op.hash (op, receipt)
                  pv.known ;
              live_blocks = pv.live_blocks ;
              live_operations = Operation_hash.Set.add op.hash pv.live_operations ;
            } in
          Applied (pv, op, receipt)
      | Error errors ->
          match classify_errors errors with
          | `Branch -> Branch_refused errors
          | `Permanent -> Refused errors
          | `Temporary -> Branch_delayed (op,errors)

  let apply_operation_list result state operations =
    let parsed_operations, _invalid_ops = (* FIXME *)
      parse_list operations in
    let sorted_operations =
      List.sort compare parsed_operations in
    Lwt_list.fold_left_s
      (fun ((result : _ Preapply_result.t), (state: t)) op ->
         apply_operation state op >>= function
         | Applied (new_state, _op, _receipt) ->
             Lwt.return
               ({ result with
                  applied = (op.hash, op.raw) :: result.applied },
                new_state)
         | Branch_delayed (op, errors) ->
             Lwt.return
               ({ result with
                  branch_delayed =
                    Operation_hash.Map.add
                      op.hash
                      (op.raw, errors)
                      result.branch_delayed },
                state)
         | Branch_refused errors ->
             Lwt.return
               ({ result with
                  branch_refused =
                    Operation_hash.Map.add
                      op.hash
                      (op.raw, errors)
                      result.branch_refused },
                state)
         | Refused errors ->
             Lwt.return
               ({ result with
                  refused =
                    Operation_hash.Map.add
                      op.hash
                      (op.raw, errors)
                      result.refused },
                state)
         | Duplicate ->
             Lwt.return (result, state)
         | Outdated ->
             Lwt.return (result, state))
      (result, state)
      sorted_operations

  let finalize pv =
    Proto.finalize_block pv.state >>=? fun (result, receipt) ->
    return (result, pv.applied, receipt)

end

let preapply ~predecessor ~timestamp ~protocol_data (* ~sort_operations:_ *)
    (module PV : T) operations =
  PV.start ~protocol_data ~predecessor ~timestamp () >>=? fun validation_state ->
  Lwt_list.fold_left_s (fun (acc_validation_result, acc_validation_state) ops ->
      PV.apply_operation_list
        Preapply_result.empty
        acc_validation_state
        ops
      >>= fun (new_validation_result, new_validation_state) ->
      Lwt.return (acc_validation_result @ [new_validation_result], new_validation_state)
    ) ([], validation_state) operations
  >>= fun (validation_result_list, validation_state) ->
  let operations_hash =
    Operation_list_list_hash.compute
      (List.map (fun r ->
           Operation_list_hash.compute
             (List.map fst r.Preapply_result.applied)
         ) validation_result_list)
  in
  PV.finalize validation_state >>=? fun (validation_result,_,_) ->
  let pred_shell_header = State.Block.shell_header predecessor in
  let level = Int32.succ pred_shell_header.level in
  Block_validator.may_patch_protocol
    ~level validation_result >>=? fun { fitness ; context ; message } ->
  State.Block.protocol_hash predecessor >>= fun pred_protocol ->
  Context.get_protocol context >>= fun protocol ->
  let proto_level =
    if Protocol_hash.equal protocol pred_protocol then
      pred_shell_header.proto_level
    else
      ((pred_shell_header.proto_level + 1) mod 256) in
  let shell_header : Block_header.shell_header = {
    level ;
    proto_level ;
    predecessor = State.Block.hash predecessor ;
    timestamp ;
    validation_passes = List.length validation_result_list ;
    operations_hash ;
    fitness ;
    context = Context_hash.zero ; (* place holder *)
  } in
  begin
    if Protocol_hash.equal protocol pred_protocol then
      return (context, message)
    else
      match Registered_protocol.get protocol with
      | None ->
          fail (Block_validator_errors.Unavailable_protocol
                  { block = State.Block.hash predecessor ; protocol })
      | Some (module NewProto) ->
          NewProto.init context shell_header >>=? fun { context ; message ; _ } ->
          return (context, message)
  end >>=? fun (context, message) ->
  Context.hash ?message ~time:timestamp context >>= fun context ->
  return ({ shell_header with context }, validation_result_list)
