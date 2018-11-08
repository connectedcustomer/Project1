(*****************************************************************************)
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

type limits = {
  worker_limits : Worker_types.limits ;
}

module type T = sig

  module Proto: Registered_protocol.T

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
    | Duplicate
    | Not_in_branch

  (** Creates/tear-down a new mempool validator context. *)
  val create : limits -> Distributed_db.chain_db -> t tzresult Lwt.t
  val shutdown : t -> unit Lwt.t

  (** parse a new operation and add it to the mempool context *)
  val parse : t -> Operation.t -> operation tzresult Lwt.t

  (** validate a new operation and add it to the mempool context *)
  val validate : t -> operation -> result tzresult Lwt.t

  val chain_db : t -> Distributed_db.chain_db

  val rpc_directory : t RPC_directory.t

end

module Make(Proto: Registered_protocol.T) : T with module Proto = Proto = struct

  module Proto = Proto

  (* used for rpc *)
  module Proto_services = Block_services.Make(Proto)(Proto)

  type operation = {
    hash: Operation_hash.t ;
    raw: Operation.t ;
    protocol_data: Proto.operation_data ;
  }

  type result =
    | Applied of Proto.operation_receipt
    | Branch_delayed of error list
    | Branch_refused of error list
    | Refused of error list
    | Duplicate
    | Not_in_branch

  let result_encoding =
    let open Data_encoding in
    union
      [ case (Tag 0)
          ~title:"Applied"
          (obj1 (req "receipt" Proto.operation_receipt_encoding))
          (function Applied receipt -> Some receipt | _ -> None)
          (fun receipt -> Applied receipt) ;
        case (Tag 1)
          ~title:"Branch Delayed"
          (obj1 (req "error" (list Error_monad.error_encoding)))
          (function Branch_delayed error -> Some error | _ -> None)
          (fun error -> Branch_delayed error) ;
        case (Tag 2)
          ~title:"Branch Refused"
          (obj1 (req "error" (list Error_monad.error_encoding)))
          (function Branch_refused error -> Some error | _ -> None)
          (fun error -> Branch_refused error) ;
        case (Tag 3)
          ~title:"Refused"
          (obj1 (req "error" (list Error_monad.error_encoding)))
          (function Refused error -> Some error | _ -> None)
          (fun error -> Refused error) ;
        case (Tag 4)
          ~title:"Duplicate"
          empty
          (function Duplicate -> Some () | _ -> None)
          (fun () -> Duplicate) ;
        case (Tag 5)
          ~title:"Not_in_branch"
          empty
          (function Not_in_branch -> Some () | _ -> None)
          (fun () -> Not_in_branch) ;
      ]

  let operation_encoding =
    let open Data_encoding in
    conv
      (fun { hash ; raw ; protocol_data } ->
         ( hash, raw, protocol_data ))
      (fun ( hash, raw, protocol_data ) -> { hash ; raw ; protocol_data })
      (obj3
         (req "hash" Operation_hash.encoding)
         (req "raw" Operation.encoding)
         (req "protocol_data" Proto.operation_data_encoding)
      )

  module Log = Tezos_stdlib.Logging.Make(struct
      let name = "node.mempool_validator"
    end)

  module Name = struct
    type t = Chain_id.t * Protocol_hash.t
    let encoding =
      Data_encoding.tup2
        Chain_id.encoding
        Protocol_hash.encoding
    let base = [ "validator.mempool" ]
    let pp fmt (chain_id, proto_hash) =
      Chain_id.pp_short fmt chain_id;
      Format.pp_print_string fmt ".";
      Protocol_hash.pp_short fmt proto_hash
  end

  module Request = struct

    type 'a t =
      | Parse : Operation.t -> operation t
      | Validate : operation -> result t

    type view = View : _ t -> view

    let view req = View req

    let encoding =
      let open Data_encoding in
      union
        [ case (Tag 0)
            ~title:"Parsed_operation"
            (* XXX:  can't I use operation_encoding defined above ? *)
            (obj3
               (req "hash" Operation_hash.encoding)
               (req "raw" Operation.encoding)
               (req "protocol_data" Proto.operation_data_encoding))
            (function
              | View (Validate { hash ; raw ; protocol_data }) ->
                  Some ( hash, raw, protocol_data )
              | _ -> None)
            (fun ( hash, raw, protocol_data ) ->
               View (Validate { hash ; raw ; protocol_data })) ;
          case (Tag 1)
            ~title:"Raw operation"
            Operation.encoding
            (function | View (Parse op) -> Some op | _ -> None)
            (fun op -> View (Parse op))
        ]

    let pp ppf = function
      | View (Parse op) ->
          Format.fprintf ppf "New raw operation %a" Operation.pp op
      | View (Validate { hash }) ->
          Format.fprintf ppf "New parsed operation hash %a" Operation_hash.pp hash
  end

  module Event = struct
    type t =
      | Request of (Request.view * Worker_types.request_status * error list option)
      | Debug of string

    let level req =
      match req with
      | Debug _ -> Logging.Debug
      | Request _ -> Logging.Info

    let encoding =
      let open Data_encoding in
      union
        [ case (Tag 0)
            ~title:"Debug"
            (obj1 (req "message" string))
            (function Debug msg -> Some msg | _ -> None)
            (fun msg -> Debug msg) ;
          case (Tag 1)
            ~title:"Request"
            (obj2
               (req "request" Request.encoding)
               (req "status" Worker_types.request_status_encoding))
            (function Request (req, t, None) -> Some (req, t) | _ -> None)
            (fun (req, t) -> Request (req, t, None)) ;
          case (Tag 2)
            ~title:"Failed request"
            (obj3
               (req "error" RPC_error.encoding)
               (req "failed_request" Request.encoding)
               (req "status" Worker_types.request_status_encoding))
            (function Request (req, t, Some errs) -> Some (errs, req, t) | _ -> None)
            (fun (errs, req, t) -> Request (req, t, Some errs)) ]

    let pp ppf = function
      | Debug msg -> Format.fprintf ppf "%s" msg
      | Request (view, { pushed ; treated ; completed }, None)  ->
          Format.fprintf ppf
            "@[<v 0>%a@,\
             Pushed: %a, Treated: %a, Completed: %a@]"
            Request.pp view
            Time.pp_hum pushed Time.pp_hum treated Time.pp_hum completed
      | Request (view, { pushed ; treated ; completed }, Some errors)  ->
          Format.fprintf ppf
            "@[<v 0>%a@,\
             Pushed: %a, Treated: %a, Failed: %a@,\
             %a@]"
            Request.pp view
            Time.pp_hum pushed Time.pp_hum treated Time.pp_hum completed
            (Format.pp_print_list Error_monad.pp) errors
  end

  (* operations' cache. used for memoization *)
  module Cache = struct

    type t = {
      operations : result Operation_hash.Table.t ;
      parsed_operations : operation tzresult Operation_hash.Table.t ;
    }

    let encoding =
      let open Data_encoding in
      conv
        (fun { operations ; parsed_operations } -> (operations, parsed_operations))
        (fun (operations, parsed_operations) -> { operations ; parsed_operations })
        (obj2
           (req "operations" (Operation_hash.Table.encoding result_encoding))
           (req "parsed_operations"
              (Operation_hash.Table.encoding
                 (Error_monad.result_encoding operation_encoding)))
        )

    let create () =
      { operations = Operation_hash.Table.create 1000 ;
        parsed_operations = Operation_hash.Table.create 1000
      }

    let add_validated t parsed_op result =
      Operation_hash.Table.replace t.operations parsed_op.hash result

    let add_parsed t raw_op parsed_op =
      let hash = Operation.hash raw_op in
      Operation_hash.Table.replace t.parsed_operations hash parsed_op

    let mem_validated t parsed_op =
      Operation_hash.Table.mem t.operations parsed_op.hash

    let mem_parsed t raw_op =
      let hash = Operation.hash raw_op in
      Operation_hash.Table.mem t.parsed_operations hash

    let find_validated_opt t parsed_op =
      Operation_hash.Table.find_opt t.operations parsed_op.hash

    let find_parsed_opt t raw_op =
      let hash = Operation.hash raw_op in
      Operation_hash.Table.find_opt t.parsed_operations hash

    let iter_validated f t =
      Operation_hash.Table.iter f t.operations

    let to_mempool t =
      let empty = {
        Proto_services.Mempool.applied = [] ;
        refused = Operation_hash.Map.empty ;
        branch_refused = Operation_hash.Map.empty ;
        branch_delayed = Operation_hash.Map.empty ;
        unprocessed = Operation_hash.Map.empty ;
      } in
      let map_op op =
        let protocol_data =
          Data_encoding.Binary.of_bytes_exn
            Proto.operation_data_encoding
            op.Operation.proto in
        { Proto.shell = op.shell ; protocol_data } in
      Operation_hash.Table.fold
        (fun hash result acc ->
           match Operation_hash.Table.find_opt t.parsed_operations hash with
           (* XXX this invariant should be better enforced *)
           | None | Some (Error _) -> assert false
           | Some (Ok op) -> begin
               match result with
               | Applied _ -> {
                   acc with
                   Proto_services.Mempool.applied =
                     (hash, map_op op.raw)::acc.Proto_services.Mempool.applied
                 }
               | Branch_refused err -> {
                   acc with
                   Proto_services.Mempool.branch_refused =
                     Operation_hash.Map.add
                       hash
                       (map_op op.raw,err)
                       acc.Proto_services.Mempool.branch_refused
                 }
               | Branch_delayed err -> {
                   acc with
                   Proto_services.Mempool.branch_delayed =
                     Operation_hash.Map.add
                       hash
                       (map_op op.raw,err)
                       acc.Proto_services.Mempool.branch_delayed
                 }
               | Refused err -> {
                   acc with
                   Proto_services.Mempool.refused =
                     Operation_hash.Map.add
                       hash
                       (map_op op.raw,err)
                       acc.Proto_services.Mempool.refused
                 }
               | _ -> acc
             end)
        t.operations empty

    let clear t =
      Operation_hash.Table.clear t.operations;
      Operation_hash.Table.clear t.parsed_operations

  end

  module Types = struct

    type parameters = {
      limits : limits ;
      chain_db : Distributed_db.chain_db ;
      validation_state : Proto.validation_state ;
    }

    (* internal worker state *)
    type state =
      {
        (* state of the validator. this is updated at each apply_operation *)
        mutable validation_state : Proto.validation_state ;

        cache : Cache.t ;

        (* live blocks and operations, initialized at worker launch *)
        live_blocks : Block_hash.Set.t ;
        live_operations : Operation_hash.Set.t ;

        operation_stream: (
          result *
          Operation.shell_header *
          Proto.operation_data
        ) Lwt_watcher.input;

        parameters : parameters ;
      }

    type view = { cache : Cache.t }

    let view (state : state) _ : view = { cache = state.cache }

    let encoding =
      let open Data_encoding in
      conv
        (fun { cache } -> cache)
        (fun cache -> { cache })
        Cache.encoding

    let pp ppf _view =
      Format.fprintf ppf "lots of operations"

  end

  module Worker = Worker.Make (Name) (Event) (Request) (Types)

  open Types

  type t = Worker.infinite Worker.queue Worker.t

  let debug w =
    Format.kasprintf (fun msg -> Worker.record_event w (Debug msg))

  let shutdown w =
    Worker.shutdown w

  (*** prevalidation ****)
  open Validation_errors

  let create ?protocol_data ~predecessor ~timestamp () =
    let { Block_header.shell =
            { fitness = predecessor_fitness ;
              timestamp = predecessor_timestamp ;
              level = predecessor_level } } =
      State.Block.header predecessor in
    State.Block.context predecessor >>= fun predecessor_context ->
    let predecessor_hash = State.Block.hash predecessor in
    Context.reset_test_chain
      predecessor_context predecessor_hash
      timestamp >>= fun predecessor_context ->
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
      ~predecessor:predecessor_hash
      ~timestamp
      ?protocol_data
      ()

  let apply_operation state op =
    if Operation_hash.Set.mem op.hash state.live_operations then
      Lwt.return (None, Duplicate)
    else if not (Block_hash.Set.mem op.raw.Operation.shell.branch state.live_blocks) then
      Lwt.return (None,Not_in_branch)
    else
      Proto.apply_operation state.validation_state
        { shell = op.raw.shell ; protocol_data = op.protocol_data } >|= function
      | Ok (validation_state, receipt) ->
          (Some validation_state, Applied receipt)
      | Error errors ->
          (None,
           match classify_errors errors with
           | `Branch -> Branch_refused errors
           | `Permanent -> Refused errors
           | `Temporary -> Branch_delayed errors)

  (*** end prevalidation ***)

  let parse_helper (_ : t) raw_op =
    let hash = Operation.hash raw_op in
    let size = Data_encoding.Binary.length Operation.encoding raw_op in
    if size > Proto.max_operation_data_length then
      fail (Oversized_operation
              { size ; max = Proto.max_operation_data_length })
    else
      match Data_encoding.Binary.of_bytes
              Proto.operation_data_encoding
              raw_op.Operation.proto with
      | None -> fail Parse_error
      | Some protocol_data ->
          return { hash ; raw = raw_op ; protocol_data }

  (* this function update the internal state of the worker *)
  let validate_helper w parsed_op =
    let state = Worker.state w in
    apply_operation state parsed_op >>= fun (validation_state, result) ->
    begin
      match validation_state with
      | Some validation_state -> state.validation_state <- validation_state
      | None -> ()
    end ;
    Lwt.return result

  let notify_helper w result { Operation.shell ; proto } =
    let state = Worker.state w in
    (* this function is called by on_validate where we take care of the error *)
    let protocol_data =
      Data_encoding.Binary.of_bytes_exn
        Proto.operation_data_encoding
        proto in
    Lwt_watcher.notify state.operation_stream (result, shell, protocol_data)

  (* memoization is done only at on_* level *)
  let on_validate w parsed_op =
    let state = Worker.state w in
    match Cache.find_validated_opt state.cache parsed_op with
    | None | Some (Branch_delayed _) ->
        validate_helper w parsed_op >>= fun result ->
        Cache.add_validated state.cache parsed_op result;
        (* operations are notified only the first time *)
        notify_helper w result parsed_op.raw ;
        Lwt.return result
    | Some result -> Lwt.return result

  let on_parse w raw_op =
    let state = Worker.state w in
    match Cache.find_parsed_opt state.cache raw_op with
    | None ->
        parse_helper w raw_op >>= fun parsed_op ->
        Cache.add_parsed state.cache raw_op parsed_op;
        Lwt.return parsed_op
    | Some parsed_op -> Lwt.return parsed_op

  (* worker's handlers *)
  let on_request :
    type r. t -> r Request.t -> r tzresult Lwt.t = fun w request ->
    match request with
    | Request.Parse raw_op -> on_parse w raw_op
    | Request.Validate parsed_op -> on_validate w parsed_op >>= return

  let on_launch (_ : t) (_ : Name.t) ( { chain_db ; validation_state } as parameters ) =
    let chain_state = Distributed_db.chain_state chain_db in
    Chain.data chain_state >>= fun { current_mempool = _mempool ; live_blocks ; live_operations } ->
    Lwt.return {
      validation_state ;
      cache = Cache.create () ;
      live_blocks ;
      live_operations ;
      operation_stream = Lwt_watcher.create_input ();
      parameters
    }

  let on_close w =
    let state = Worker.state w in
    Lwt_watcher.shutdown_input state.operation_stream;
    Cache.iter_validated (fun hash _ ->
        Distributed_db.Operation.clear_or_cancel
          state.parameters.chain_db hash)
      state.cache ;
    Cache.clear state.cache;
    Lwt.return_unit

  let on_error w r st errs =
    Worker.record_event w (Event.Request (r, st, Some errs)) ;
    Lwt.return (Error errs)

  let on_completion w r _ st =
    Worker.record_event w (Event.Request (Request.view r, st, None)) ;
    Lwt.return_unit

  let table = Worker.create_table Queue

  let create limits chain_db  =
    let chain_state = Distributed_db.chain_state chain_db in
    let chain_id = State.Chain.id chain_state in
    let module Handlers = struct
      type self = t
      let on_launch = on_launch
      let on_close = on_close
      let on_error = on_error
      let on_completion = on_completion
      let on_no_request _ = return_unit
      let on_request = on_request
    end in
    Chain.data chain_state >>= fun { current_head = predecessor } ->
    let timestamp = Time.now () in
    create ~predecessor ~timestamp () >>=? fun validation_state ->
    (Worker.launch
       table
       limits.worker_limits
       (chain_id, Proto.hash)
       { limits ; chain_db ; validation_state }
       (module Handlers) >>= return)

  (* Exporting functions *)

  let validate t parsed_op =
    Worker.push_request_and_wait t (Request.Validate parsed_op)

  let parse t op =
    Worker.push_request_and_wait t (Request.Parse op)

  let chain_db t =
    let state = Worker.state t in
    state.parameters.chain_db

  let pending_rpc_directory : t RPC_directory.t =
    RPC_directory.gen_register
      RPC_directory.empty
      (Proto_services.S.Mempool.pending_operations RPC_path.open_root)
      (fun w () () ->
         let state = Worker.state w in
         RPC_answer.return (Cache.to_mempool state.cache)
      )

  let monitor_rpc_directory : t RPC_directory.t =
    RPC_directory.gen_register
      RPC_directory.empty
      (Proto_services.S.Mempool.monitor_operations RPC_path.open_root)
      (fun w params () ->
         let state = Worker.state w in
         let filter_result = function
           | Applied _ -> params#applied
           | Refused _ -> params#branch_refused
           | Branch_refused _ -> params#refused
           | Branch_delayed _ -> params#branch_delayed
           | _ -> false in

         let op_stream, stopper = Lwt_watcher.create_stream state.operation_stream in
         let shutdown () = Lwt_watcher.shutdown stopper in
         let next () =
           Lwt_stream.get op_stream >>= function
           | Some (kind, shell, protocol_data) when filter_result kind ->
               Lwt.return_some [ { Proto.shell ; protocol_data } ]
           | _ -> Lwt.return_none in
         RPC_answer.return_stream { next ; shutdown }
      )

  (* /mempool/<chain_id>/pending
     /mempool/<chain_id>/monitor *)
  let rpc_directory =
    RPC_directory.merge
      pending_rpc_directory
      monitor_rpc_directory

end
