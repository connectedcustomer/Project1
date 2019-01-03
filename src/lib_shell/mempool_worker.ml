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
  val shutdown : t -> unit Lwt.t

  (** parse a new operation and add it to the mempool context *)
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
= struct

  module Proto = Proto
  module Mempool_filters = Mempool_filters
  module Mempool_advertiser = Mempool_advertiser

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
    | Refused_by_pre_filter
    | Refused_by_post_filter
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
          ~title:"Refused by prefilter"
          unit
          (function Refused_by_pre_filter -> Some () | _ -> None)
          (fun () -> Refused_by_pre_filter) ;
        case (Tag 5)
          ~title:"Refused by postfilter"
          unit
          (function Refused_by_post_filter -> Some () | _ -> None)
          (fun () -> Refused_by_post_filter) ;
        case (Tag 6)
          ~title:"Duplicate"
          empty
          (function Duplicate -> Some () | _ -> None)
          (fun () -> Duplicate) ;
        case (Tag 7)
          ~title:"Not_in_branch"
          empty
          (function Not_in_branch -> Some () | _ -> None)
          (fun () -> Not_in_branch) ;
      ]

  let pp_result ppf = function
    | Applied _ -> Format.pp_print_string ppf "applied"
    | Branch_delayed _ -> Format.pp_print_string ppf "branch delayed"
    | Branch_refused _ -> Format.pp_print_string ppf "branch refused"
    | Refused _ -> Format.pp_print_string ppf "refused"
    | Refused_by_pre_filter -> Format.pp_print_string ppf "refused by pre-filter"
    | Refused_by_post_filter -> Format.pp_print_string ppf "refused by post-filter"
    | Duplicate -> Format.pp_print_string ppf "duplicate"
    | Not_in_branch -> Format.pp_print_string ppf "not in branch"

  let operation_encoding =
    let open Data_encoding in
    conv
      (fun { hash ; raw ; protocol_data } ->
         ( hash, raw, protocol_data ))
      (fun ( hash, raw, protocol_data ) -> { hash ; raw ; protocol_data })
      (obj3
         (req "hash" Operation_hash.encoding)
         (req "raw" (dynamic_size Operation.encoding))
         (req "protocol_data" (dynamic_size Proto.operation_data_encoding))
      )

  module Log = Tezos_stdlib.Logging.Make(struct
      let name = "node.mempool_validator"
    end)

  module Name = struct
    type t = Chain_id.t
    let encoding = Chain_id.encoding
    let base =
      let proto_hash =
        let _: string = Format.flush_str_formatter () in
        Format.fprintf Format.str_formatter "%a" Protocol_hash.pp Proto.hash;
        Format.flush_str_formatter () in
      [ "node"; "mempool"; "worker"; proto_hash ]
    let pp = Chain_id.pp_short
  end

  module Request = struct

    type 'a t = Validate : operation -> result t [@@ocaml.unboxed]

    type view = View : _ t -> view

    let view req = View req

    let encoding =
      let open Data_encoding in
      conv
        (fun (View (Validate op)) -> op)
        (fun op -> View (Validate op))
        (dynamic_size operation_encoding)

    let pp ppf (View (Validate { hash })) =
      Format.fprintf ppf "Validating new operation %a" Operation_hash.pp hash
  end

  module Event = struct
    type t =
      | Request of (Request.view * Worker_types.request_status * error list option)
      | Filter_config_update of Mempool_filters.config
      | Debug of string

    let level req =
      match req with
      | Debug _ -> Logging.Debug
      | Filter_config_update _ -> Logging.Info
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
            ~title:"Filter config update"
            (obj1 (req "config" Mempool_filters.config_encoding))
            (function Filter_config_update config -> Some config | _ -> None)
            (fun config -> Filter_config_update config) ;
          case (Tag 2)
            ~title:"Request"
            (obj2
               (req "request" Request.encoding)
               (req "status" Worker_types.request_status_encoding))
            (function Request (req, t, None) -> Some (req, t) | _ -> None)
            (fun (req, t) -> Request (req, t, None)) ;
          case (Tag 3)
            ~title:"Failed request"
            (obj3
               (req "error" RPC_error.encoding)
               (req "failed_request" Request.encoding)
               (req "status" Worker_types.request_status_encoding))
            (function Request (req, t, Some errs) -> Some (errs, req, t) | _ -> None)
            (fun (errs, req, t) -> Request (req, t, Some errs)) ]

    let pp ppf = function
      | Debug msg -> Format.fprintf ppf "%s" msg
      | Filter_config_update config ->
          Format.fprintf ppf "Updated filter configuration: %a"
            Mempool_filters.pp_config config
      | Request (view, { pushed ; treated ; completed }, None)  ->
          Format.fprintf ppf
            "@[<v 0>%a@,Pushed: %a, Treated: %a, Completed: %a@]"
            Request.pp view
            Time.pp_hum pushed Time.pp_hum treated Time.pp_hum completed
      | Request (view, { pushed ; treated ; completed }, Some errors)  ->
          Format.fprintf ppf
            "@[<v 0>%a@,Pushed: %a, Treated: %a, Failed: %a@,Errors: %a@]"
            Request.pp view
            Time.pp_hum pushed Time.pp_hum treated Time.pp_hum completed
            (Format.pp_print_list Error_monad.pp) errors
  end

  (* parsed operations' cache. used for memoization *)
  module ParsedCache = WeakRingTable.Make(struct
      type t = Operation.t
      let equal = Operation.equal
      (* TODO: we can't use [Operation_hash] hashing because [WeakRingTable]
         requires a [t -> int] hashing function. *)
      let hash = Hashtbl.hash
    end)

  (* validated operations' cache. used for memoization *)
  module ValidatedCache = struct

    type t = (result * Operation.t) Operation_hash.Table.t

    let encoding =
      let open Data_encoding in
      Operation_hash.Table.encoding (
        tup2
          result_encoding
          (dynamic_size Operation.encoding)
      )

    let pp break ppf table =
      let open Format in
      Operation_hash.Table.iter
        (fun h (r, _) ->
           fprintf ppf "Operation %a: %a"
             Operation_hash.pp_short h
             pp_result r;
           break ppf
        )
        table

    let create () = Operation_hash.Table.create 1000

    let add t parsed_op result =
      Operation_hash.Table.replace t parsed_op.hash result

    let find_opt t parsed_op =
      Operation_hash.Table.find_opt t parsed_op.hash

    let iter f t =
      Operation_hash.Table.iter f t

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
        (fun hash (result,raw_op) acc ->
           let proto_op = map_op raw_op in
           match result with
           | Applied _ -> {
               acc with
               Proto_services.Mempool.applied =
                 (hash, proto_op)::acc.Proto_services.Mempool.applied
             }
           | Branch_refused err -> {
               acc with
               Proto_services.Mempool.branch_refused =
                 Operation_hash.Map.add
                   hash
                   (proto_op,err)
                   acc.Proto_services.Mempool.branch_refused
             }
           | Branch_delayed err -> {
               acc with
               Proto_services.Mempool.branch_delayed =
                 Operation_hash.Map.add
                   hash
                   (proto_op,err)
                   acc.Proto_services.Mempool.branch_delayed
             }
           | Refused err -> {
               acc with
               Proto_services.Mempool.refused =
                 Operation_hash.Map.add
                   hash
                   (proto_op,err)
                   acc.Proto_services.Mempool.refused
             }
           | Refused_by_pre_filter | Refused_by_post_filter -> {
               acc with
               Proto_services.Mempool.refused =
                 Operation_hash.Map.add
                   hash
                   (proto_op,[])
                   acc.Proto_services.Mempool.refused
             }
           | _ -> acc
        ) t empty

    let clear t = Operation_hash.Table.clear t

  end

  module Types = struct

    type parameters = {
      limits : limits ;
      chain : Mempool_helpers.chain ;
      head_info : Mempool_helpers.head_info ;
      validation_state : Proto.validation_state ;
      filter_config : Mempool_filters.config ;
      mempool_advertiser : Mempool_advertiser.t ;
    }

    (* internal worker state *)
    type state =
      {
        (* state of the validator. this is updated at each apply_operation *)
        mutable validation_state : Proto.validation_state ;

        cache : ValidatedCache.t ;

        mutable filter_config : Mempool_filters.config ;

        (* live blocks and operations, initialized at worker launch *)
        live_blocks : Block_hash.Set.t ;
        live_operations : Operation_hash.Set.t ;

        current_head : State.Block.t ;
        chain : Mempool_helpers.chain ;

        mempool_advertiser : Mempool_advertiser.t ;

        operation_stream: (
          result *
          Operation.shell_header *
          Proto.operation_data
        ) Lwt_watcher.input;

      }

    type view = {
      cache : ValidatedCache.t ;
      filter_config : Mempool_filters.config ;
    }

    let view (state : state) _ : view =
      { cache = state.cache ; filter_config = state.filter_config }

    let encoding =
      let open Data_encoding in
      conv
        (fun { cache ; filter_config } -> (cache, filter_config))
        (fun (cache, filter_config) -> { cache ; filter_config })
        (obj2
           (req "cache" ValidatedCache.encoding)
           (req "filter_config" Mempool_filters.config_encoding))

    let pp ppf { cache } =
      ValidatedCache.pp
        (fun ppf ->
           Format.pp_print_string ppf ";";
           Format.pp_print_space ppf ())
        ppf
        cache

  end

  module Worker = Worker.Make (Name) (Event) (Request) (Types)

  open Types

  type t = Worker.infinite Worker.queue Worker.t

  let parsed_cache = ParsedCache.create Static.max_size_parsed_cache

  let debug w =
    Format.kasprintf (fun msg -> Worker.record_event w (Debug msg))

  let shutdown w =
    Worker.shutdown w

  (*** prevalidation ****)
  open Validation_errors

  let create
      ?protocol_data
      ~(chain:Mempool_helpers.chain)
      ~(head_info:Mempool_helpers.head_info)
      ~timestamp
      () =
    State.Block.context head_info.current_head >>= fun predecessor_context ->
    Context.reset_test_chain
      predecessor_context head_info.current_head_hash
      timestamp >>= fun predecessor_context ->
    Context.reset_test_chain
      predecessor_context head_info.current_head_hash
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
      ~chain_id:chain.id
      ~predecessor_context
      ~predecessor_timestamp:head_info.current_head_header.shell.timestamp
      ~predecessor_fitness:head_info.current_head_header.shell.fitness
      ~predecessor_level:head_info.current_head_header.shell.level
      ~predecessor:head_info.current_head_hash
      ~timestamp
      ?protocol_data
      ()

  let apply_operation state op =
    if Operation_hash.Set.mem op.hash state.live_operations then
      Lwt.return (None, Duplicate)
    else if not (Block_hash.Set.mem op.raw.Operation.shell.branch state.live_blocks) then
      Lwt.return (None,Not_in_branch)
    else
      match Mempool_filters.pre_filter state.filter_config op.protocol_data with
      | false -> Lwt.return (None, Refused_by_pre_filter)
      | true ->
          Proto.apply_operation state.validation_state
            { shell = op.raw.shell ; protocol_data = op.protocol_data } >>= function
          | Error errors ->
              Lwt.return
                (None,
                 match classify_errors errors with
                 | `Branch -> Branch_refused errors
                 | `Permanent -> Refused errors
                 | `Temporary -> Branch_delayed errors)
          | Ok (validation_state, receipt) ->
              Mempool_filters.post_filter
                state.filter_config
                ~validation_state_before:state.validation_state
                ~validation_state_after:validation_state
                (op.protocol_data, receipt) >>= function
              | false -> Lwt.return (None, Refused_by_post_filter)
              | true -> Lwt.return (Some validation_state, Applied receipt)

  (*** end prevalidation ***)

  let parse_helper raw_op =
    let hash = Operation.hash raw_op in
    let size = Data_encoding.Binary.length Operation.encoding raw_op in
    if size > Proto.max_operation_data_length then
      error (Oversized_operation
               { size ; max = Proto.max_operation_data_length })
    else
      match Data_encoding.Binary.of_bytes
              Proto.operation_data_encoding
              raw_op.Operation.proto with
      | None -> error Parse_error
      | Some protocol_data ->
          ok { hash ; raw = raw_op ; protocol_data }

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
    match ValidatedCache.find_opt state.cache parsed_op with
    | None | Some ((Branch_delayed _),_) ->
        validate_helper w parsed_op >>= fun result ->
        ValidatedCache.add state.cache parsed_op (result, parsed_op.raw);
        (* operations are notified only the first time *)
        notify_helper w result parsed_op.raw ;
        Lwt.return result
    | Some (result,_) -> Lwt.return result

  (* worker's handlers *)
  let on_request :
    type r. t -> r Request.t -> r tzresult Lwt.t = fun w request ->
    match request with
    | Request.Validate parsed_op -> on_validate w parsed_op >>= return

  let on_launch (_ : t) (_ : Name.t) (parameters : Types.parameters) =
    return {
      chain = parameters.chain ;
      validation_state = parameters.validation_state ;
      cache = ValidatedCache.create () ;
      filter_config = parameters.filter_config ;
      live_blocks = parameters.head_info.live_blocks ;
      live_operations = parameters.head_info.live_operations ;
      current_head = parameters.head_info.current_head ;
      mempool_advertiser = parameters.mempool_advertiser ;
      operation_stream = Lwt_watcher.create_input () ;
    }

  let on_close w =
    let state = Worker.state w in
    Lwt_watcher.shutdown_input state.operation_stream;
    ValidatedCache.iter (fun hash _ ->
        Distributed_db.Operation.clear_or_cancel state.chain.db hash)
      state.cache ;
    ValidatedCache.clear state.cache;
    Lwt.return_unit

  let on_error w r st errs =
    Worker.record_event w (Event.Request (r, st, Some errs)) ;
    Lwt.return (Error errs)

  let on_completion
    : type r. t -> r Request.t -> r -> Worker_types.request_status -> unit Lwt.t
    = fun w req res st ->
      Worker.record_event w (Event.Request (Request.view req, st, None)) ;
      let state = Worker.state w in
      match req with
      | Request.Validate op -> (* for typing the res *)
          match res with
          | Applied _ ->
              Mempool_advertiser.applied
                state.mempool_advertiser
                op.hash
                { shell = op.raw.shell ; protocol_data = op.protocol_data }
          | Branch_delayed _ ->
              Mempool_advertiser.branch_delayed
                state.mempool_advertiser
                op.hash
                { shell = op.raw.shell ; protocol_data = op.protocol_data }
          | Branch_refused _
          | Refused _
          | Refused_by_pre_filter | Refused_by_post_filter
          | Duplicate | Not_in_branch ->
              Lwt.return_unit

  let table = Worker.create_table Queue

  let create limits
      (chain: Mempool_helpers.chain)
      head_info
      filter_config
      mempool_advertiser  =
    let module Handlers = struct
      type self = t
      let on_launch = on_launch
      let on_close = on_close
      let on_error = on_error
      let on_completion = on_completion
      let on_no_request _ = return_unit
      let on_request = on_request
    end in
    let timestamp = Time.now () in
    create ~chain ~head_info ~timestamp () >>=? fun validation_state ->
    Worker.launch
      table
      limits.worker_limits
      chain.Mempool_helpers.id
      { limits ;
        chain ; head_info ;
        validation_state ;
        filter_config ;
        mempool_advertiser }
      (module Handlers)

  (* Exporting functions *)

  let validate t parsed_op =
    Worker.push_request_and_wait t (Request.Validate parsed_op)

  let update_filter_config t filter_config =
    let state = Worker.state t in
    state.filter_config <- filter_config

  let filter_config t =
    let state = Worker.state t in
    state.filter_config

  (* atomic parse + memoization *)
  let parse raw_op =
    begin match ParsedCache.find_opt parsed_cache raw_op with
      | None ->
          let parsed_op = parse_helper raw_op in
          ParsedCache.add parsed_cache raw_op parsed_op;
          parsed_op
      | Some parsed_op -> parsed_op
    end

  let chain t =
    let state = Worker.state t in
    state.chain

  let fitness t =
    let state = Worker.state t in
    Proto.finalize_block state.validation_state >>=? fun (block_result, _) ->
    return block_result.fitness

  let status t = Worker.status t

  let head t =
    let state = Worker.state t in
    state.current_head

  let pending_operations : t -> Proto_services.Mempool.t = fun t ->
    let state = Worker.state t in
    ValidatedCache.to_mempool state.cache

  let pending_rpc_directory : t RPC_directory.t =
    RPC_directory.gen_register
      RPC_directory.empty
      (Proto_services.S.Mempool.pending_operations RPC_path.open_root)
      (fun w () () ->
         let state = Worker.state w in
         RPC_answer.return (ValidatedCache.to_mempool state.cache)
      )

  let monitor_operations = fun t params ->
    let state = Worker.state t in
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
    ( next, shutdown )

  let rpc_directory : t RPC_directory.t =
    RPC_directory.empty |> fun dir ->
    RPC_directory.gen_register dir
      (Proto_services.S.Mempool.pending_operations RPC_path.open_root)
      (fun t () () ->
         let mempool_t = pending_operations t in
         RPC_answer.return mempool_t) |> fun dir ->
    RPC_directory.gen_register dir
      (Proto_services.S.Mempool.monitor_operations RPC_path.open_root)
      (fun t param () ->
         let next,shutdown = monitor_operations t param in
         RPC_answer.return_stream { next ; shutdown }) ;

end
