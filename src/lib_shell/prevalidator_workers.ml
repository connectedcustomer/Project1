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

(* The core of the prevalidator: it defines the functions that are actually used
 * to prevalidate operations.
 *
 * The responsibilities are distributed as follows:
 * - The _instantiator_ (currently only [chain_validator] is responsible for
 * creating a prevalidator context and sending it information about new
 * operations and new blocks.
 * - [prevalidator] dispatches the information sent by the instantiator to the
 * correct worker (see [prevalidator_workers]) and maintains some bits of the
 * state.
 * - [prevalidator_workers] provides a pool of worker (that is instantiated by
 * the [prevalidator]). *)


(* # HELPERS FOR STATE MANAGEMENT *)




(* # IMPLEMENTATION *)

module Make (S: Prevalidator_sigs.PROTO_AND_SUCH)
  : Prevalidator_sigs.PREVALIDATOR_CONTEXT with module Proto = S.Proto
= struct

  (* ## Some repeats from the argument *)
  module Proto = S.Proto
  module Filter = S.Filter
  let limits = S.limits

  (* ## Configuration hooks *)
  let filter_config = ref Filter.default_config

  (* ## Managing shared-state prevalidation context *)
  module Preval:
  sig
    include Prevalidation.T with module Proto = Proto
    val shared_preval_errors: unit tzresult Lwt.t
    val get_shared: unit -> t Lwt.t
    val hard_reset_shared: State.Block.t -> unit tzresult Lwt.t
    val apply_shared:
      operation ->
      (Filter.config -> Proto.operation_data * Proto.operation_receipt -> bool) ->
      result option Lwt.t
  end
  =  struct
    include Prevalidation.Make(Proto)
    let shared_preval: t ref tzresult Lwt.t =
      State.read_chain_data
        S.chain_state
        (fun _ { current_head } -> Lwt.return current_head) >>= fun block ->
      start
        ~predecessor:block
        ~timestamp:(Time.now ())
        () >>= fun t ->
      Lwt.return (record_trace (Prevalidator_sigs.Initialization_error) t) >>=? fun t ->
      return (ref t)
    let shared_preval_errors = shared_preval >>=? fun _ -> return_unit
    let get_shared () =
      shared_preval >>= function
      | Ok r -> Lwt.return !r
      | Error err -> raise (Prevalidator_sigs.Uninitialized err)
    let hard_reset_shared block =
      shared_preval >>=? fun r ->
      start
        ~predecessor:block
        ~timestamp:(Time.now ())
        () >>=? fun t ->
      r := t;
      return_unit

    (* soft increment: when one of the worker adds an operation to the
     * Prevalidation context.
     *
     * NOTE: These increments need to happen in sequence. In particular, if two
     * workers where to call Preval.apply_operation concurrently both would obtain
     * irreconciliables Prevalidation contexts.
     * Moreover, Preval.apply_operation returns an Lwt promise.
     *
     * Consequently, we provide [apply_shared] below so that workers can
     * sequentialize their increments. *)

    (* [soft_incrementer] is a sequentializer for calls to apply_operation *)
    let soft_incrementer: Lwt_mutex.t = Lwt_mutex.create ()

    (* [apply op] waits in line for other calls to apply_operation to
     * resolve. Then it applies the provided operation and, if appropriate,
     * updates the shared prevalidation context before resolving to the
     * prevalidation result. *)
    let apply_shared op filter =
      shared_preval >>= function
      | Ok r ->
          (* Returns None if refused by the filter, some otherwise *)
          Lwt_mutex.with_lock soft_incrementer (fun () ->
              let preval = !r in
              apply_operation preval op >>= function
              | Applied (preval, _, op_receipt) as res ->
                  if filter !filter_config (op.protocol_data, op_receipt) then begin
                    r := preval;
                    Lwt.return_some res
                  end else begin
                    Lwt.return_none
                  end
              | res ->
                  Lwt.return_some res
            )
      | Error errs ->
          (* NOTE: this happens when there is an initialisation error for the
           * prevalidation context. *)
          raise (Prevalidator_sigs.Uninitialized errs)

  end

  (* We monitor Preval initialisation errors and propagate them to the functor
   * caller. *)
  let init_errors = Preval.shared_preval_errors



  let get_mempool () =
    Preval.get_shared () >>= fun preval ->
    Preval.finalize preval >>=? fun (_, ops_and_receipt, _) ->
    let known_valid = List.map (fun ({ Preval.hash }, _) -> hash) ops_and_receipt in
    let pending =
      (* TODO[FUTURE]: maintain a set of `pending` ops *)
      Operation_hash.Set.empty in
    return { Mempool.known_valid; pending }

  let preapplied_operations, preapplied_operations_source =
    let s, p = Lwt_stream.create () in
    ref s, ref p
  let refresh_streams () =
    let s, p = Lwt_stream.create () in
    preapplied_operations := s;
    preapplied_operations_source := p
  let record_preapllication x = !preapplied_operations_source (Some x)
  let preapplied_operations () = !preapplied_operations


  (* ## Types to communicate with the workers *)
  type inclusion_result =
    | Cannot_download of error list
    | Cannot_parse of error list
    | Refused_by_prefilter
    | Refused_by_postfilter
    | Prevalidation_result of Preval.result
  type _ request =
    | Req_mempool: Mempool.t -> (Operation_hash.t * inclusion_result) list request

  module Pipeline = struct
    (* A pipeline is a set of buffers that a worker uses to treat a single
     * request. There is one buffer for the initial hashes carried by the
     * request. There are buffers for each of the blocking steps in the
     * prevalidation process. *)
    type t = {
      received: Operation_hash.t Queue.t;
      downloading: (Operation_hash.t * Operation.t tzresult Lwt.t) Queue.t;
      applying: (Preval.operation * Preval.result option Lwt.t) Queue.t;
      mutable results: inclusion_result Operation_hash.Map.t
    }

    let is_empty t =
      Queue.is_empty t.received &&
      Queue.is_empty t.downloading &&
      Queue.is_empty t.applying

    let select t =
      (* A `select`-like function to wait on any of the pipeline's buffers'
       * heads to resolve *)
      assert (not (Queue.is_empty t.downloading && Queue.is_empty t.applying));
      let first_task_or_never q =
        if Queue.is_empty q then
          Lwt_utils.never_ending
        else
          snd (Queue.peek q) >>= fun _ -> Lwt.return_unit
      in
      Lwt.choose (
        (first_task_or_never t.downloading) ::
        (first_task_or_never t.applying) ::
        []
      )

    let init op_hashes =
      let received = Queue.create () in
      List.iter (fun op_hash -> Queue.push op_hash received) op_hashes;
      {
        received;
        downloading = Queue.create ();
        applying = Queue.create ();
        results = Operation_hash.Map.empty;
      }

  end

  type state = {
    mempool_requests: (Operation_hash.t * inclusion_result) list request Queue.t;
    mutable work_traffic_light: Lwt_mutex.t;
    mutable pipeline: Pipeline.t option; (* reset between each task *)
  }
  let init_state () = {
    mempool_requests = Queue.create ();
    work_traffic_light = Lwt_mutex.create ();
    pipeline = None;
  }
  type parameters = {
    initial_op_hashes: Operation_hash.t list;
  }

  (* ## Boilerplate to set up worker pool *)

  module Name = struct
    let base = [ "shell"; "prevalidator" ]
    include P2p_peer.Id (* type t, val encoding, val pp *)
  end
  module Request = struct
    type 'a t = 'a request
    type view = Prevalidator_sigs.req_view
    let view: type a. a t -> view = function
      | Req_mempool m -> Prevalidator_sigs.View m
    let encoding = Prevalidator_sigs.req_view_encoding
    let pp ppf = fun (Prevalidator_sigs.View mempool) ->
      Data_encoding.Json.pp ppf
        (Data_encoding.Json.construct Mempool.encoding mempool)

  end
  module Types = struct
    type nonrec state = state
    type nonrec parameters = parameters
    type view = Prevalidator_sigs.state_view
    let view _ _ = ()
    let encoding = Prevalidator_sigs.state_view_encoding
    let pp ppf v =
      Data_encoding.Json.pp ppf
        (Data_encoding.Json.construct encoding v)
  end


  (* ## The worker pool *)
  module type WORKERS =
    Worker.WORKER
    with type name_t := Name.t
     and type event_t := Prevalidator_sigs.Event.t
     and type 'a request_t := 'a request
     and type request_view := Prevalidator_sigs.req_view
     and type types_state := state
     and type types_parameters := parameters
     and type types_view := Types.view
  module Worker_pool: WORKERS = Worker.Make (Name) (Prevalidator_sigs.Event) (Request) (Types)
  type worker = Worker_pool.bounded Worker_pool.queue Worker_pool.t
  module Peer_level_registry:
    Registry.S
    with type k = P2p_peer.Id.t
     and type v = worker
    = Registry.Make(
    struct
      include P2p_peer.Id
      type v = worker
    end)




  (* ## One final bit of shared state *)
  module Op_result_cache :
    Registry.S
    with type k = Operation_hash.t
     and type v = inclusion_result
    = Registry.Make(
    struct
      include Operation_hash
      type v = inclusion_result
    end)

  type kind_operation_input = [
    | `Applied
    | `Branch_delayed
  ]
  type new_operation_input =
    (kind_operation_input * Operation.shell_header * Proto.operation_data) Lwt_watcher.input

  let new_operation_input : new_operation_input = Lwt_watcher.create_input ()

  (* rpc directory for the worker pool *)
  let rpc_directory =
    let module Proto_services = Block_services.Make(Proto)(Proto) in
    let dir : unit RPC_directory.t ref = ref RPC_directory.empty in
    let register s f = dir := RPC_directory.gen_register !dir s f in
    register
      (Proto_services.S.Mempool.monitor_operations RPC_path.open_root)
      begin fun () params () ->
        let operation_stream, stopper = Lwt_watcher.create_stream new_operation_input in
        (* Convert ops from Op_result_cache to a list of operations to be streamed *)
        let map_op op =
          let protocol_data =
            Data_encoding.Binary.of_bytes_exn
              Proto.operation_data_encoding
              op.Operation.proto
          in
          Proto.{ shell = op.shell ; protocol_data }
        in
        let fold_op _k (op, _error) acc = map_op op :: acc in
        (* First call : retrieve the current set of op from the mempool *)
        let (applied, branch_delayed) =
          Op_result_cache.fold (fun ophash result (applied,branch_delayed) ->
              match result with
              |Prevalidation_result (Applied (_, op, _)) when params#applied ->
                  let newop = Proto.{ shell = op.raw.shell ;
                                      protocol_data = op.protocol_data }
                  in
                  (newop::applied,branch_delayed)
              |Prevalidation_result (Branch_delayed (op,error)) when params#branch_delayed ->
                  let branch_delayed =
                    Operation_hash.Map.add ophash (op.raw,error) branch_delayed
                  in
                  (applied,branch_delayed)
              | _ -> (applied,branch_delayed)
            ) ([],Operation_hash.Map.empty)
        in
        let branch_delayed =
          if params#branch_delayed then
            Operation_hash.Map.fold fold_op branch_delayed [] else []
        in
        let current_mempool = List.concat [ applied ; branch_delayed ] in
        let current_mempool = ref (Some current_mempool) in
        let filter_result = function
          | `Applied -> params#applied
          | `Branch_delayed -> params#branch_delayed
        in
        let next () =
          (* first time this rpc is called, we return the content of the
             current mempool a.k.a. Op_result_cache, and subsequently we wait
             for a notification on the stream and we return the operation *)
          match !current_mempool with
          | Some mempool -> begin
              current_mempool := None ;
              Lwt.return_some mempool
            end
          | None -> begin
              Lwt_stream.get operation_stream >>= function
              | Some (kind, shell, protocol_data) when filter_result kind ->
                  Lwt.return_some [ { Proto.shell ; protocol_data } ]
              | _ -> Lwt.return_none
            end
        in
        let shutdown () = Lwt_watcher.shutdown stopper in
        let _stream : Proto.operation list RPC_answer.stream = { next ; shutdown } in
        RPC_answer.return_stream { next ; shutdown }
      end ;
    return !dir

  let has_resolved t = match Lwt.state t with
    | Lwt.Return _ | Lwt.Fail _ -> true
    | Lwt.Sleep -> false

  let head_is_resolved q =
    (not (Queue.is_empty q)) && has_resolved (snd (Queue.peek q))

  let record_result worker (pipeline: Pipeline.t) op_hash result =
    begin match result with
      | Prevalidation_result (Applied (_, op, _)) ->
          Worker_pool.record_event worker (Op_done op_hash);
          record_preapllication (op.raw.shell, op.protocol_data)
      | Prevalidation_result Duplicate ->
          Worker_pool.record_event worker (Op_done op_hash)
      | Refused_by_prefilter
      | Refused_by_postfilter
      | Prevalidation_result Outdated ->
          Worker_pool.record_event worker (Drop (op_hash, []))
      | Cannot_download errs
      | Cannot_parse errs
      | Prevalidation_result (Branch_delayed (_,errs))
      | Prevalidation_result (Branch_refused errs)
      | Prevalidation_result (Refused errs) ->
          Worker_pool.record_event worker (Drop (op_hash, errs))
    end;
    begin match result with
      | Prevalidation_result Duplicate -> begin
          match Op_result_cache.query op_hash with
          | Some (Prevalidation_result (Applied _)) ->
              (* Don't overwrite the application with a duplicate *)
              ()
          | Some _ | None -> Op_result_cache.register op_hash result
        end
      | _ -> Op_result_cache.register op_hash result
    end;
    pipeline.results <- Operation_hash.Map.add op_hash result pipeline.results

  (* notify the RPC stream that a new operation was received *)
  let notify_new_operation_input op = function
    |Preval.Branch_delayed _ ->
        Lwt_watcher.notify new_operation_input
          (`Branch_delayed,op.Preval.raw.shell,op.protocol_data);
    |Applied _ ->
        Lwt_watcher.notify new_operation_input
          (`Applied,op.raw.shell,op.protocol_data);
    | _ -> () (* we do not notify operations with permanent errors *)

  (* ### the main loop: take a task from a buffer, do it, repeat *)
  let rec work self state =
    (* Going through each buffer one by one. The function is careful to
     * preserve order within the pipeline. Thus, even if a later operation is
     * much faster to, say, download than an earlier one, the later will stay
     * "behind" in the pipeline: it will be parsed, validated, filtered, etc.
     * later. *)

    assert (state.pipeline <> None);
    let pipeline: Pipeline.t = Option.unopt_exn (Assert_failure (__FILE__, __LINE__, 0) ) state.pipeline in


    if head_is_resolved pipeline.applying then
      let (op, p) = Queue.pop pipeline.applying in
      p >>= function
      | Some preval_res ->
          record_result self pipeline op.hash (Prevalidation_result preval_res);
          notify_new_operation_input op preval_res ;
          work self state
      | None ->
          record_result self pipeline op.hash Refused_by_postfilter;
          work self state

    else if head_is_resolved pipeline.downloading then
      let (op_hash, p) = Queue.pop pipeline.downloading in
      p >>= function
      | Error errs ->
          record_result self pipeline op_hash (Cannot_download errs);
          work self state
      | Ok op_t ->
          match Preval.parse op_t with
          | Error errs ->
              record_result self pipeline op_hash (Cannot_parse errs);
              work self state
          | Ok op ->
              if Filter.pre_filter !filter_config op.protocol_data then begin
                Worker_pool.record_event self (Apply_start op_hash);
                let p =
                  Preval.apply_shared op Filter.post_filter >>= fun preval_res ->
                  Worker_pool.record_event self (Apply_end op_hash);
                  Lwt.return preval_res
                in
                (* notify the RPC stream that a new operation was received *)
                Lwt_watcher.notify new_operation_input (`Applied,op_t.shell,op.protocol_data);
                Queue.push (op, p) pipeline.applying;
                work self state
              end else begin
                record_result self pipeline op.hash Refused_by_prefilter;
                work self state
              end

    else if (not (Queue.is_empty pipeline.received)) then
      let op_hash = Queue.pop pipeline.received in
      match Op_result_cache.query op_hash with
      (* first time we see this hash *)
      | None
      (* failed previously, but maybe it'll work now *)
      | Some (Cannot_download _) ->
          (* TODO[?] should we specify the current peer for fetching? *)
          let fetching = Distributed_db.Operation.fetch S.chain_db op_hash () in
          let p =
            fetching >>= fun op_result ->
            Worker_pool.record_event self (Download_end op_hash);
            Lwt.return op_result in
          Worker_pool.record_event self (Download_start op_hash);
          Queue.push (op_hash, p) pipeline.downloading;
          work self state
      | Some (Prevalidation_result (Branch_delayed (op, _))) ->
          (* TODO[OPTIMISATION]: bypass parsing (put `op` in the pipeline) *)
          Queue.push (op.hash, Lwt.return (Ok op.raw)) pipeline.downloading;
          work self state
      | Some ((Cannot_parse _
              | Refused_by_prefilter
              | Refused_by_postfilter
              | Prevalidation_result (Branch_refused _) (* FIXME? does this belong here? *)
              | Prevalidation_result (Refused _)
              | Prevalidation_result Duplicate
              | Prevalidation_result Outdated
              ) as repeatable_result) ->
          record_result self pipeline op_hash repeatable_result;
          work self state
      | Some (Prevalidation_result (Applied _)) ->
          record_result self pipeline op_hash (Prevalidation_result Duplicate);
          work self state

    else if Pipeline.is_empty pipeline then begin
      (* every buffer is empty, we're done *)
      Worker_pool.record_event self Batch_done;
      Lwt.return pipeline.results
    end

    else
      (* There are some pending operations, we need to wait on them *)
      Pipeline.select pipeline >>= fun () ->
      work self state

  let work worker hashes =
    (* wrapped up version to set the proper transient state *)
    let state = Worker_pool.state worker in
    Lwt_mutex.with_lock state.work_traffic_light (fun () ->
        let pipeline = Pipeline.init hashes in
        assert (state.pipeline = None);
        state.pipeline <- Some pipeline;
        work worker state >>= fun results ->
        state.pipeline <- None;
        Lwt.return results
      )


  (* ## The work that workers do *)
  module Handlers: Worker_pool.HANDLERS with type self = worker
  = struct
    type self = worker

    let on_request : type a . worker -> a request -> a tzresult Lwt.t
      = fun worker (Req_mempool mempool) ->
        work worker (Mempool.all_hashes mempool) >>= fun results ->
        return (Operation_hash.Map.bindings results)

    (* ### additional handlers for other events *)

    let on_launch worker _name { initial_op_hashes } =
      let state = init_state () in
      begin
        if initial_op_hashes = []
        then
          ()
        else begin
          let (_: inclusion_result Operation_hash.Map.t Lwt.t) =
            work worker initial_op_hashes in
          ()
        end
      end;
      Lwt.return state

    let on_no_request self =
      (* We use the `on_no_request` to clean up peer workers that have not
       * received any requests for a while (this can mean the peer is
       * disconected, having issues, not very active, etc.)
       * Launching another worker if the same peer contacts us later is
       * relatively cheap so it is not a problem to shutdown idle ones. *)
      (* TODO[FUTURE] replace this with the same as the system from peer_validator *)
      Worker_pool.trigger_shutdown self;
      return_unit

    let on_close worker =
      (* TODO[SCORE]: make sure the score is maintained in the peer metadata *)
      (* TODO[?]: maybe add call to Distributed_db.shutdown_reader to shutdown DDB readers *)
      let state = Worker_pool.state worker in
      begin match state.pipeline with
        | Some { downloading } ->
            Queue.iter (fun (_, p) -> Lwt.cancel p) downloading
        | None -> ()
      end;
      Lwt.return_unit

    let on_error _t _view _req_status errs =
      Prevalidator_sigs.Log.lwt_log_error "Error: %a" pp_print_error errs >>= fun () ->
      Lwt.return (Error errs)

    let on_completion _t req _res _req_status =
      Prevalidator_sigs.Log.lwt_debug "Complete: %a" Request.pp (Request.view req) >>= fun () ->
      Lwt.return_unit

  end

  let workers_table = Worker_pool.create_table (Bounded { size = limits.max_queued_operations })

  let launch_and_register_worker peer_id =
    Worker_pool.launch
      workers_table
      limits.worker_limits
      peer_id
      { initial_op_hashes = [] }
      (module Handlers: Worker_pool.HANDLERS
        with type self = worker) >>= fun worker ->
    Peer_level_registry.register peer_id worker;
    Lwt.return worker

  let iter_worker f =
    Lwt_list.iter_p (fun (_, w) -> f w) (Worker_pool.list workers_table)

  let notify_head block_hash =
    State.Block.read S.chain_state block_hash >>= function
    | Ok block ->
        begin
          !preapplied_operations_source None;
          refresh_streams ();
          let wt, wk = Lwt.wait () in
          (* wt and wk are used so that the following steps can happen
           * sequentially:
           * - workers cancel ongoing work and set aside branch-delayed ops
           * - the shared prevalidation context (SPC) is updated
           * - workers attempt to apply the branch-delayed ops on the new SPC *)
          Peer_level_registry.iter_p
            (fun _peer_id worker ->
               let backed_ops: Operation_hash.t list =
                 List.fold_right
                   (fun (_, Prevalidator_sigs.View { known_valid; pending }) acc ->
                      known_valid @ (Operation_hash.Set.elements pending) @ acc)
                   (Worker_pool.pending_requests worker)
                   [] in
               let ongoing_ops =
                 let state = Worker_pool.state worker in
                 match state.pipeline with
                 | None ->
                     (* Nothing to do, the worker was not working *)
                     []
                 | Some { received; downloading; applying; results } ->
                     (* Worker working: cancel and recover ops *)
                     let acc = ref [] in
                     Queue.iter (fun h -> acc := h :: !acc) received;
                     Queue.iter (fun (h, p) -> Lwt.cancel p; acc := h :: !acc) downloading;
                     Queue.iter (fun (o, p) -> Lwt.cancel p; acc := o.Preval.hash :: !acc) applying;
                     Operation_hash.Map.iter
                       (fun h r -> match r with
                          | Prevalidation_result (Preval.Branch_delayed _)
                          | Cannot_download _ ->
                              acc := h :: !acc
                          | Cannot_parse _
                          | Refused_by_prefilter
                          | Refused_by_postfilter
                          | Prevalidation_result
                              (Preval.Applied _
                              | Preval.Branch_refused _
                              | Preval.Refused _
                              | Preval.Duplicate
                              | Preval.Outdated) ->
                              (* TODO[?] should we salvage other results? *)
                              ()
                       )
                       results;
                     !acc in
               let retrying_ops = ongoing_ops @ backed_ops in
               let (_: inclusion_result Operation_hash.Map.t Lwt.t) =
                 wt >>= fun () -> (* Schedule for later *)
                 (* NOTE: we still use `work` because the task is
                  * specific to the worker *)
                 work worker retrying_ops in
               Lwt.return_unit) >>= fun () ->
          Preval.hard_reset_shared block >>= fun t ->
          Lwt.return (record_trace Prevalidator_sigs.Initialization_error t) >>= function
          | Ok () ->
              Lwt.wakeup wk ();
              return_unit
          | Error _ as err ->
              Lwt.wakeup_exn wk Lwt.Canceled;
              iter_worker Worker_pool.shutdown >>= fun () ->
              Lwt.return err
        end
    | Error _ as err ->
        iter_worker Worker_pool.shutdown >>= fun () ->
        Lwt.return err


  let prevalidate_injection op =
    (* NOTE: based on parts of the `work` function *)
    match Preval.parse op with
    | Error errs -> Lwt.return (Cannot_parse errs)
    | Ok op ->
        let tautological_filter = fun _ (_, _) -> true in
        Preval.apply_shared op tautological_filter >>= function
        | None -> assert false (*cannot be refused by tautological post-filter*)
        | Some p -> Lwt.return (Prevalidation_result p)

end




(* # EPILOGUE: Future work *)

(* NOT FOR THIS PRELIMINARY VERSION (let's get something working and then maybe
 * optimise it) *)


(* Cache of ongoing validation work (to avoid two operations being worked on by
 * separate workers in parallel). This requires carefulness. E.g., in the
 * following scenario:
 *
 *   Worker1 starts work on op_a
 *   Worker2 starts work on op_a
 *   Worker2 notices the op is already being worked on
 *   Worker1 finds out the op_a is branch_refused/branch_delayed
 *   Worker2 should still try to work on op_a because additional ops it has
 *   might make it valid
 *   Worker2 should keep the order of processing as received from the peer
 *   This means that Worker2 has to stall some of its own processing until it
 *   gets some progress report on Worker1's work on op_a
 *
 * Also note that there is currently a race where two workers working on the
 * same operation can record a different result in the Op_result_cache.
 * Avoiding double work also avoids this race.
*)


(* Limits to the number of operations being processed by a worker.
 *
 * The idea would be to limit how many operations are *moving* in the pipeline
 * (i.e., operations that are neither in received nor results).
 *
 * The operations in `received` are just hashes (not a lot of memory, shared
 * with the DDB). The operations in `results` are useful (to the baker, to the
 * other workers, etc.). In addition, valid operations in result are represented
 * with additional information that is useful to the baker (and hence worth
 * storing in memory); invalid operations in `results` are represented as a hash
 * and a variant which typically carries only a list of errors (the size of
 * which is less directly controllable by a spammer than the size of the byte
 * buffer).
 *
 * On the other hands, operations that are flying in the pipeline carry more
 * data (the raw byte buffer, the parsed operation, etc.) which a spammer can
 * easily inflate (e.g., an operation publishing a smart contract with a lot of
 * NO-OP). Limiting the number of these operations limits the amount of data a
 * spammer can force us to store.
*)


(* Maintain a set of pending operations
 *
 * Specifically, a mempool (i.e., the structure that the prevalidator is
 * supposed to maintain) is composed of a set of known-valid operations and a
 * set of not-known-to-be-invalid operations. This set for optimistic peers is
 * currently not maintained.
 *
 * To maintain this set of operation, we need to add the hash of any operation
 * that is classified as branch-delayed (and branch-refused? and refused?). We
 * also need to remove any operation that is classified as applied, duplicate,
 * or outdated.
*)


(* Better manage shutdown
 *
 * Use a similar technique as peer_validator to get notified from disconnect.
*)


(* Provide and use meaningful limits
 *
 * The [limits] parameter is mostly ignored. It should be used to guarantee the
 * prevalidator does not use more resources than given. In addition, the limits
 * need to carry more information to limit resource usage in more ways (e.g.,
 * max memory, max concurrent download, etc.
*)


(* Provide RPCs to update the filter configs *)
