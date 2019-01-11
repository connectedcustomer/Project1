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
  module Gossip : Proto_plugin.GOSSIP with module Proto = Proto

  type t

  (** Creates/tear-down a new mempool advertiser. *)
  val create :
    limits ->
    Mempool_helpers.chain ->
    Mempool_helpers.head_info ->
    Gossip.config ->
    t tzresult Lwt.t
  val shutdown : t -> unit Lwt.t

  (** add the given operation to the internal state for later advertisement *)
  val applied : t -> Operation_hash.t -> Proto.operation -> unit Lwt.t
  val branch_delayed : t -> Operation_hash.t -> Proto.operation -> unit Lwt.t

  (** advertised the operations in the internal state and clear it *)
  val advertise : t -> unit Lwt.t

  (** introspection *)
  val status : t -> Worker_types.worker_status

  val update_gossip_config : t -> Gossip.config -> unit
  val gossip_config : t -> Gossip.config


end

module Make
    (Proto : Registered_protocol.T)
    (Gossip : Proto_plugin.GOSSIP with module Proto = Proto)
  : T
    with module Proto = Proto
     and module Gossip = Gossip
= struct

  module Proto = Proto
  module Gossip = Gossip

  module Log = Tezos_stdlib.Logging.Make(struct
      let name = "node.mempool_advertiser"
    end)

  module Name = struct
    type t = Block_hash.t
    let encoding = Block_hash.encoding
    let base = [ "node" ; "mempool" ; "advertiser" ]
    let pp = Block_hash.pp_short
  end

  module Request = struct

    type 'a t =
      | Include_applied : (Operation_hash.t * Proto.operation) -> unit t
      | Include_branch_delayed : (Operation_hash.t * Proto.operation) -> unit t
      | Advertise : unit t

    type view =
      | View_include_applied of Operation_hash.t
      | View_include_branch_delayed of Operation_hash.t
      | View_advertise

    let view
      : type a. a t -> view
      = function
        | Include_applied (op_hash, _) -> View_include_applied op_hash
        | Include_branch_delayed (op_hash, _) -> View_include_branch_delayed op_hash
        | Advertise -> View_advertise

    let encoding =
      let open Data_encoding in
      union
        [ case (Tag 0)
            ~title:"Include_applied"
            (obj1 (req "op_hash" Operation_hash.encoding))
            (function View_include_applied op_hash -> Some op_hash | _ -> None)
            (fun op_hash -> View_include_applied op_hash) ;
          case (Tag 1)
            ~title:"Include_branch_delayed"
            (obj1 (req "op_hash" Operation_hash.encoding))
            (function View_include_branch_delayed op_hash -> Some op_hash | _ -> None)
            (fun op_hash -> View_include_branch_delayed op_hash) ;
          case (Tag 2)
            ~title:"Advertise"
            unit
            (function View_advertise -> Some () | _ -> None)
            (fun () -> View_advertise) ]

    let pp ppf = function
      | View_include_applied op_hash ->
          Format.fprintf ppf "Applied %a" Operation_hash.pp_short op_hash
      | View_include_branch_delayed op_hash ->
          Format.fprintf ppf "Branch delayed %a" Operation_hash.pp_short op_hash
      | View_advertise ->
          Format.fprintf ppf "Advertise"

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

  module Types = struct

    type parameters = {
      limits : limits ;
      chain_db : Distributed_db.chain_db ;
      head : State.Block.t ;
      min_wait : float ;
      gossip_config : Gossip.config ;
    }

    (* internal worker state *)
    type state =
      {
        (* We maintain the mempool with a reversed `known_valid` field so that
           insertion is faster. We do a single `List.rev` just before sending
           the advertisement. *)
        mutable rev_mempool : Mempool.t ;
        (* When an advertisement request is received, if the promise of the
           throttle field is `Return`, then a new promise is created (it sleeps
           for a tiny bit, and then advertises the mempool). This promise is
           stored in the `throttle` field. This limits the rate of
           advertisements. *)
        mutable throttle : unit Lwt.t ;
        mutable gossip_config : Gossip.config ;
        parameters : parameters ;
      }

    type view = Mempool.t

    let view { rev_mempool } _ = rev_mempool

    let encoding = Mempool.encoding

    let pp ppf { Mempool.known_valid ; pending } =
      Format.fprintf ppf
        "knwon valid (%d) %a; pending (%d) %a"
        (List.length known_valid)
        (Format.pp_print_list Operation_hash.pp_short) known_valid
        (Operation_hash.Set.cardinal pending)
        (Format.pp_print_list Operation_hash.pp_short)
        (Operation_hash.Set.elements pending)

  end

  module Worker = Worker.Make (Name) (Event) (Request) (Types)
  let table = Worker.create_table Queue
  type t = Worker.infinite Worker.queue Worker.t

  let send_advertisement (state : Types.state) =
    if Mempool.is_empty state.rev_mempool then
      ()
    else
      let mempool =
        { state.rev_mempool with
          known_valid = List.rev state.rev_mempool.known_valid } in
      state.rev_mempool <- Mempool.empty ;
      Distributed_db.Advertise.current_head
        state.parameters.chain_db
        ~mempool
        state.parameters.head

  let handle_request
    : type r. t -> r Request.t -> r tzresult
    = fun w req ->
      let state = Worker.state w in
      match req with
      | Request.Include_applied (op_hash, _op) ->
          state.rev_mempool <-
            { state.rev_mempool with
              known_valid = op_hash :: state.rev_mempool.known_valid } ;
          Ok ()
      | Request.Include_branch_delayed (op_hash, op) ->
          begin if Gossip.gossip_branch_delayed state.gossip_config op then
              state.rev_mempool <-
                { state.rev_mempool with
                  pending = Operation_hash.Set.add op_hash state.rev_mempool.pending }
          end ;
          Ok ()
      | Request.Advertise ->
          match Lwt.state state.throttle with
          | Lwt.Fail Lwt.Canceled -> Ok ()
          | Lwt.Fail exc -> Error [ Exn exc ]
          | Lwt.Sleep -> Ok ()
          | Lwt.Return () ->
              let p =
                Lwt_unix.sleep state.parameters.min_wait >>= fun () ->
                send_advertisement state ;
                Lwt.return_unit in
              state.throttle <- p ;
              Ok ()

  let handle_request
    : type r. t -> r Request.t -> r tzresult Lwt.t
    = fun w req ->
      (* NOTE: This additional layer serves to show that requests are treated
         in a non-blocking way. It makes it easier to reason about the queue of
         requests and the possible delay between a request for advertisement and
         the request being fulfilled (or, more precisely, being sent to the
         Distributed_db): there is no important delay. *)
      (* FIXME? Do we need the whole worker boilerplate if we have instant
         requests? *)
      Lwt.return (handle_request w req)

  module Handlers = struct
    type self = t
    let on_launch _ _name (parameters : Types.parameters) =
      return { Types.
               rev_mempool = Mempool.empty ;
               throttle = Lwt.return_unit ;
               gossip_config = parameters.gossip_config ;
               parameters ;
             }
    let on_request t req = handle_request t req
    let on_no_request _ = return_unit
    let on_close t =
      let state = Worker.state t in
      Lwt.cancel state.throttle ;
      send_advertisement state ;
      Lwt.return_unit
    let on_error _t _view _status _errs =
      return_unit
    let on_completion _t _req _res _status =
      Lwt.return_unit
  end

  let create limits
      (chain: Mempool_helpers.chain)
      (head_info: Mempool_helpers.head_info)
      gossip_config
    =
    let min_wait = limits.minimum_wait_between_advertisments in
    Worker.launch
      table
      limits.worker_limits
      head_info.current_head_hash
      { limits ;
        chain_db = chain.db ;
        head = head_info.current_head ;
        min_wait ;
        gossip_config }
      (module Handlers)

  let shutdown w =
    Worker.shutdown w

  let applied t op_hash op =
    Worker.push_request t (Include_applied (op_hash, op))
  let branch_delayed t op_hash op =
    Worker.push_request t (Include_branch_delayed (op_hash, op))
  let advertise t =
    Worker.push_request t Advertise

  let status w = Worker.status w

  let update_gossip_config t gossip_config =
    let state = Worker.state t in
    state.gossip_config <- gossip_config
  let gossip_config t =
    let state = Worker.state t in
    state.gossip_config

end
