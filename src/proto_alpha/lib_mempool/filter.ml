(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Development. <contact@tezcore.com>             *)
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

open Alpha_context

type config =
  { minimal_fees : Alpha_context.Tez.t ;
    minimal_fees_per_gas_unit : Alpha_context.Tez.t ;
    minimal_fees_per_byte : Alpha_context.Tez.t ;
    allow_script_failure : bool }

let config_encoding : config Data_encoding.t =
  let open Data_encoding in
  conv
    (fun { minimal_fees ;
           minimal_fees_per_gas_unit ;
           minimal_fees_per_byte ;
           allow_script_failure } ->
      (minimal_fees,
       minimal_fees_per_gas_unit,
       minimal_fees_per_byte,
       allow_script_failure))
    (fun (minimal_fees,
          minimal_fees_per_gas_unit,
          minimal_fees_per_byte,
          allow_script_failure) ->
      { minimal_fees ;
        minimal_fees_per_gas_unit ;
        minimal_fees_per_byte ;
        allow_script_failure })
    (obj4
       (dft "minimal_fees" Tez.encoding Tez.zero)
       (dft "minimal_fees_per_gas_unit" Tez.encoding Tez.zero)
       (dft "minimal_fees_per_byte" Tez.encoding Tez.zero)
       (dft "allow_script_failure" bool true))

let pp_config ppf config =
  let obj = Data_encoding.Json.construct config_encoding config in
  Data_encoding.Json.pp ppf obj

let default_config =
  { minimal_fees = Tez.zero ;
    minimal_fees_per_gas_unit = Tez.zero ;
    minimal_fees_per_byte = Tez.zero ;
    allow_script_failure = true }

module Proto = Tezos_embedded_protocol_alpha.Registerer.Registered

let rec pre_filter_manager
  : type t. config -> t Kind.manager contents_list -> bool
  = fun config op -> match op with
    | Single (Manager_operation { fee ; gas_limit } as op) ->
        let bytes =
          Data_encoding.Binary.length
            Operation.contents_encoding
            (Contents op) in
        begin match Tez.(fee /? Int64.of_int bytes) with
          | Ok fee_per_byte -> Tez.(fee_per_byte >= config.minimal_fees_per_byte)
          | Error _ -> false
        end
        && begin match Tez.(fee /? Z.to_int64 gas_limit) with
          | Ok fee_per_gas_unit -> Tez.(fee_per_gas_unit >= config.minimal_fees_per_gas_unit)
          | Error _ -> false
          | exception _ -> false
        end
        && Tez.(fee >= config.minimal_fees)
    | Cons (Manager_operation op, rest) ->
        pre_filter_manager config (Single (Manager_operation op))
        && pre_filter_manager config rest

let pre_filter config (Operation_data { contents } : Operation.packed_protocol_data) =
  match contents with
  | Single (Endorsement _) -> true
  | Single (Seed_nonce_revelation _) -> true
  | Single (Double_endorsement_evidence _) -> true
  | Single (Double_baking_evidence _) -> true
  | Single (Activate_account _) -> true
  | Single (Proposals _) -> true
  | Single (Ballot _) -> true
  | Single (Manager_operation _) as op -> pre_filter_manager config op
  | Cons (Manager_operation _, _) as op -> pre_filter_manager config op

open Apply_results

let rec post_filter_manager
  : type t. t Kind.manager contents_result_list -> bool
  = fun op -> match op with
    | Single_result (Manager_operation_result { operation_result }) ->
        begin match operation_result with
          | Applied _ -> true
          | Skipped _ | Failed _ | Backtracked _ -> false
        end
    | Cons_result (Manager_operation_result res, rest) ->
        post_filter_manager (Single_result (Manager_operation_result res))
        && post_filter_manager rest

let post_filter config (_op, receipt) =
  match receipt with
  | No_operation_metadata -> assert false (* only for multipass validator *)
  | Operation_metadata { contents } ->
      if config.allow_script_failure then
        true
      else
        match contents with
        | Single_result (Endorsement_result _) -> true
        | Single_result (Seed_nonce_revelation_result _) -> true
        | Single_result (Double_endorsement_evidence_result _) -> true
        | Single_result (Double_baking_evidence_result _) -> true
        | Single_result (Activate_account_result _) -> true
        | Single_result (Proposals_result) -> true
        | Single_result (Ballot_result) -> true
        | Single_result (Manager_operation_result _) as op -> post_filter_manager op
        | Cons_result (Manager_operation_result _, _) as op -> post_filter_manager op
