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

type config =
  { allow_delayed_endorsements : bool ;
    allow_delayed_voting : bool ;
    allow_other_delayed : bool }

let default_allow_delayed_endorsements = true
let default_allow_delayed_voting = false
let default_allow_other_delayed = false

let config_encoding : config Data_encoding.t =
  let open Data_encoding in
  conv
    (fun { allow_delayed_endorsements ;
           allow_delayed_voting ;
           allow_other_delayed } ->
      (allow_delayed_endorsements,
       allow_delayed_voting,
       allow_other_delayed))
    (fun (allow_delayed_endorsements,
          allow_delayed_voting,
          allow_other_delayed) ->
      { allow_delayed_endorsements ;
        allow_delayed_voting ;
        allow_other_delayed })
    (obj3
       (dft "allow_delayed_endorsements" bool default_allow_delayed_endorsements)
       (dft "allow_delayed_voting" bool default_allow_delayed_voting)
       (dft "allow_other_delayed" bool default_allow_other_delayed))

let pp_config ppf config =
  let obj = Data_encoding.Json.construct config_encoding config in
  Data_encoding.Json.pp ppf obj

let default_config =
  { allow_delayed_endorsements = default_allow_delayed_endorsements ;
    allow_delayed_voting = default_allow_delayed_voting ;
    allow_other_delayed = default_allow_other_delayed }

module Proto = Tezos_embedded_protocol_alpha.Registerer.Registered

let gossip_branch_delayed config op =
  match Proto.acceptable_passes op with
  | [ 0 ] -> config.allow_delayed_endorsements
  | [ 1 ] -> config.allow_delayed_voting
  | _ -> config.allow_other_delayed
