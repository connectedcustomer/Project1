(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let log = Logging.Client.Sign.lwt_log_notice

let run (cctxt : #Client_context.io_wallet) ~host ~port ~cert ~key =
  log "Accepting HTTPS requests on port %d" port >>= fun () ->
  let mode : Conduit_lwt_unix.server =
    `TLS (`Crt_file_path cert, `Key_file_path key, `No_password, `Port port) in
  let dir = RPC_directory.empty in
  let dir =
    RPC_directory.register0 dir Client_signer_remote_services.sign begin fun () req ->
      Handler.sign cctxt req.key req.data
    end in
  let dir =
    RPC_directory.register0 dir Client_signer_remote_services.public_key begin fun () req ->
      Handler.public_key cctxt req.key
    end in
  Lwt.catch
    (fun () ->
       RPC_server.launch ~host mode dir
         ~media_types:Media_type.all_media_types
         ~cors: { allowed_origins = [ "*" ] ;
                  allowed_headers = [ "Content-Type" ] }
       >>= fun _server ->
       fst (Lwt.wait ()))
    (function
      | Unix.Unix_error(Unix.EADDRINUSE, "bind","") ->
          failwith "Port already in use."
      | exn -> Lwt.return (error_exn exn))
