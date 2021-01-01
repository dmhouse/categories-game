open Core
open Async

module Static_resources = struct
  type t =
    { js : Filename.t
    ; css : Filename.t
    ; html : Filename.t
    }

  let param =
    let%map_open.Command js =
      flag "js" (required Filename.arg_type) ~doc:" path to javascript"
    and css = flag "css" (required Filename.arg_type) ~doc:" path to css"
    and html = flag "html" (required Filename.arg_type) ~doc:" path to html" in
    { js; css; html }
  ;;

  let handler t ~url_root =
    Staged.stage (fun ~body:_ _sock (request : Cohttp.Request.t) ->
        let normalised_url =
          request.resource
          |> String.chop_prefix ~prefix:(Uri.to_string url_root)
          |> Option.map ~f:(String.lstrip ~drop:(fun c -> Char.( = ) c '/'))
        in
        match normalised_url with
        | Some "categories.bc.js" -> Cohttp_async.Server.respond_with_file t.js
        | Some "style.css" -> Cohttp_async.Server.respond_with_file t.css
        | Some ("" | "index.html") -> Cohttp_async.Server.respond_with_file t.html
        | Some _ | None ->
          printf "404: %s\n" request.resource;
          Cohttp_async.Server.respond_string "Not found" ~status:`Not_found)
  ;;
end

module Ssl_config = struct
  type t =
    | No_https
    | Use_https of
        { cert_file : Filename.t
        ; private_key_file : Filename.t
        }

  let param =
    let open Command.Param in
    Command.Param.choose_one
      [ (let%map.Command cert_file =
           flag
             "cert"
             (optional Filename.arg_type)
             ~doc:" path to certificate chain ending in server cert"
         and private_key_file =
           flag
             "private-key"
             (optional Filename.arg_type)
             ~doc:" path to certificate private key"
         in
         let%map.Option cert_file = cert_file
         and private_key_file = private_key_file in
         Use_https { cert_file; private_key_file })
      ; flag "no-https" (no_arg_some No_https) ~doc:" don't serve https"
      ]
      ~if_nothing_chosen:Raise
  ;;

  let conduit_mode = function
    | No_https -> `TCP
    | Use_https { cert_file; private_key_file } ->
      `OpenSSL (`Crt_file_path cert_file, `Key_file_path private_key_file)
  ;;
end

let go' ~static_resources ~ssl_config ~port ~url_root =
  let state =
    Server_state.create (Random.State.make_self_init ()) (Time_source.wall_clock ())
  in
  let handler =
    Cohttp_async_websocket.Server.create
      ~opcode:`Binary
      ~non_ws_request:(Static_resources.handler static_resources ~url_root |> unstage)
      (fun ~inet:_ ~subprotocol:_ _request ->
        Cohttp_async_websocket.Server.On_connection.create (fun reader writer ->
            let open Async_rpc_kernel in
            let transport = Pipe_transport.(create Kind.string reader writer) in
            let%bind conn =
              Rpc.Connection.create
                transport
                ~connection_state:(fun _conn -> Server_state.Conn_state.create ())
                ~implementations:(Server_state.implementations state)
              >>| Result.ok_exn
            in
            let%bind () = Rpc.Connection.close_finished conn in
            Rpc.Transport.close transport)
        |> Deferred.return)
  in
  let%bind _server =
    Cohttp_async.Server.create_expert
      ~on_handler_error:`Ignore
      ~mode:(Ssl_config.conduit_mode ssl_config)
      (Tcp.Where_to_listen.of_port port)
      handler
  in
  Deferred.never ()
;;

let go () =
  Command.async
    ~summary:"start server"
    (let%map_open.Command static_resources = Static_resources.param
     and ssl_config = Ssl_config.param
     and port =
       flag "port" (required int) ~doc:" listen for http(s) connections on this port"
     and url_root =
       flag
         "url-root"
         (optional_with_default (Uri.of_string "/") (Arg_type.create Uri.of_string))
         ~doc:" root URL for the server"
     in
     fun () -> go' ~static_resources ~ssl_config ~port ~url_root)
  |> Command.run
;;
