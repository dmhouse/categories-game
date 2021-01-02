open Core
open Async

module Static_resources = struct
  type t =
    { js : Filename.t
    ; css : Filename.t
    }

  let js_filename = "categories.bc.js"
  let css_filename = "style.css"

  let param =
    let%map_open.Command js =
      flag "js" (required Filename.arg_type) ~doc:" path to javascript"
    and css = flag "css" (required Filename.arg_type) ~doc:" path to css" in
    { js; css }
  ;;

  let html ~path_root =
    sprintf
      {|
       <!DOCTYPE html>
       <html>
          <head>
             <meta charset="UTF-8">
             <title>Scattegories</title>
             <script defer src="%s"></script>
             <link rel="stylesheet" href="%s" type="text/css">
             <meta name="viewport" content="width=device-width, initial-scale=1.0">
          </head>
          <body>
            <div id="app">Loading...</div>
          </body>
       </html>
       |}
      (path_root ^/ js_filename)
      (path_root ^/ css_filename)
  ;;

  let handler t ~path_root =
    Staged.stage (fun ~body:_ _sock (request : Cohttp.Request.t) ->
        let normalised_url =
          request.resource
          |> String.chop_prefix ~prefix:path_root
          |> Option.map ~f:(String.lstrip ~drop:(fun c -> Char.( = ) c '/'))
        in
        let respond_404 () =
          Cohttp_async.Server.respond_string "Not found" ~status:`Not_found
        in
        match normalised_url with
        | None -> respond_404 ()
        | Some resource ->
          if String.( = ) resource js_filename
          then Cohttp_async.Server.respond_with_file t.js
          else if String.( = ) resource css_filename
          then Cohttp_async.Server.respond_with_file t.css
          else if String.( = ) resource "" || String.( = ) resource "index.html"
          then Cohttp_async.Server.respond_string (html ~path_root)
          else respond_404 ())
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

let go' ~static_resources ~ssl_config ~port ~root_uri =
  let state =
    Server_state.create (Random.State.make_self_init ()) (Time_source.wall_clock ())
  in
  let path_root =
    match root_uri with
    | None -> "/"
    | Some uri -> Uri.path uri
  in
  let should_process_request _inet header ~is_websocket_request =
    match is_websocket_request with
    | false -> Ok ()
    | true ->
      let origins =
        match root_uri with
        | None -> []
        | Some uri ->
          [ Uri.make ?scheme:(Uri.scheme uri) ?host:(Uri.host uri) ?port:(Uri.port uri) ()
            |> Uri.to_string
          ]
      in
      Cohttp_async_websocket.Header.origin_matches_host_or_is_one_of header ~origins
  in
  let handler =
    Cohttp_async_websocket.Server.create
      ~opcode:`Binary
      ~non_ws_request:(Static_resources.handler static_resources ~path_root |> unstage)
      ~should_process_request
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
      ~on_handler_error:
        (`Call
          (fun addr exn ->
            print_s
              [%message "Error in TCP handler" (addr : Socket.Address.Inet.t) (exn : exn)]))
      ~mode:(Ssl_config.conduit_mode ssl_config)
      (Tcp.Where_to_listen.of_port port)
      (fun ~body sock request ->
        print_s
          [%message
            "Serving URL" (request.meth : Cohttp.Code.meth) (request.resource : string)];
        handler ~body sock request)
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
     and root_uri =
       flag
         "root-url"
         (optional (Arg_type.create Uri.of_string))
         ~doc:" root URL for the server"
     in
     fun () -> go' ~static_resources ~ssl_config ~port ~root_uri)
  |> Command.run
;;
