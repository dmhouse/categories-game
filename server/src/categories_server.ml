open Core
open Async

module Files = struct
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
end

let static_files_handler ~body:_ _sock (request : Cohttp.Request.t) ~(files : Files.t) =
  match request.resource with
  | "/categories.bc.js" -> Cohttp_async.Server.respond_with_file files.js
  | "/style.css" -> Cohttp_async.Server.respond_with_file files.css
  | "/" | "/index.html" -> Cohttp_async.Server.respond_with_file files.html
  | _ ->
    printf "404: %s\n" request.resource;
    Cohttp_async.Server.respond_string "Not found" ~status:`Not_found
;;

let go' ~files ~port =
  let state =
    Server_state.create (Random.State.make_self_init ()) (Time_source.wall_clock ())
  in
  let handler =
    Cohttp_async_websocket.Server.create
      ~opcode:`Binary
      ~non_ws_request:(static_files_handler ~files)
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
      (Tcp.Where_to_listen.of_port port)
      handler
  in
  Deferred.never ()
;;

let go () =
  Command.async
    ~summary:"start server"
    (let%map_open.Command files = Files.param
     and port =
       flag "http-port" (required int) ~doc:" listen for http connections on this port"
     in
     fun () -> go ~files ~port)
  |> Command.run
;;
