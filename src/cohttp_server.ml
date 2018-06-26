open Cohttp
open Cohttp_lwt_unix

(* module Str_server = Cohttp_lwt.Make_server(String_io) *)

let callback _conn req body =
  let uri = req |> Request.uri |> Uri.to_string in
  let meth = req |> Request.meth |> Code.string_of_method in
  let headers = req |> Request.headers |> Header.to_string in
  let version = req |> Request.version |> Code.string_of_version in
  let body = Cohttp_lwt.Body.to_string body in
  (uri, meth, headers, body) |> (fun (uri, meth, headers, body) ->
    if uri = "//127.0.0.1:8000/img/camel.jpg" && meth = "GET" then
      Server.respond_file "img/camel.jpg" ()
    else
      Server.respond_string ~flush:true ~status:(`Code 404) ~body:""
        ~headers:(Header.of_list
          [("meth",meth); ("uri",uri); ("version",version)]) ()
  )

let make_server port =
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())

let make_unix_socket_server =
  Server.create ~mode:(`Unix_domain_socket (`File "cohttp_uds"))
    (Server.make ~callback ())

let cohttp_serv port =
  let server = make_server port in
  ignore (Lwt_main.run server)

let create_server = cohttp_serv 8000

let () = create_server
(******************************************************************************)
(* let handler ~body:_ _sock req =
  let uri = Cohttp.Request.uri req in
  match Uri.path uri with
  | "/test" ->
       Uri.get_query_param uri "hello"
    |> Base.Option.map ~f:(fun v -> "hello: " ^ v)
    |> Base.Option.value ~default:"No param hello supplied"
    |> Server.respond_string
  | _ ->
    Server.respond_string ~status:`Not_found "Route not found"

let start_server port () =
  Caml.Printf.eprintf "Listening for HTTP on port %d\n" port;
  Caml.Printf.eprintf "Try 'curl http://localhost:%d/test?hello=xyz'\n%!" port;
  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Async_extra.Tcp.Where_to_listen.of_port port) handler
  >>= fun _ -> Deferred.never ()


let tcp_CAC () =
  let module Command = Async_extra.Command in
  Command.async_spec
    ~summary:"Start a hello world Async server"
    Command.Spec.(
      empty +>
      flag "-p" (optional_with_default 8080 int)
        ~doc:"int Source port to listen on"
    ) start_server

  |> Command.run *)


(* cohttp_server using async *)
(* open Base
open Async_kernel
open Cohttp_async

type 'a io = 'a Deferred.t
type body = Body.t
type spec = Request.t -> body -> (Response.t * body) io
type async_test = unit -> unit io

(* let const = Cohttp_test.const
             = fun a b c -> a *)
let const resp arg2 arg3 = resp

let response_sequence fail responses =
  let xs = ref responses in
  fun req body ->
    match !xs with
    | x::xs' ->
      xs := xs';
      x req body
    | [] -> fail "response_sequence: Server exhausted responses"


let response_sequence = response_sequence failwith

let temp_server ?(port=8001) spec callback =
  let uri = Uri.of_string ("http://0.0.0.0:" ^ (Int.to_string port)) in
  let server = Server.create ~on_handler_error:`Raise
    (Async_extra.Tcp.Where_to_listen.of_port port)
    (fun ~body _sock req -> spec req body) in
  server >>= fun server ->
  callback uri >>= fun res ->
  Server.close server >>| fun () ->
  res

(* let test_server_s ?port ?(name="Cohttp Server Test") spec f =
  temp_server ?port spec begin fun uri ->
    Logs.info (fun m -> m "Test %s running on %s" name (Uri.to_string uri));
    let tests = f uri in
    let results =
      tests
      |> Deferred.List.map ~how:`Sequential ~f:(fun (name, test) ->
        Logs.debug (fun m -> m "Running %s" name);
        let res =
          try_with test >>| function
          | Ok () -> `Ok
          | Error exn -> `Exn exn in
        res >>| (fun res -> (name, res))) in
    results >>| (fun results ->
      let ounit_tests =
        results
        |> List.map ~f:(fun (name, res) ->
          name >:: fun () ->
            match res with
            | `Ok -> ()
            | `Exn x -> raise x) in
      name >::: ounit_tests)
  end *)

let chunk_body = ["one"; ""; " "; "bar"; ""]

let large_string = String.make (Int.pow 2 16) 'A'

let response_bodies = [ "Testing"
                      ; "Foo bar" ]

let ok s = Server.respond `OK ~body:(Body.of_string s)

let chunk size = String.init ~f:(Fn.const 'X') size
let chunk_size = 33_000
let chunks = 3

(* apparently this is the spec *)
let server =
  [ (* empty_chunk *)
    const @@ Server.respond `OK ~body:(Body.of_string_list chunk_body);
    (* large response *)
    const @@ Server.respond_string large_string;
    (* large request *)
    (fun _ body ->
       body |> Body.to_string >>| String.length >>= fun len ->
       Server.respond_string (Int.to_string len));
  ] @ (* pipelined_chunk *)
  (response_bodies |> List.map ~f:(Fn.compose const ok))
  @ [ (* large response chunked *)
    (fun _ _ ->
       let body =
         let (r, w) = Pipe.create () in
         let chunk = chunk chunk_size in
         for _ = 0 to chunks - 1 do
           Pipe.write_without_pushback w chunk
         done;
         Pipe.close w;
         r
       in
       Server.respond_with_pipe ~code:`OK body
    )
  ]
  |> response_sequence

(* stupid on purpose *)
let callback uri =
  let body = Printf.sprintf  "uri: %s\n"(Uri.to_string uri) in
  Server.respond_string body

let cohttp_async_serv () =
  temp_server ~port:8001 server callback *)
(******************************************************************************)
