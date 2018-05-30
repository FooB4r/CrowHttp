(* server_example.ml - TCP & Lwt *)
open Lwt
open Cohttp
(* module Str_server = Cohttp_lwt_unix.Server *)
(* module Str_server = Cohttp_lwt.Server.Make(String_io) *)
module Str_Server = Cohttp_lwt.S.Server(String_io)

let callback _conn req body =
  let uri = req |> Request.uri |> Uri.to_string in
  let meth = req |> Request.meth |> Code.string_of_method in
  let headers = req |> Request.headers |> Header.to_string in
  let body = Cohttp_lwt.Body.to_string body in
  (uri, meth, headers, body) |> (fun (uri, meth, headers, body) ->
    if uri = "//img/camel.jpg" && meth = "GET" then
      (*make a body from the image*)
      Str_server.respond_file "img/camel.jpg" ()
    else
      let ret_info = Printf.sprintf "Not Found\r\n\nuri: %s\nmeth: %s" uri meth in
      Str_server.respond_string ~status:(`Code 404) ~body:ret_info ())

let make_server port =
  Str_server.create ~mode:(`TCP (`Port port)) (Str_server.make ~callback ())

let make_unix_socket_server =
  Str_server.create ~mode:(`Unix_domain_socket (`File "cohttp_uds"))
    (Str_server.make ~callback ())

let cohttp_serv port =
  let server = make_unix_socket_server in
  ignore (Lwt_main.run server)

let () = cohttp_serv 8000



(******************************************************************************)
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
