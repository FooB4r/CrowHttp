(* client_example.ml *)
open Cohttp
open Cohttp_lwt_unix
open Unix
open Part_gen

let abs_target target = "http://127.0.0.1:8000/"^target

let md5_list = "img/camel.md5"

(* =-=-=-=-=-=-=-=-=-=-=- cohttp lwt client (CLC) -=-=-=-=-=-=-=-=-=-=-=-=-= *)
let cohttp_call req =
  Client.call
    ~headers:(Header.of_list req.pg_headers)
    ~body:`Empty
    ~chunked:false
    (Code.method_of_string req.pg_method)
    (Uri.of_string (abs_target req.pg_target))

let tcp_CLC req =
  Lwt_main.run (cohttp_call req)

(* =-=-=-=-=-=-=-=-=-=-=-Shell ocaml scripting-=-=-=-=-=-=-=-=-=-=-=-=-=-= *)
let uds_SC () =
  (* TODO: return status in a file or something not with a trick *)
  let cmd_status = Sys.command
    "status=$(curl --unix-socket cohttp_uds -o img/camel.recv.jpg --write-out \
      %{http_code} --silent --url http://img/camel.jpg) &&
        exit $( [ \"$status\"==200 ] ) " in
  if cmd_status = 0 then
    let checksum = Sys.command ("md5sum --quiet -c " ^ md5_list) in
    let success = if checksum = 1 then "Failure" else "Success" in
    Printf.printf "%s\n" success
  else
    Printf.printf "Response Not 200"

(*  =-=-=-=-=-=-=-=-=-=-=- TCP Unix client  -=-=-=-=-=-=-=-=-=-=-= *)
(* Client *)
let open_connection port =
  let sock = socket PF_INET SOCK_STREAM 0 in
  let sock_addr = ADDR_INET (inet_addr_loopback, port) in
  try
    connect sock sock_addr;
    sock
  with ex -> close sock; raise ex

let close_connection fd =
  shutdown fd SHUTDOWN_SEND (* SHUTDOWN_ALL? *)

let send_message fd msg =
  send fd msg 0 (String.length msg) []

let read_response fd =
  let rsize = 1024 in
  let rec _read fd msg =
    let recvd = Bytes.create rsize in
    let n = read fd recvd 0 rsize in
    if n > 0 then
      let recvd = if n < 1024 then Bytes.sub recvd 0 n else recvd in
      _read fd (Bytes.cat msg recvd)
    else
      Bytes.to_string msg
  in
  _read fd Bytes.empty

(* Client that open a connection, send the message [msg] and closes the co *)
let one_time_client port msg =
  let fd = open_connection port in
  let _ = send_message fd msg in
  let resp = read_response fd in
  close_connection fd;
  close fd;
  resp
