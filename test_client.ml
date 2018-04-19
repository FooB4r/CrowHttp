(* Simple Client *)
open Unix
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
  let resp = Bytes.create 256 in
  let _ = read fd resp 0 256 in
  Bytes.to_string resp

(* Client that open a connection, send the message [msg] and closes the co *)
let one_time_client port msg =
  let fd = open_connection port in
  let _ = send_message fd msg in
  let resp = read_response fd in
  close_connection fd;
  close fd;
  resp
