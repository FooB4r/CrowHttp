(* client_example.ml *)
open Lwt
open Cohttp
open Cohttp_lwt_unix

let reception_target = "img/camel.recv.jpg"
let target           = "/img/camel.jpg"
let abs_target       = "http://127.0.0.1:8000"^target
let unix_target      = "http://img/camel.jpg"

let get_md5 filename =
  Sys.command ("md5sum " ^ filename)

let md5_list = "img/camel.md5"

(* let body =
  Client.get (Uri.of_string abs_target) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body *)

let () =
  (* let body = Lwt_main.run body in *)
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
