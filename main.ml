open Http_gen
open Test_server

let okResponse = "HTTP/1.1 200 OK"
let portNumber = 8000
let verbosity = true

let print_cond a =
  if verbosity then
    Printf.printf a
  else
    (fun _ -> ())

(* returns [str] from begining to the first occurence of \n *)
let get_first_line str =
  let pos = String.index_opt str '\n' in
  match pos with
  | None -> str
  | Some p -> let fst_line = String.sub str 0 (p-1) in fst_line

let pp_http ppf http =
  let msg = get_first_line http in
  Crowbar.pp ppf "%s" msg

(* Check if the reponse is [expected] *)
let eq_http response_str expected =
  let fst_line = get_first_line response_str in
  String.equal fst_line expected

open Lwt
let print_status status =
  print_cond "STATUS: %s\n" (string_of_status status)

(* Check *)
let () =
  print_cond "%s\n%!" "Starting the tests...";
  Crowbar.add_test ~name:"http" [http_message] @@ (fun http ->
    print_cond "[===-TESTING-===]\n%s\n" http;
    let status = Lwt_main.run (read_request http) in
    print_cond "STATUS: %s\n" (string_of_status status);
    Crowbar.check_eq ~eq:is_parsed status status);
