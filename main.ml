open Http_gen

let okResponse = "HTTP/1.1 200 OK"
let portNumber = 8000
let verbosity = ref false

let print_cond a =
  if !verbosity then
    Printf.printf a
  else
    (fun _ -> ())

let print_cond a =
  if !verbosity then
    Printf.printf a
  else
    (fun _ -> ())

(* http processing *)
(* returns [str] from begining to the first occurence of \n *)
let get_first_line str =
  let pos = String.index_opt str '\n' in
  match pos with
  | None -> str
  | Some p -> let fst_line = String.sub str 0 (p-1) in fst_line

let pp_http ppf http =
  let msg = get_first_line http in
  Crowbar.pp ppf "%s" msg

let eq_http response_str expected =
  let fst_line = get_first_line response_str in
  String.equal fst_line expected

(* Request processing *)
module Request = Cohttp.Request.Make(String_io)

let read_request req =
  Request.read (Cohttp__String_io.open_in req)

let string_of_status = function
  | `Ok _ -> "Ok"
  | `Eof -> "Eof"
  | `Invalid reason -> "Invalid: " ^ reason

let is_parsed retCode expected =
  match retCode with
  | `Ok _ -> true
  | `Eof -> false
  | `Invalid _ -> false

let print_status status =
  print_cond "STATUS: %s\n" (string_of_status status)

let print_help () =
  Printf.printf "%s\n" Sys.argv.(0);
  Printf.printf "Usage %s [OPTION]\n" Sys.argv.(0);
  Printf.printf "\t -v, --verbose Print information on each tests\n";
  Printf.printf "\t -h, --help    Show this help\n"

(* Check *)
let () =
  for i = 0 to Array.length Sys.argv - 1 do
    if Sys.argv.(i) = "-v" || Sys.argv.(i) = "--verbose" then
      verbosity := true;
    if Sys.argv.(i) = "-h" || Sys.argv.(i) = "--help" then
      print_help ();
  done;
  print_cond "%s\n%!" "Starting the tests...";
  Crowbar.add_test ~name:"http" [http_message] @@ (fun http ->
    print_cond "[===-TESTING-===]\n%s\n" http;
    let status = Lwt_main.run (read_request http) in
    print_cond "STATUS: %s\n" (string_of_status status);
    Crowbar.check_eq ~eq:is_parsed status status);
