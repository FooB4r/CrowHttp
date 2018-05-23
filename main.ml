open Http_gen
open Cohttp_test
open Httpaf_test



(**)
let part_http_test =
  let core = Part_gen.cohttp_request in
  let afre = Part_gen.httpaf_request in
  Printf.printf ">cohttp dummy:\n%s\n>httpaf fummy:\n%s\n"
    (Part_gen.string_of_corequest core) (Httpaf.Request.pp_hum afre)
(**)

let okResponse = "HTTP/1.1 200 OK"
let portNumber = 8000
let verbosity = ref false

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

let xor_bool out1 out2 =
  (out1 = out2)

let print_status status =
  print_cond "STATUS: %s\n" (string_of_cohttp_status status)

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
    let co_status = read_request_cohttp http in
    let af_status = read_request_httpaf http in
    print_cond "%s\n" "[---cohttp-Status---]";
    print_cond "%s\n" (string_of_cohttp_status co_status);
    print_cond "%s\n" "[---httpaf-Status---]";
    print_cond "%s/" (string_of_httpaf_status af_status);
    print_cond "%d\n" (String.length http);
    let are_same = xor_bool (bool_of_retCode co_status) (bool_of_httpaf af_status) in
    Crowbar.check are_same);
