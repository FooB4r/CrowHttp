open Cohttp_test
open Httpaf_test

let okResponse = "HTTP/1.1 200 OK"
let portNumber = 8000
let verbosity = ref false

let print_cond a =
  if !verbosity then
    Printf.printf a
  else
    (fun _ -> ())

let string_of_corequest r =
  Printf.sprintf "%s" (
  (r |> Cohttp.Request.meth |> Cohttp.Code.string_of_method)^" "^
  (r |> Cohttp.Request.resource)^" "^
  (r |> Cohttp.Request.version |> Cohttp.Code.string_of_version)^"\n"^
  (r |> Cohttp.Request.headers |> Cohttp.Header.to_string))

let print_cohttp_request r =
  Printf.printf "%s" (string_of_corequest r)

let print_httpaf_request r =
  Httpaf.Request.pp_hum Format.std_formatter r

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
  let meth = Http_gen.meth in
  let target = Http_gen.uri in
  let version = Http_gen.version in
  let headers = Http_gen.headers in
  Crowbar.add_test ~name:"http" [meth; target; version; headers] @@
  (fun meth target version headers ->
      let req = Part_gen.make_request ~meth ~target ~version ~headers in
      Printf.printf "%s\n\n" (Part_gen.to_string req);
      let coreq = Part_gen.cohttp_request_opt req in
      let afreq = Part_gen.httpaf_request_opt req in
      match coreq, afreq with
      | Some _, None -> Crowbar.fail "Httpaf failed to create | Cohttp created"
      | None, Some _ -> Crowbar.fail "Cohttp failed to create | Httpaf created"
      (* maybe add a test to see where it failed and compare *)
      (* with few tests it seems that cohttp never raises exceptions when creating requests *)
      | None, None -> Printf.printf "Both lib failed to create"; Crowbar.check true
      (* TODO: Get server's answer and compare *)
      | Some coreq, Some afcoreq ->  Printf.printf "Both lib created the request";Crowbar.check true
    );
