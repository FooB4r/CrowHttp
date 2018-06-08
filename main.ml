open Cohttp_test
open Httpaf_test

let okResponse = "HTTP/1.1 200 OK"
let portNumber = 8000
let verbosity = ref false
let log = ref false

let target = "http://127.0.0.1:8000/img/camel.jpg"

let m_request target = Part_gen.make_request
  ~meth:"GET" ~target ~version:"HTTP/1.1" ~headers:[("Connection", "close")]

let request = m_request target

let debug msg =
  if !verbosity then Printf.eprintf "%s\n%!" msg

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

let log_file req =
  (* create dir if it doesn't exists *)
  let _safe_mkdir name =
    try Unix.mkdir name 0o771 with
    | Unix.Unix_error(Unix.EEXIST, "mkdir", name) -> ()
  in
  _safe_mkdir "output";
  _safe_mkdir "output/log";
  let logDir = "output/log/" in
  let filename = logDir ^ "request.crash.all" in
  let len = (String.length req) in
  let fd = Unix.openfile filename [Unix.O_WRONLY; O_CREAT; O_APPEND] 0o640 in
  let wr = Unix.write fd req 0 len in
  if len > wr then
    Printf.eprintf "Error: Unable to write log files";
  Unix.close fd

let test_httpaf conn req =
  let afreq = Part_gen.httpaf_request_opt req in
  match afreq with
  | Some req ->
    let afres = Httpaf_server.test_request conn (`Request req, `Empty) in
    let len = String.length afres in
    if len > 0 then (
      debug ("Httpaf: "^(string_of_int len)^">"^afres); Some afres
    ) else (
      debug "Httpaf didn't parse"; None
    )
  | None -> debug "Httpaf didn't parse"; None

let test_cohttp req =
  let coreq = Part_gen.cohttp_request_opt req in
  match coreq with
  | None -> failwith "Cohttp didn't parse (NONE)" (* fail because this never happens *)
  | _ -> ();
  let sreq = (Part_gen.to_string req) in
  let cores = Clients.one_time_client portNumber sreq in
  let len = String.length cores in
  if len > 0 then (
    debug ("Cohttp: "^(string_of_int len)^">"^cores); Some cores
  ) else (
    debug "Cohttp didn't parse"; None
  )

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
    if Sys.argv.(i) = "--log-crashes" then
      log := true
  done;
  Printf.printf "Starting the tests...\n";
  let meth = Http_gen.meth in
  let target = Http_gen.uri in
  let version = Http_gen.version in
  let headers = Http_gen.headers in
  let afserv = Httpaf_server.create_connection () in
  (* Cohttp_server.create_server portNumber; *)
  Crowbar.add_test ~name:"http" [meth; target; version; headers] @@
  (fun meth target version headers ->
    let req = Part_gen.make_request ~meth ~target ~version ~headers in
    debug "===========TEST REQUEST===========";
    debug (Part_gen.to_string req);
    let afstats = test_httpaf afserv req in
    let costats = test_cohttp req in
    debug "\n";
    match costats, afstats with
    | Some cores, Some afres -> Crowbar.check true (* TODO check *)
    | None, None -> Crowbar.check true
    | _ -> if !log then log_file (Part_gen.to_string req);
      Crowbar.check false
  );
