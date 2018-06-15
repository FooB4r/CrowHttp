type test_status = Parsing | Server | Response of int | Md5 of string | Success
type mode = Overall | Step

let okResponse = "HTTP/1.1 200 OK"
let portNumber = 8000
let verbosity = ref false
let log = ref false
let checkMode = Step (* env variable ? *)

let httpaf_target = "img/afcamel.recv.jpg"
let httpaf_md5    = "img/afcamel.md5"
let cohttp_target = "img/cocamel.recv.jpg"
let cohttp_md5    = "img/cocamel.md5"

let target = "img/camel.jpg"

let m_request target = Part_gen.make_request
  ~meth:"GET" ~target ~version:"HTTP/1.1" ~headers:[("Connection", "close")]

let good_request = m_request target

let debug msg =
  if !verbosity then Printf.eprintf "%s\n%!" msg

let string_of_test_status = function
  | Parsing -> "Parsing"
  | Server -> "Server"
  | Response(c) -> "Response ("^(string_of_int c)^")"
  | Md5(md5) -> "md5 ("^md5^")"
  | Success -> "Success"

let string_of_corequest req =
  Printf.sprintf "%s" (
    (req |> Cohttp.Request.meth |> Cohttp.Code.string_of_method)^" "^
    (req |> Cohttp.Request.resource)^" "^
    (req |> Cohttp.Request.version |> Cohttp.Code.string_of_version)^"\n"^
    (req |> Cohttp.Request.headers |> Cohttp.Header.to_string)^"\r\n\r\n"
  )

let string_of_coresponse (resp, body) =
  let sbody = Lwt_main.run (body |> Cohttp_lwt__.Body.to_string) in
  let sresp = Printf.sprintf "%s %s\r\n%s"
    (resp |> Cohttp.Response.version |> Cohttp.Code.string_of_version)
    (resp |> Cohttp.Response.status |> Cohttp.Code.string_of_status)
    (resp |> Cohttp.Response.headers |> Cohttp.Header.to_string)
  in
  (sresp, sbody)

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

let write_file filename msg =
  (* Truncate file to 0 length if exist create if not *)
  let oc = open_out filename in
  Printf.fprintf oc "%s" msg;
  close_out oc (* flush and close *)

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
  let len = String.length req in
  let fd = Unix.openfile filename [Unix.O_WRONLY; O_CREAT; O_APPEND] 0o640 in
  let wr = Unix.write fd req 0 len in
  if len > wr then
    Printf.eprintf "Error: Unable to write log files";
  Unix.close fd

let check_md5 file =
  let result = Sys.command ("md5sum --status -c "^file) in
  result = 0

let check_overall costats afstats =
  let isfail = function
  | Success -> false
  | _ -> true
  in
  (isfail costats) = (isfail afstats)

let check_by_step costats afstats =
  match costats, afstats with
  | Success, Success -> true
  | Md5(chk1), Md5(chk2) -> (String.equal chk1 chk2)
  | Response(c1), Response(c2) -> (c1 = c2)
  | afs, cos -> afs = cos

let test_httpaf conn req =
  let afreq = Part_gen.httpaf_request_opt req in
  match afreq with
  | Some req ->
    let res = Httpaf_server.test_request conn (`Request req, `Empty) in
    let afres = String.concat "" res in
    (* List.iteri (fun i e -> Printf.eprintf "res[%d]: %s\n" i e) res; *)
    let len = String.length afres in
    if len > 0 then (
      let shortResp = if (len > 100) then
        (List.nth res 0)^"\n(body)..." else afres in
      debug ("Httpaf: "^(string_of_int len)^">"^shortResp);
      let code = Scanf.sscanf (List.hd res) "HTTP/%d.%d %d" (fun _ _ code -> code) in
      if code = 404 then Response 404 else (
        write_file httpaf_target (List.nth res 1);
        if check_md5 httpaf_md5 then Success else Md5("NYI")
      )
    ) else (
      debug "Httpaf: 0>response empty"; Server
    )
  | None -> debug "Httpaf didn't parse/create the request"; Parsing

let test_cohttp req =
  let coreq = Part_gen.cohttp_request_opt req in
  match coreq with
  | Some r -> let (resp, body) = Clients.tcp_CLC req in
    let (sresp, sbody) = string_of_coresponse (resp, body) in
    let len = (String.length sresp) + (String.length sbody) in
    if len > 0 then (
      let shortResp = if (len > 100) then
        sresp ^"\n(body)..." else sresp^sbody in
      debug ("Cohttp: "^(string_of_int len)^">"^shortResp);
      let code = match Cohttp.Response.status resp with
        | `Code c -> c
        | s -> Cohttp.Code.code_of_status s
        in
      if code = 404 then Response 404 else
      (
        write_file cohttp_target sbody;
        if check_md5 cohttp_md5 then Success else Md5("NYI")
      )
    ) else (
      debug "Cohttp: 0>response empty"; Server
    )
  | None -> Parsing

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
    debug "----------------------------------";
    let afstats = test_httpaf afserv req in
    debug ("- - - - - - - - - - - - - - - - -");
    let costats = test_cohttp req in
    debug "----------------------------------";
    debug ("Cohttp: "^string_of_test_status costats);
    debug ("Httpaf: "^string_of_test_status afstats);
    let success = match checkMode with
    | Overall -> check_overall costats afstats
    | Step    -> check_by_step costats afstats
    in Crowbar.check success
  );
