open Http_gen
open Test_client

let okResponse = "HTTP/1.1 200 OK"
let portNumber = 8000

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

(* Check *)
let () =
  (* TODO: Launching the server *)
  (* Testing *)
  Crowbar.add_test ~name:"http" [http_message] @@ (fun http ->
    let serv_response = one_time_client portNumber http in
    Printf.printf "[===-Response-===]\n %s\n---\n" serv_response;
    Crowbar.check_eq ~pp:pp_http ~eq:eq_http serv_response okResponse);
