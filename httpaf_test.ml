open Httpaf

(* Httpaf Request processing *)
let req_handler body reqd =
  let req_body = Reqd.request_body reqd in
  (* Not in 0.2.0 release *)
  Body.close_reader req_body;
  Reqd.respond_with_string reqd (Response.create `OK) body

let error_handler ?request:_ error handle =
  Printf.printf "I caught an error \n";
  match error with
  | `Bad_gateway -> raise (Failure "Bad_gateway")
  | `Bad_request -> raise (Failure "Bad request")
  | `Exn e -> raise (Failure ("(Exn) " ^ (Printexc.to_string e)))
  | `Internal_server_error -> raise (Failure "Internal_server_error")

let error_handler ?request:_ error handle =
  raise (Failure "lol")

let serv_co () = Server_connection.create ~error_handler (req_handler "")

(* read the whole message *)
let read_loop conn req =
  let input = Bigstring.of_string req in
  let reqlen = Bigstring.length input in
  let rec _loop size  =
    if size >= reqlen then
      size
    else
      match Server_connection.next_read_operation conn with
      | `Read  ->
        let len = reqlen - size in (* len remaining *)
        (* Not in 0.2.0 release *)
        let result = Server_connection.read conn input ~off:size ~len in
        (* Printf.printf "[READ(%d)] size: %d, len: %d -- tot: %d\n" result size len reqlen; *)
        if result = 0 then size
        else _loop (size+result)
      | `Close -> size
      | `Yield -> Server_connection.yield_reader conn (fun () -> ());
        _loop size
  in
  _loop 0

let read_request_httpaf req =
    (* Create a server *)
    let serv = serv_co () in
    try
      read_loop serv req
    with
    | Failure msg -> Printf.printf "%s\n" msg; -1
    (* | _ -> Printf.printf "Unkown failure\n"; -1 *)

let string_of_httpaf_status = function
  | i -> string_of_int i

let bool_of_httpaf n =
  n > 0
