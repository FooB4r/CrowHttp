open Httpaf

let handler body reqd =
  (* let {meth; target; version; headers} = Reqd.request redq in *)
  let request_body = Reqd.request_body reqd in
  let request = Reqd.request reqd in
  let response =
    Printf.printf "target: %s" request.target;
    if request.target = "img/camel.jpg" && request.meth = `GET then
      Response.create `OK
    else
      Response.create `Not_found
    in
  Body.close_reader request_body;
  Reqd.respond_with_string reqd response body

let debug msg =
  if true then Printf.eprintf "%s\n%!" msg

let echo_handler got_eof reqd =
  debug " > echo_handler called";
  let request_body  = Reqd.request_body reqd in
  let response      = Response.create ~headers:Headers.(of_list ["connection", "close"]) `OK in
  let response_body = Reqd.respond_with_streaming reqd response in
  let rec on_read buffer ~off ~len =
    Body.write_string response_body (Bigstring.to_string ~off ~len buffer);
    Body.flush response_body (fun () ->
      Body.schedule_read request_body ~on_eof ~on_read)
  and on_eof () = got_eof := true; Body.close_writer response_body in
  Body.schedule_read request_body ~on_eof ~on_read

(* let () =
  let sc = Server_connection.create (handler "") in *)


let request_to_string r =
  let f = Faraday.create 0x1000 in
  Httpaf.Httpaf_private.Serialize.write_request f r;
  Faraday.serialize_to_string f

let response_to_string r =
  let f = Faraday.create 0x1000 in
  Httpaf.Httpaf_private.Serialize.write_response f r;
  Faraday.serialize_to_string f

let body_to_strings = function
  | `Empty       -> []
  | `Fixed   xs  -> xs
  | `Chunked xs  ->
    List.fold_right (fun x acc ->
      let len = String.length x in
      [Printf.sprintf "%x\r\n" len; x; "\r\n"] @ acc)
    xs [ "0\r\n" ]

(* let body_to_strings b = [()] *)

let case_to_strings = function
  | `Request  r, body -> [request_to_string  r] @ (body_to_strings body)
  | `Response r, body -> [response_to_string r] @ (body_to_strings body)

let response_stream_to_body (`Response response, body) =
  let response = response_to_string response in
  match body with
  | `Empty  -> response
  | `Fixed xs | `Chunked xs -> String.concat "" (response :: xs)

let iovec_to_string { IOVec.buffer; off; len } =
  Bigstring.to_string ~off ~len buffer

let bigstring_append_string bs s =
  Bigstring.of_string (Bigstring.to_string bs ^ s)

let bigstring_empty = Bigstring.of_string ""

let test_server ~input ~output ~handler () =
  let reads  = List.(concat (map case_to_strings input)) in
  let writes = List.(concat (map case_to_strings output)) in
  let conn   = Server_connection.create handler in
  let iwait, owait = ref false, ref false in
  let rec loop conn input reads =
    if !iwait && !owait then
      assert false (* deadlock, at lest for test handlers. *);
    if Server_connection.is_closed conn
    then begin
      debug "state: closed";
      []
    end else begin
      let input', reads' = iloop conn input reads in
      let output         = oloop conn in
      output @ loop conn input' reads'
    end
  and iloop conn input reads =
    if !iwait
    then begin debug " iloop: wait"; input, reads end
    else
      match Server_connection.next_read_operation conn, reads with
      | `Read, read::reads' ->
        debug " server iloop: read";
        let input     = bigstring_append_string input read in
        let input_len = Bigstring.length input in
        let result    = Server_connection.read conn input ~off:0 ~len:input_len in
        if result = input_len
        then bigstring_empty, reads'
        else Bigstring.sub ~off:result input, reads'
      | `Read, [] ->
        debug " server iloop: eof";
        Server_connection.shutdown_reader conn;
        bigstring_empty, []
      | _          , [] ->
        debug " server iloop: eof";
        Server_connection.shutdown_reader conn;
        bigstring_empty, []
      | `Close    , _     ->
        debug " server iloop: close(ok)"; input, []
      | `Yield , _  ->
        debug " server iloop: yield";
        iwait := true;
        Server_connection.yield_reader conn (fun () -> debug " iloop: continue"; iwait := false);
        input, reads
  and oloop conn =
    if !owait
    then (begin debug " server oloop: wait"; [] end)
    else
      match Server_connection.next_write_operation conn with
      | `Close _ ->
        debug " server oloop: closed"; []
      | `Yield ->
        debug " server oloop: yield";
        owait := true;
        Server_connection.yield_writer conn (fun () -> debug " server oloop: continue"; owait := false);
        []
      | `Write iovecs ->
        debug " server oloop: write";
        let output = List.map iovec_to_string iovecs in
        Server_connection.report_write_result conn (`Ok (IOVec.lengthv iovecs));
        output
  in
  let test_output = loop conn bigstring_empty reads |> String.concat "" in
  let output      = String.concat "" writes in
  Printf.printf "test_output:<%s>\n" test_output;
  Printf.printf "output:<%s>\n" output

(* TODO instrument this and automate the making of requests *)
let gogo =
  test_server
  (* ~handler: (echo_handler (ref false)) *)
  ~handler: (handler "")
  ~input:   [(`Request (Request.create `GET "img/camel.jpg")), `Empty]
  ~output:  [(`Response (Response.create `OK) ), `Empty]
  ()
