open Httpaf

let debug msg =
  if true then Printf.eprintf "%s\n%!" msg

let bigstring_append_string bs s =
  Bigstring.of_string (Bigstring.to_string bs ^ s)

let bigstring_empty = Bigstring.of_string ""

let stream_file conn filename =
  let ic = open_in_bin filename in
  let buffer = Bytes.create 1024 in
  let rec _read conn =
    let n = input ic buffer 0 1024 in
    debug ("streaming "^ (string_of_int n)^ " bytes");
    if n != 0 then (
      let realdata = Bytes.sub buffer 0 n in
      Body.write_string conn ~off:0 ~len:n (Bytes.to_string realdata);
      _read conn)
  in
  _read conn;
  close_in ic

let handler reqd =
  (* let {meth; target; version; headers} = Reqd.request redq in *)
  (* let request_body = Reqd.request_body reqd in *)
  let request = Reqd.request reqd in
  let headers = Headers.of_list [ ("meth",    Method.to_string request.meth);
                                  ("target",  request.target)] in
  let goodReq = request.target = "img/camel.jpg" && request.meth = `GET in
  let response =
    if goodReq then
      Response.create ~headers `OK
    else
      Response.create ~headers `Not_found
    in
  let response_body = Reqd.respond_with_streaming reqd response in
  (if goodReq then
    stream_file response_body "img/camel.jpg"
  else
    Body.write_string response_body ~off:0 ~len:1 "" (* ahem no one saw that *)
  );
  Body.close_writer response_body

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

let case_to_strings = function
  | `Request  r, body -> [request_to_string  r] @ (body_to_strings body)
  | `Response r, body -> [response_to_string r] @ (body_to_strings body)

let iovec_to_string { IOVec.buffer; off; len } =
  Bigstring.to_string ~off ~len buffer

let test_server ~input () =
  let reads  = List.(concat (map case_to_strings input)) in
  let iwait, owait = ref false, ref false in
  let conn = Server_connection.create handler in
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
  loop conn bigstring_empty reads (*|> String.concat ""*)

let test_request req =
  test_server ~input:[req] ()
