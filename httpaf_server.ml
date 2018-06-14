open Httpaf

let debug msg =
  if false then Printf.eprintf "%s\n%!" msg

let bigstring_append_string bs s =
  Bigstring.of_string (Bigstring.to_string bs ^ s)

let bigstring_empty = Bigstring.of_string ""

(* to improve *)
let bigstring_of_file filename =
  let ic = open_in_bin filename in (* open as binary *)
  let buffer = Bytes.create 1024 in
  let rec read_file bsbuf =
    let n = input ic buffer 0 1024 in
    if n = 0 then bsbuf else
      let rbuf = Bytes.sub buffer 0 n in
      let bs = bigstring_append_string bsbuf (Bytes.to_string rbuf) in
      read_file bs
  in
  let file = read_file bigstring_empty in
  close_in ic;
  file

let handler got_eof reqd =
  (* let {meth; target; version; headers} = Reqd.request redq in *)
  let request_body = Reqd.request_body reqd in
  let request = Reqd.request reqd in
  let headers = Headers.of_list [ ("meth",    Method.to_string request.meth);
                                  ("target",  request.target)] in
  let response =
    if request.target = "img/camel.jpg" && request.meth = `GET then
      Response.create ~headers `OK
    else
      Response.create ~headers `Not_found
    in
  let response_body = Reqd.respond_with_streaming reqd response in
  let body = bigstring_of_file "img/camel.jpg" in (*make it buffer by buffer*)
  Body.write_bigstring response_body ~off:0 ~len:(Bigstring.length body) body;
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

let test_server ~conn ~input () =
  let reads  = List.(concat (map case_to_strings input)) in
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
  loop conn bigstring_empty reads (*|> String.concat ""*)

let create_connection () = Server_connection.create (handler (ref false))

let test_request conn req =
  test_server ~conn ~input:[req] ()
