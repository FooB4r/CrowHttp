open Lwt
open Cohttp
open Cohttp_lwt_unix

module String_io = Cohttp__String_io
module StringResponse = Cohttp.Response.Make(String_io.M)

let ok_response =
  "HTTP/1.1 200 OK\r\n\r\n\r\n"

let eq_http response_str expected =
  String_io.M.(
    StringResponse.read (String_io.open_in response_str)
    >>= function
    | `Ok resp -> true
    | _ -> false
  )

(* Simple server that only returns the message OK when ok and nothing otherwise *)
let server =
  let callback _conn req body =
    body |> Cohttp_lwt.Body.to_string >>= (fun body ->
      Server.respond_string ~status:`OK ~body ()
    )
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
