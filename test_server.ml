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

(* Nothing ATM *)
let server = ""

let get_reponse response_str =
  String_io.M.(
    StringResponse.read (String_io.open_in response_str)
    >>= function
    | `Ok resp -> failwith "whatever"
    | _ -> failwith ("### Fail on: ###\n" ^ response_str)
  )
