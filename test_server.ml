open Cohttp_lwt
open Lwt

module Request = Cohttp.Request.Make(String_stdio)

let read_request req =
  Request.read (Cohttp__String_io.open_in req)

let string_of_status = function
  | `Ok _ -> "Ok"
  | `Eof -> "Eof"
  | `Invalid reason -> "Invalid: " ^ reason

let is_parsed retCode expected =
  match retCode with
  | `Ok _ -> true
  | `Eof -> false
  | `Invalid _ -> false
