(* Cohttp Request processing *)
module Cohttp_Request = Cohttp.Request.Make(String_io)

let read_request_cohttp req =
  Cohttp_Request.read (Cohttp__String_io.open_in req)

let string_of_cohttp_status = function
  | `Ok _ -> "Ok"
  | `Eof -> "Eof"
  | `Invalid reason -> "Invalid: " ^ reason

let bool_of_retCode = function
  | `Ok _ -> true
  | `Eof -> true
  | `Invalid _ -> false
