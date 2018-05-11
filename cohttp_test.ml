(* Cohttp Request processing *)
open Cohttp
module Cohttp_Request = Cohttp.Request.Make(String_io)

let read_request_cohttp req =
  Cohttp_Request.read (Cohttp__String_io.open_in req)

let string_of_cohttp_status = function
  | `Ok r ->
    Printf.sprintf "%s" (
    "\tOK\n" ^
    "\t<Meth>\n\t"     ^(r |> Request.meth |> Code.string_of_method)^"\n"^
    "\t<Headers>\n\t"  ^(r |> Request.headers |> Header.to_string)^"\n"^
    "\t<ressource>\n\t"^ (r |> Request.resource)^"\n"^
    "\t<version>\n\t"  ^ (r |> Request.version |> Code.string_of_version)^"\n"^
    "\t<encoding>\n\t" ^ (r |> Request.encoding |> Transfer.string_of_encoding)
    )
  | `Eof -> "Eof"
  | `Invalid reason -> "Invalid: " ^ reason

let bool_of_retCode = function
  | `Ok _ -> true
  | `Eof -> true
  | `Invalid _ -> false
