(* cohttp request *)
(*
  type t = {
    headers: Header.t;    (** HTTP request headers *)
    meth: Code.meth;      (** HTTP request method *)
    resource: string;         (** Request path and query *)
    version: Code.version; (** HTTP version, usually 1.1 *)
    encoding: Transfer.encoding; (** transfer encoding of this HTTP request *)
  } [@@deriving fields, sexp]
*)
open Cohttp

let co_headers = Header.init ()

let co_meth = Code.method_of_string "GET"

let co_ressource = "/"

let co_version = Code.version_of_string "HTTP/1.1"

let co_encoding = Transfer.Unknown

let co_uri = Uri.of_string "/"

let cohttp_request = Request.make ~meth:co_meth ~version:co_version
  ~headers:co_headers ~encoding:co_encoding co_uri

let string_of_corequest r =
  Printf.sprintf "%s" (
  (r |> Request.meth |> Code.string_of_method)^" "^
  (r |> Request.resource)^" "^
  (r |> Request.version |> Code.string_of_version)^"\n"^
  (r |> Request.headers |> Header.to_string))

(* httpaf request *)
(*
type t =
    { meth    : Method.t
    ; target  : string
    ; version : Version.t
    ; headers : Headers.t }
*)
open Httpaf

let af_headers = Headers.empty
(*Header.of_list (name*value) list*)

let af_meth = Method.of_string "GET"

let af_version = Version.of_string "HTTP/1.1"

let af_target = "/"

let httpaf_request = Request.create ~version:af_version ~headers:af_headers
  af_meth af_target

let print_part_http_test =
  let core = Part_gen.cohttp_request in
  let afre = Part_gen.httpaf_request in
  Printf.printf ">cohttp dummy:\n%s\n>httpaf dummy:\n" (Part_gen.string_of_corequest core);
  Httpaf.Request.pp_hum Format.std_formatter afre
