type pg_request = {
  pg_method : string;
  pg_target : string;
  pg_version: string;
  pg_headers: (string * string) list
}

let make_request ~meth ~target ~version ~headers =
  {
    pg_method = meth;
    pg_target = target;
    pg_version = version;
    pg_headers = headers
  }

let to_string req =
  req.pg_method ^ " " ^ req.pg_target ^ " " ^ req.pg_version ^ "\r\n" ^
  (Http_gen.concat_headers req.pg_headers) ^ "\r\n\r\n"

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

let co_meth meth = Code.method_of_string meth
let co_uri uri = Uri.of_string uri
let co_version version = Code.version_of_string version
let co_headers headers = Header.of_list headers

let cohttp_request req =
  Printexc.print (fun meth target version headers ->
      Request.make
        ~meth:(co_meth meth)
        ~version:(co_version version)
        ~headers:(co_headers headers)
        ~encoding:Transfer.Unknown
        (co_uri target)
  ) req.pg_method req.pg_target req.pg_version req.pg_headers


let cohttp_request_opt req =
  try
    Some (cohttp_request req)
  with
  | _ -> None


(* httpaf request *)
(*
type t =
    { meth    : Method.t
    ; target  : string
    ; version : Version.t
    ; headers : Headers.t }
*)
open Httpaf

let af_meth meth = Method.of_string meth
let af_version version = Version.of_string version
let af_headers headers = Headers.of_list headers

let httpaf_request req =
  Printexc.print (fun meth target version headers ->
    Request.create
      ~version:(af_version version)
      ~headers:(af_headers headers)
      (af_meth meth)
      target
  ) req.pg_method req.pg_target req.pg_version req.pg_headers

let httpaf_request_opt req =
  try
    Some (httpaf_request req)
  with
  | _ -> None
