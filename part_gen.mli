type pg_request = {
  pg_method : string;
  pg_target : string;
  pg_version: string;
  pg_headers: (string * string) list
}

val make_request: meth:string -> target:string -> version:string
  -> headers:(string * string) list -> pg_request

val to_string: pg_request -> string

val cohttp_request: pg_request -> Cohttp__Request.t
val httpaf_request: pg_request -> Httpaf.Request.t


val cohttp_request_opt: pg_request -> Cohttp__Request.t option
val httpaf_request_opt: pg_request -> Httpaf.Request.t option
