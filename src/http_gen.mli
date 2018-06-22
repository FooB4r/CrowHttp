type 'a gen = 'a Crowbar.gen

(** Generate basic elements of an Http Request / Response *)
val meth    : string gen
val uri     : string gen
val version : string gen
val header  : (string * string) gen
val headers : (string * string) list gen
val request : string gen

val response: string gen

val valid_version : string gen

val concat_headers    : (string * string) list -> string
val concat_headers_gen: (string * string) list gen -> string gen

(** Provided with functions to change the syntax and preserve the semantic *)
val split_header : string -> int list -> string list
