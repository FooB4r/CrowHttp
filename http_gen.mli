type 'a gen = 'a Crowbar.gen

(** Generate basic elements of an Http Request / Response *)
val meth    : unit -> string gen
val uri     : unit -> string gen
val version : unit -> string gen
val header  : unit -> string gen
val headers : unit -> (string list) gen
val request : unit -> string gen

val response: unit -> string gen

(** Provides with function to change the syntax and preserve the semantic *)
val split_header : string -> int list -> string
