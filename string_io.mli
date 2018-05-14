(** Lwt IO implementation that uses strings to marshal and unmarshal HTTP *)

(** IO interface that uses {!buf} for input data and queues output
   data into a {!Buffer.t}.  Never actually blocks despite the Lwt
   use, although a future revision may yield when parsing large
   strings. *)
include Cohttp.S.IO
  with type 'a t = 'a
  and type ic = Cohttp__String_io.buf
  and type oc = Buffer.t
