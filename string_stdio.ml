module Cohttp_String_stdio = struct
  type 'a t = 'a Lwt.t
  type ic = Lwt_io.input_channel
  type oc = Lwt_io.output_channel
  type conn = Conduit_lwt_unix.flow
  let (>>=) = Lwt.bind
  let return = Lwt.return

  let ic = Lwt_io.stdin
  let oc = Lwt_io.stdout
  (* let conn = Conduit_lwt_unix.flow *)

  let read_line ic =
    Lwt_io.read_line_opt ic

  let read ic n =
    Lwt_io.read ~count:n ic

  let write oc s =
    Lwt_io.write oc s

  let flush oc =
    Lwt_io.flush oc
end
