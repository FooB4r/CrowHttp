module Cohttp_String_stdio : Cohttp.S.IO
 with type 'a t = 'a Lwt.t
 and type ic = Lwt_io.input_channel
 and type oc = Lwt_io.output_channel
 and type conn = Conduit_lwt_unix.flow
