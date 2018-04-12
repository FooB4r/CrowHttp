module Cohttp_String_stdio : Cohttp.S.IO
 with type 'a t = 'a Lwt.t
 and type ic = in_channel
 and type oc = out_channel
 and type conn = string
