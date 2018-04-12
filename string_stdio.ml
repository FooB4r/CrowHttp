module Cohttp_String_stdio = struct
  type 'a t = 'a Lwt.t
  type ic = in_channel
  type oc = out_channel
  type conn = string
  let (>>=) = Lwt.bind
  let return = Lwt.return

  let ic = stdin
  let oc = stdout
  let conn = ""

  let read_line ic =
    try
      let s = input_line ic in
      return (Some s)
    with _ -> return None

  let read ic n =
    try
      let s = really_input_string ic n in
      return s
    with _ -> return ""


  let write oc s =
    return (output_string oc s)

  let flush oc =
    return (flush oc)
end
