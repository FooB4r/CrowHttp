type 'a t = 'a
let return a = a
let (>>=) = (|>)

module Sio = Cohttp__String_io

type ic = Sio.M.ic
type oc = Sio.M.oc
type conn = Sio.M.conn

let read_line ic = return (Sio.M.read_line ic)
let read ic n = return (Sio.M.read ic n)

let write oc str = return (Sio.M.write oc str)
let flush oc = return (Sio.M.flush oc)
