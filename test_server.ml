open Lwt
open Cohttp
open Cohttp_lwt_unix

let callback _conn req body =
  (Cohttp_lwt.Body.to_string body) >>= (fun body ->
    Server.respond_string ~status:`OK ~body ())

let server = Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

(* let server port = Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ()) *)

let () = ignore (Lwt_main.run server)
