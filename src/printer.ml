let debug msg =
  if true then Printf.eprintf "%s\n%!" msg

let string_of_corequest req =
  Printf.sprintf "%s" (
    (req |> Cohttp.Request.meth |> Cohttp.Code.string_of_method)^" "^
    (req |> Cohttp.Request.resource)^" "^
    (req |> Cohttp.Request.version |> Cohttp.Code.string_of_version)^"\n"^
    (req |> Cohttp.Request.headers |> Cohttp.Header.to_string)^"\r\n\r\n"
  )

let string_of_coresponse (resp, body) =
  let sbody = Lwt_main.run (body |> Cohttp_lwt__.Body.to_string) in
  let sresp = Printf.sprintf "%s %s\r\n%s"
    (resp |> Cohttp.Response.version |> Cohttp.Code.string_of_version)
    (resp |> Cohttp.Response.status |> Cohttp.Code.string_of_status)
    (resp |> Cohttp.Response.headers |> Cohttp.Header.to_string)
  in
  (sresp, sbody)

let print_cohttp_request r =
  Printf.printf "%s" (string_of_corequest r)

let print_httpaf_request r =
  Httpaf.Request.pp_hum Format.std_formatter r

let () =
  let meth = Http_gen.meth in
  let target = Http_gen.uri in
  let version = Http_gen.version in
  let headers = Http_gen.headers in
  Crowbar.add_test ~name:"debug" [meth; target; version; headers] @@
  (fun meth target version headers ->
    let req = Part_gen.make_request ~meth ~target ~version ~headers in
    debug (Part_gen.to_string req);
    Crowbar.check true
  )
