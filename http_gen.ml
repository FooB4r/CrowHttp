(* =-=-=-=-=-=-=-=-=-= Generates http messages =-=-=-=-=-=-=-=-=-= *)
open Crowbar

let empty = const ""

(* concatenate a List of string gen inserting the separator string sep between each *)
let concat_gen_list sep l =
  List.fold_left (fun acc e ->
    map [acc; sep; e] (fun acc sep e -> acc ^ e ^ sep)
  ) empty l

(* create a list of size n containing gen *)
let list_gen_sized n gen =
  let rec list_gen_sized_aux n gen acc =
    if n > 0 then
      list_gen_sized_aux (n-1) gen (gen::acc)
    else
      acc
  in list_gen_sized_aux n gen []

(* Concatenate a Crowbar.list *)
let concat_list_gen sep l =
  map [sep; l] String.concat

(* regex: ?s <=> zero or one time s *)
let optional s =
  choose [empty; s]

(* Basic types *)
let octet = bytes_fixed 8
let char_t = map [range 128] (fun n -> String.make 1 (Char.chr n))
let upalpha = map [range 26] (fun n -> Char.chr (n + 65) |> String.make 1)
let loalpha = map [range 26] (fun n -> Char.chr (n + 97) |> String.make 1)
let alpha = choose [upalpha; loalpha]
let digit = map [range 10] (fun n -> Char.chr (n + 48) |> String.make 1)
let ctl =
  map [choose [range 32; const 127]] (fun n -> Char.chr n |> (String.make 1))

(* (Char.chr |> (String.make 1)) *)
let cr = const "\r" (* <=> map [const 13] Char.chr exept it is a string*)
let lf = const "\n" (* 10 *)
let sp = const " " (* 32 *)
let ht = const "\t" (* 9 *)
let dblquote = const "\"" (* 34 *)

let crlf = const "\r\n"
let port = map [range 65536] string_of_int

(* linear white space : [CRLF] 1*( SP | HT ) *)
let lws = map [(optional crlf); (list1 (choose [sp; ht]))] String.concat
let lws_star = concat_list_gen empty (list lws) (* regex: *lws *)
let word_sep = concat_list_gen empty (list1 lws)

(* any OCTET except CTLs, but including LWS*)
let text =
  let octet = map [octet] (fun str ->
    let b = Bytes.create 8 in
    Bytes.blit_string str 0 b 0 8;
    Bytes.iter (fun c -> let code = Char.code c in
      guard (code > 32 && code != 127)) b;
    str
  ) in
  choose [lws; octet]

let hex = choose [digit; const "A"; const "B"; const "C"; const "D"; const "E";
  const "F"; digit; const "a"; const "b"; const "c"; const "d"; const "e";
  const "f"]

(* 1*<any CHAR except CTLs or separators> *)
let token = concat_list_gen empty (list1
      (choose [
        map [range 94] (fun n -> (String.make 1) (Char.chr (n + 32))); (* 32 -> 126*)
        map [range 127] (fun n -> (String.make 1) (Char.chr (n + 128))) (*128 -> 255 *)
      ])
    )

let product_version = token
let product = concat_gen_list empty [const "/"; optional product_version]

let separators = choose [const "("; const ")"; const "<"; const ">"; const "@";
  const ","; const ";"; const ":"; const "\\"; dblquote; const "/"; const "[";
  const "]"; const "?"; const "="; const "{"; const "}"; sp; ht]

let quoted_pair = concat_gen_list lws_star [const "\\"; char_t]
(*let qdtext =
  let a = map [text] () *)

let qdtext = const "#qdtext#" (* all but "\"" *)
let ctext = const "#ctext#"
let quoted_string = concat_gen_list lws_star [
    dblquote; choose [qdtext; quoted_pair]; dblquote
  ]
let comment = concat_gen_list lws_star [
    const "(";
    concat_list_gen lws_star (list(choose [ctext; quoted_pair]));
    const ")"
  ]


let http_version =
  concat_gen_list lws_star [const "HTTP"; const "/";
  concat_list_gen empty (list1 digit);
  const "."; concat_list_gen empty (list1 digit)]
(* let http_version = const "HTTP/1.1" *)

let uri = const "#URI#" (*TODO read rfc on URIs + 3.2.3 URI Comparison ?? *)
let http_url = const "#http_url#" (* TODO *)

let rfc1123_date = const "Sun, 06 Nov 1994 08:49:37 GMT"
let rfc850_date = const "Sunday, 06-Nov-94 08:49:37 GMT"
let asctime_date = const "Sun Nov  6 08:49:37 1994"
let date = choose [rfc1123_date; rfc850_date; asctime_date]


let gh_cache_control = const "#gh_cache_control#"
let gh_connection = choose [const "Close"; const "Keep-Alive"]
let gh_date = date
let gh_pragma = const "#gh_pragma#"
let gh_trailer = const "#gh_trailer#"
let gh_transfer_encoding = const "#gh_transfer_encoding#"
let gh_upgrade = const "#gh_upgrade#"
let gh_via = const "#gh_via#"
let gh_warning = const "#gh_warning#"

let general_header = choose [
  gh_cache_control;            (* Section 14.9 *)
  gh_connection;               (* Section 14.10 *)
  gh_date;                     (* Section 14.18 *)
  gh_pragma;                   (* Section 14.32 *)
  gh_trailer;                  (* Section 14.40 *)
  gh_transfer_encoding;        (* Section 14.41 *)
  gh_upgrade;                  (* Section 14.42 *)
  gh_via;                      (* Section 14.45 *)
  gh_warning                   (* Section 14.46 *)
]

let rh_accept = const "#rh_accept#"
let rh_accept_charset = const "#rh_accept_charset#"
let rh_accept_encoding = const "#rh_accept_encoding#"
let rh_accept_language = const "#rh_accept_language#"
let rh_authorization = const "#rh_authorization#"
let rh_expect = const "#rh_expect#"
let rh_from = const "#rh_from#"
let rh_host = concat_gen_list lws_star [
    const "Host"; const ":"; const "www.w3.org";
    optional (concat_gen_list lws_star [const ":"; port])
  ]
let rh_if_match = const "#rh_if_match#"
let rh_if_modified_since = const "#rh_if_modified_since#"
let rh_if_none_match = const "#rh_if_none_match#"
let rh_if_range = const "#rh_if_range#"
let rh_if_unmodified_since = const "#rh_if_unmodified_since#"
let rh_max_forwards = concat_gen_list lws_star [
    const "Max-Fowards"; const ":"; concat_list_gen empty (list1 digit)
  ]
let rh_proxy_authorization = const "#rh_proxy_authorization#"
let rh_range = const "#rh_range#"
let rh_referer = const "#rh_referer#"
let rh_te = const "#rh_te#"
let rh_user_agent = concat_gen_list lws_star [
    const "User-Agent"; const ":";
    concat_list_gen lws_star (list1 (choose [product; comment]))
  ]

let request_header = choose [
  rh_accept;
  rh_accept_charset;         (* Section 14.2 *)
  rh_accept_encoding;        (* Section 14.3 *)
  rh_accept_language;        (* Section 14.4 *)
  rh_authorization;          (* Section 14.8 *)
  rh_expect;                 (* Section 14.20 *)
  rh_from;                   (* Section 14.22 *)
  rh_host;                   (* Section 14.23 *)
  rh_if_match;               (* Section 14.24 *)
  rh_if_modified_since;      (* Section 14.25 *)
  rh_if_none_match;          (* Section 14.26 *)
  rh_if_range;               (* Section 14.27 *)
  rh_if_unmodified_since;    (* Section 14.28 *)
  rh_max_forwards;           (* Section 14.31 *)
  rh_proxy_authorization;    (* Section 14.34 *)
  rh_range;                  (* Section 14.35 *)
  rh_referer;                (* Section 14.36 *)
  rh_te;                     (* Section 14.39 *)
  rh_user_agent              (* Section 14.43 *)
]

let eh_allow = const "#eh_allow#"
let eh_content_encoding = const "#eh_content_encoding#"
let eh_content_language = const "#eh_content_language#"
let eh_content_length = const "#eh_content_length#"
let eh_content_location = const "#eh_content_location#"
let eh_content_md5 = const "#eh_content_md5#"
let eh_content_range = const "#eh_content_range#"
let eh_content_type = const "#eh_content_type#"
let eh_expires = const "#eh_expires#"
let eh_last_modified = const "#eh_last_modified#"
let extension_header = const "#message_header#"

let entity_header = choose [
  eh_allow;                    (* Section 14.7 *)
  eh_content_encoding;         (* Section 14.11 *)
  eh_content_language;         (* Section 14.12 *)
  eh_content_length;           (* Section 14.13 *)
  eh_content_location;         (* Section 14.14 *)
  eh_content_md5;              (* Section 14.15 *)
  eh_content_range;            (* Section 14.16 *)
  eh_content_type;             (* Section 14.17 *)
  eh_expires;                  (* Section 14.21 *)
  eh_last_modified;            (* Section 14.29 *)
  extension_header
]


let general_header = const "#general_header#"
let entity_header = const "#entity_header#"

(* @debug: if doesn't generate a lot change list -> list1 to force it *)
let request_body = concat_list_gen lws_star (list(concat_gen_list lws
  [choose [general_header; request_header; entity_header]; crlf]))


let entity_body = concat_list_gen empty (list octet)
let message_body = entity_body (* + encoded *)

(*URI: https://tools.ietf.org/html/rfc2396 *)
let authority = const "#authority#"
let abs_path = const "#abs_path#"
let absolute_uri = const "#absolute_uri#"
let request_uri = choose [const "*"; absolute_uri; abs_path; authority]

let extension_method = token
(* Ok here is a big problem, some methods requires some header fields and such *)
let http_method = choose [const "OPTIONS"; const "GET"; const "HEAD"; const "POST";
  const "PUT"; const "DELETE"; const "TRACE"; const "CONNECT"; extension_method]

let request_line = (* Section 5.1 *)
  concat_gen_list empty [http_method; sp; request_uri; sp; http_version; crlf]

let request = (* Section 5 *)
  concat_gen_list lws_star [request_line; request_body; crlf; optional message_body]
let http_message = request (* We are only interested in requests not responses *)

let http =
  choose [http_message]

let expected_http_output = "\r"

let pp_http ppf http =
  pp ppf "(%s)" (http)


(* Check *)
(* let () =
  add_test ~name:"http" [http] @@ (fun http ->
    check_eq ~pp:pp_http http expected_http_output) *)

(* Print possible outputs NO check *)

(* To debug messages *)
let () =
  add_test ~name:"http" [http] @@ (fun http ->
    Printf.printf "======================\n";
    Printf.printf "%s\n" http;
  )
