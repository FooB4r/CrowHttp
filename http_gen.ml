(* =-=-=-=-=-=-=-=-=-= Generates http messages =-=-=-=-=-=-=-=-=-= *)
open Crowbar

let empty = const ""

(* Concat operator for string gen *)
let (^^) a b = map [a; b] (^)

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

(* converts an int in [0;255] in a string(1) of the corresponding char *)
let string_of_charint i =
  i |> Char.chr |> (String.make 1)

(* Basic types *)
let octet = bytes_fixed 8
let char_t = map [range 128] string_of_charint
let upalpha = map[range ~min:65 26] string_of_charint
let loalpha = map [range ~min:97 26] string_of_charint
let alpha = choose [upalpha; loalpha]
let digit = map [range ~min:48 10] string_of_charint
let tchar = choose[const "!"; const "#"; const "$"; const "%"; const "&";
  const "'"; const "*"; const "+"; const "-"; const "."; const "^"; const "_";
  const "`"; const "|"; const "~"; digit; alpha]
let number = concat_list_gen empty (list1 digit)
let ctl =
  map [choose [range 32; const 127]] string_of_charint
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
let ows = concat_list_gen empty (list (choose [sp; ht])) (* optional ws *)
let rws = concat_list_gen empty (list1 (choose [sp; ht])) (* required ws *)

(* List in http look like this *)
(* *( "," OWS ) content *( OWS "," [ OWS content ) ] ) *)
let http_list content =
  (concat_list_gen empty (list(const "," ^^ ows))) ^^ content ^^
  (concat_list_gen empty (list(ows ^^ const "," ^^ optional(ows ^^ content))))

(* any OCTET except CTLs, but including LWS*)
let text =
  let octet = map [octet] (fun str ->
    String.iter (fun c -> let code = Char.code c in
      guard (code > 32 && code != 127)) str;
    str
  ) in
  choose [lws; octet]

let hex = choose [digit; const "A"; const "B"; const "C"; const "D"; const "E";
  const "F"; const "a"; const "b"; const "c"; const "d"; const "e"; const "f"]

(* 1*<any CHAR except CTLs or separators> *)
let token = concat_list_gen empty (list1 tchar)

let product_version = token
let product = const "/" ^^ optional product_version

let separators = choose [const "("; const ")"; const "<"; const ">"; const "@";
  const ","; const ";"; const ":"; const "\\"; dblquote; const "/"; const "[";
  const "]"; const "?"; const "="; const "{"; const "}"; sp; ht]

let quoted_pair = concat_gen_list lws_star [const "\\"; char_t]
(* any TEXT except "\"" *)
let qdtext = map [text] (fun t ->
    guard (not (String.contains t '"')); t
  )
(* any TEXT excluding "(" and ")" *)
let ctext = map [text] (fun t ->
    guard (not (String.contains t '(' || String.contains t ')')); t
  )
let quoted_string = concat_gen_list lws_star [
    dblquote; choose [qdtext; quoted_pair]; dblquote
  ]
let comment = concat_gen_list lws_star [
    const "(";
    concat_list_gen lws_star (list(choose [ctext; quoted_pair])); (*rec comment ?*)
    const ")"
  ]

(* let http_version =
  concat_gen_list ows [const "HTTP"; const "/"; number; const "."; number] *)
let http_version = concat_gen_list ows [const "HTTP"; const "/"; const "1"; const "."; const "1"]

let uri = const "#URI#"
let http_url = const "#http_url#"

let rfc1123_date = const "Sun, 06 Nov 1994 08:49:37 GMT"
let rfc850_date = const "Sunday, 06-Nov-94 08:49:37 GMT"
let asctime_date = const "Sun Nov 6 08:49:37 1994"
let date = choose [rfc1123_date; rfc850_date; asctime_date]

(* split the string s at the index n *)
let split_at s index =
  let length = String.length s in
  let index = max 1 (min (length - 2) index) in
  ((String.sub s 0 index), (String.sub s index (String.length s - index)))

(* str -> int list -> str list -- [split_at_n str indexes] *)
(* Splits [str] at all [indexes] once and returns the list of splitted parts *)
(* split_at_n "hello" [1;3;3;5] -> ["h";"el";"lo";""] *)
let split_at_n s indexes =
  if String.length s < 2 then [s] (* avoid early msg end *)
  else
    let indexes = List.sort_uniq (fun a b -> b - a) indexes in
    let rec _split_at_n s ids acc =
      match ids with
      | [] -> s::acc
      | id::nextids -> let (s1,s2) = split_at s id in
        _split_at_n s1 nextids (s2::acc)
    in
    _split_at_n s indexes []


(* 20 random list of 10 numbers in [0;99] *)
(* https://www.random.org/integer-sets/ *)
let rdm_lists = [
  const [80; 93; 35; 63; 27; 37; 90; 16; 46; 13];
  const [12; 94; 11; 61; 73; 60; 37; 40; 47; 79];
  const [39; 15; 98; 25; 3; 21; 34; 93; 16; 81];
  const [21; 62; 39; 33; 17; 12; 48; 22; 54; 10];
  const [6; 32; 41; 10; 35; 23; 63; 73; 83; 37];
  const [99; 51; 87; 35; 26; 78; 24; 83; 94; 19];
  const [10; 12; 70; 52; 15; 64; 49; 44; 62; 6];
  const [78; 13; 10; 3; 82; 26; 8; 18; 80; 11];
  const [65; 11; 0; 39; 73; 53; 97; 30; 54; 56];
  const [86; 28; 96; 13; 32; 1; 47; 51; 20; 77];
  const [58; 50; 82; 90; 39; 94; 23; 72; 32; 18];
  const [12; 35; 43; 92; 7; 67; 52; 88; 72; 34];
  const [85; 39; 46; 15; 72; 16; 53; 23; 5; 8];
  const [54; 47; 79; 45; 46; 73; 50; 31; 74; 1];
  const [24; 99; 77; 43; 45; 55; 0; 13; 31; 72];
  const [24; 69; 21; 58; 75; 76; 93; 36; 26; 80];
  const [14; 97; 69; 25; 11; 89; 20; 79; 23; 26];
  const [66; 64; 78; 83; 44; 51; 60; 58; 43; 75];
  const [4; 93; 28; 81; 75; 5; 72; 86; 96; 65];
  const [89; 26; 72; 55; 93; 14; 21; 47; 82; 28]
]

let rdm_list = choose rdm_lists

(* Adds lws_star between words *)
(* TODO: to split generate the headers separetly and an random number list *)
(* Then in ocaml call the function split_at_n <genHeader> <RdmList>*)
(* separetly generate the rest if the message and concat the whole processed pieces *)
let split_content sep str split =
  let splitted = map [str; split] (fun str spl -> split_at_n str spl) in
  map [sep; splitted] String.concat

(*  Make a http header called <name> with <content> as a content *)
let make_header sep name content =
  const name ^^ const ":" ^^ (optional lws) ^^ (split_content sep content rdm_list)

(* /!\ MOST OF THE RULES ARE SIMPLIFIED /!\ *)
let gh_cache_control = make_header lws_star "Cache-Control" (const "no-cache")
let gh_connection = make_header lws_star "Connection"
  (choose [const "Close"; const "Keep-Alive"])
let gh_date = make_header empty "Date" date
let gh_pragma = make_header lws_star "Pragma" (const "no-cache")
(* let gh_trailer = const "#gh_trailer#" *)
(* let gh_transfer_encoding = const "#gh_transfer_encoding#" *)
(* let gh_upgrade = const "#gh_upgrade#" *)
let gh_via = make_header lws_star "Via" (concat_gen_list lws_star
  [http_version; token; const "pseudonym"]) (* simplified *)
let gh_warning = make_header lws_star "Warning" (const "299 pseudo \"warn-text\"") (* simplified *)

let general_header = choose [
  gh_cache_control;            (* Section 14.9 *)
  gh_connection;               (* Section 14.10 *)
  gh_date;                     (* Section 14.18 *)
  gh_pragma;                   (* Section 14.32 *)
  (* gh_trailer;                  (* Section 14.40 *) (* useless *) *)
  (* gh_transfer_encoding;        (* Section 14.41 *) *)
  (* gh_upgrade;                  (* Section 14.42 *) *)
  gh_via;                      (* Section 14.45 *)
  gh_warning                   (* Section 14.46 *)
]

let rh_accept = make_header lws_star "Accept"
  (const "text/*, text/html, text/html;level=1, */*")
let rh_accept_charset = make_header lws_star "Accept-Charset" (const "iso-8859-5")
let rh_accept_encoding = make_header lws_star "Accept-Encoding" (const "compress, gzip")
let rh_accept_language = make_header lws_star "Accept-Language" (const "da, en-gb;q=0.8, en;q=0.7")
let rh_authorization = make_header lws_star "Authorization" token
let rh_expect = make_header lws_star "Expect" (const "100-continue")
let rh_from = make_header lws_star "From" (const "webmaster@w3.org")
let rh_host = make_header lws_star "Host" (concat_gen_list lws_star
  [const "www.w3.org"; optional (concat_gen_list lws_star [const ":"; port])])
let rh_if_match = make_header lws_star "If-Match" (const "*")
let rh_if_modified_since = make_header lws_star "If-Modifier-Since" date
let rh_if_none_match = make_header lws_star "If-None-Match" (const "*")
let rh_if_range = make_header lws_star "If-Range" date
let rh_if_unmodified_since = make_header lws_star "If-Unmodified-Since" date
let rh_max_forwards = make_header lws_star "Max-Fowards" number
let rh_proxy_authorization = make_header lws_star "Proxy-Authorization" token
(* let rh_range = const "#rh_range#"  *)
let rh_referer = make_header lws_star "Referer" uri
(* let rh_te = const "#rh_te#" *)
let rh_user_agent = make_header lws_star "User-Agent"
  (concat_list_gen lws_star (list1 (choose [product; comment])))

let request_header = choose [
  (* rh_accept;                 (* Section 14.1 *) *)
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
  (* rh_range;                  (* Section 14.35 *) *)
  rh_referer;                (* Section 14.36 *)
  (* rh_te;                     (* Section 14.39 *) *)
  rh_user_agent              (* Section 14.43 *)
]

let eh_allow = make_header lws_star "Allow" (const "GET, HEAD, PUT")
(* let eh_content_encoding = const "#eh_content_encoding#" *)
let eh_content_language = make_header lws_star "Content-Language" (const "mi, en")
let eh_content_length = make_header lws_star "Content-Length" number
let eh_content_location = make_header lws_star "Content-Location" uri
let eh_content_md5 = make_header lws_star "Content-MD5" (const "md5-digest")
(* let eh_content_range = const "#eh_content_range#" *)
let eh_content_type =  make_header lws_star "Content-Type"
  (const "text/html; charset=ISO-8859-4")
let eh_expires = make_header lws_star "Expires" date
let eh_last_modified = make_header lws_star "Last-Modified" date
(* let extension_header = const "#message_header#" *)

let entity_header = choose [
  eh_allow;                    (* Section 14.7 *)
  (* eh_content_encoding;         (* Section 14.11 *) *)
  eh_content_language;         (* Section 14.12 *)
  eh_content_length;           (* Section 14.13 *)
  eh_content_location;         (* Section 14.14 *)
  eh_content_md5;              (* Section 14.15 *)
  (* eh_content_range;            (* Section 14.16 *) *)
  eh_content_type;             (* Section 14.17 *)
  eh_expires;                  (* Section 14.21 *)
  eh_last_modified;            (* Section 14.29 *)
  (* extension_header *)
]

(* @debug: if doesn't generate a lot change list -> list1 to force it *)
let request_body = concat_list_gen empty (
  list(choose [general_header; request_header; entity_header] ^^ crlf))

let full_request_body = concat_gen_list lws_star [
  gh_cache_control; crlf;
  gh_connection; crlf;
  gh_date; crlf;
  gh_pragma; crlf;
  gh_via; crlf;
  gh_warning; crlf;
  rh_accept_charset; crlf;
  rh_accept_encoding; crlf;
  rh_accept_language; crlf;
  rh_authorization; crlf;
  rh_expect; crlf;
  rh_from; crlf;
  rh_host; crlf;
  rh_if_match; crlf;
  rh_if_modified_since; crlf;
  rh_if_none_match; crlf;
  rh_if_range; crlf;
  rh_if_unmodified_since; crlf;
  rh_max_forwards; crlf;
  rh_proxy_authorization; crlf;
  rh_referer; crlf;
  rh_user_agent; crlf;
  eh_allow; crlf;
  eh_content_language; crlf;
  eh_content_length; crlf;
  eh_content_location; crlf;
  eh_content_md5; crlf;
  eh_content_type; crlf;
  eh_expires; crlf;
  eh_last_modified; crlf
]

let entity_body = concat_list_gen empty (list1 octet) (* WARNING this yield anything *)
let big_entity_body = concat_gen_list empty (list_gen_sized 20 octet)
let message_body = entity_body (* + encoded *)

(*URI: https://tools.ietf.org/html/rfc2396 *)
let authority_form = const "userinfo@host:1234"
let absolute_uri = const "/absolutely/uri"
let origin_form = absolute_uri ^^ const "?q=helloworld&t=ffab&ia=web"
let request_target = choose [const "*"; origin_form; absolute_uri; authority_form]

let extension_method = token
let http_method = choose [const "OPTIONS"; const "GET"; const "HEAD"; const "POST";
  const "PUT"; const "DELETE"; const "TRACE"; const "CONNECT"; extension_method]

let request_line = (* Section 5.1 *)
  http_method ^^ sp ^^ request_target ^^  sp ^^ http_version ^^ crlf

let request = (* Section 5 *)
  request_line ^^ request_body ^^ (optional (message_body ^^ crlf)) ^^ crlf

let not_a_request = const "Not a http request"
let test_request = const "GET / HTTP/1.1" ^^ crlf ^^ crlf
let http_message = request (* We are only interested in requests not responses *)
