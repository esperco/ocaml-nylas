open Lwt
open Cohttp
open Cohttp.Code
open Cohttp_lwt_unix

open Inbox_app

open Nlencoding

exception Error_code of Cohttp.Code.status_code

(** Default URIs, suitable for hosted Inbox instances. *)
let api_uri  = Uri.of_string "https://api.inboxapp.com"
let base_uri = Uri.of_string "https://www.inboxapp.com"

let api_path { api_uri } path = Uri.with_path api_uri path

let authentication_uri app user_email redirect_uri =
  let uri = Uri.with_path app.base_uri "oauth/authorize" in
  Uri.add_query_params' uri [
    ("client_id", app.app_id);
    ("response_type", "code");
    ("scope", "email");
    ("login_hint", user_email);
    ("redirect_uri", Uri.to_string redirect_uri)
  ]

let call_string http_method ?access_token ?headers ?body uri =
  let headers = match headers with | Some h -> h | None -> [] in
  let headers = match access_token with
    | Some token ->
       ("Authorization", ("Basic " ^ Base64.encode (token ^ ":")))::headers
    | None       -> headers
  in
  let headers = Header.of_list headers in 
  Client.call ~headers ?body http_method uri >>= fun (response, body) ->
  match response.Client.Response.status, body with
  | `OK, `Stream body -> Lwt_stream.fold (^) body ""
  | `OK, `Empty       -> return ""
  | err, `Stream body -> raise (Error_code err)
  | err, _            -> raise (Error_code err)

let call_parse http_method parse_fn ?access_token ?headers ?body uri =
  let body = match body with
    | None      -> None
    | Some body -> Some (Cohttp_lwt_body.of_string body)
  in
  call_string ?access_token ?headers ?body http_method uri >>= fun response ->
  Lwt.return (parse_fn response)

let post_authentication_code app code =
  (* NOTE: The leading slash in /oauth/token is necessary. *)
  let base = Uri.with_path app.base_uri "/oauth/token" in
  let uri  = Uri.add_query_params' base [
      ("client_id", app.app_id);
      ("client_secret", app.app_secret);
      ("grant_type", "authorization_code");
      ("code", code)
    ]
  in
  call_parse `POST Inbox_j.authentication_result_of_string uri

let get_namespaces ~access_token ~app =
  call_parse ~access_token `GET Inbox_j.namespace_list_of_string (api_path app "/n")

let get_namespace ~access_token ~app id  =
  let uri = api_path app ("/n/" ^ id) in
  call_parse ~access_token `GET Inbox_j.namespace_of_string uri


(* Email APIs *)
(** Sends a message, creating a new thread. *)
let send_new_message ~access_token ~app namespace_id message =
  let body = Inbox_j.string_of_message_edit message in
  let uri = api_path app ("/n/" ^ namespace_id ^ "/send") in
  call_parse ~access_token ~body `POST Inbox_j.message_of_string uri

let create_draft ~access_token ~app namespace_id message =
  let body = Inbox_j.string_of_message_edit message in
  let uri = api_path app ("/n/" ^ namespace_id ^ "/drafts") in
  call_parse ~access_token ~body `POST Inbox_j.message_of_string uri


(* Files *)
let get_files ~access_token ~app namespace_id filters =
  let uri =
    Filter.add_query filters (api_path app ("/n/" ^ namespace_id ^ "/files"))
  in
  call_parse ~access_token `GET Inbox_j.file_list_of_string uri

(** Takes Inbox file metadata and produces a "part" for a multipart
 *  request that contains the necessary Content-Disposition and
 *  Content-Type headers.
 *)
let part_of_file content_type filename content =
  {
    Multipart.headers = [
      ("Content-Disposition",
       "form-data; name=\"" ^ filename ^ "\"; filename=\"" ^ filename ^ "\"")
      (* ("Content-Type", content_type) *)
    ];
    body = content
  }

let upload_file ~access_token ~app namespace_id content_type filename content =
  let file_part = part_of_file content_type filename content in
  let (header, body) = Multipart.request_of_parts "form-data" [file_part] in
  let headers = [
      (* ("Content-Length", string_of_int (String.length body + 6)); *)
      header;
  ]
  in
  let uri = api_path app ("/n/" ^ namespace_id ^ "/files/") in
  (* (headers, body) *)
  call_parse ~access_token ~headers ~body `POST (fun x -> x) uri

(* Calendar APIs *)
let get_calendars ~access_token ~app namespace_id =
  let uri = api_path app ("/n/" ^ namespace_id ^ "/calendars") in
  call_parse ~access_token `GET Inbox_j.calendar_list_of_string uri

let get_calendar ~access_token ~app namespace_id calendar_id=
  let uri = api_path app ("/n/" ^ namespace_id ^ "/calendars/" ^ calendar_id) in
  call_parse ~access_token `GET Inbox_j.calendar_of_string uri

let get_event ~access_token ~app namespace_id event_id =
  let uri = api_path app ("/n/" ^ namespace_id ^ "/events/" ^ event_id) in
  call_parse ~access_token `GET Inbox_j.event_of_string uri

let get_events ~access_token ~app namespace_id filters =
  let uri =
    Filter.add_query filters (api_path app ("/n/" ^ namespace_id ^ "/events"))
  in
  call_parse ~access_token `GET Inbox_j.event_list_of_string uri
