open Lwt
open Cohttp
open Cohttp.Code
open Cohttp_lwt_unix

open Inbox_t

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
  match response.Cohttp.Response.status, body with
  | `OK, `Stream body -> Lwt_stream.fold (^) body ""
  | `OK, `Empty       -> return ""
  | err, `Stream body -> Lwt_stream.fold (^) body ""
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

(* Threads *)
let get_threads ~access_token ~app namespace_id filters =
  let uri =
    Filter.add_query filters (api_path app ("/n/" ^ namespace_id ^ "/threads"))
  in
  call_parse ~access_token `GET Inbox_j.thread_list_of_string uri

let get_thread ~access_token ~app namespace_id thread_id =
  let uri = api_path app ("/n/" ^ namespace_id ^ "/threads/" ^ thread_id) in
  call_parse ~access_token `GET Inbox_j.thread_of_string uri

(* Message *)
let get_messages ~access_token ~app namespace_id filters =
  let uri =
    Filter.add_query filters (api_path app ("/n/" ^ namespace_id ^ "/messages"))
  in
  call_parse ~access_token `GET Inbox_j.message_list_of_string uri

let get_message ~access_token ~app namespace_id message_id =
  let uri = api_path app ("/n/" ^ namespace_id ^ "/messages/" ^ message_id) in
  call_parse ~access_token `GET Inbox_j.message_of_string uri

(** Returns the rfc2822 message, which is encoded as a base-64 string. *)
let get_raw_message_64 ~access_token ~app namespace_id message_id =
  let uri = api_path app ("/n/" ^ namespace_id ^ "/messages/" ^ message_id ^ "/rfc2822") in
  call_parse ~access_token `GET Inbox_j.message_raw_of_string uri

(** Gets the raw message as a normal string. *)
let get_raw_message ~access_token ~app namespace_id message_id =
  get_raw_message_64 ~access_token ~app namespace_id message_id >>= fun { mr_rfc2822 } ->
  return (Base64.decode mr_rfc2822)

(** Gets the raw message and parses it into a `complex_mime_message'. *)
let get_raw_message_mime ~access_token ~app namespace_id message_id =
  get_raw_message ~access_token ~app namespace_id message_id >>= fun str ->
  let input = new Nlstream.input_stream (new Nlchannels.input_string str) in
  return (Nlmime.read_mime_message input)

(** Gets the global Message-id, if one exists. *)
let get_message_id_mime ~access_token ~app namespace_id message_id =
  get_raw_message_mime ~access_token ~app namespace_id message_id >>= fun (headers, _) ->
  let id =
    try Some (List.assoc "Message-Id" headers#fields) with Not_found -> None
  in
  return id

let get_thread_messages ~access_token ~app namespace_id thread =
  get_messages ~access_token ~app namespace_id [`Thread_id thread.tr_id]

(** Sends a message, creating a new thread. *)
let send_new_message ~access_token ~app namespace_id message =
  let body = Inbox_j.string_of_message_edit message in
  let uri = api_path app ("/n/" ^ namespace_id ^ "/send") in
  call_parse ~access_token ~body `POST Inbox_j.message_of_string uri

(* Drafts *)
let get_drafts ~access_token ~app namespace_id =
  let uri = api_path app ("/n/" ^ namespace_id ^ "/drafts") in
  call_parse ~access_token `GET Inbox_j.draft_list_of_string uri

let get_draft ~access_token ~app namespace_id draft_id =
  let uri = api_path app ("/n/" ^ namespace_id ^ "/drafts/" ^ draft_id) in
  call_parse ~access_token `GET Inbox_j.draft_of_string uri
  
let create_draft ~access_token ~app namespace_id message =
  let body = Inbox_j.string_of_message_edit message in
  let uri = api_path app ("/n/" ^ namespace_id ^ "/drafts") in
  call_parse ~access_token ~body `POST Inbox_j.draft_of_string uri

(** Create a draft with the given message, replying to the specified
 *  thread. This clears the message's subject, because messages
 *  replying to a thread have their subject set automatically by the
 *  Inbox API.
 *)
let reply_draft ~access_token ~app namespace_id thread_id message =
  let message = { message with me_subject = None; me_thread_id = Some thread_id } in
  create_draft ~access_token ~app namespace_id message

(** Updates the *latest version* of the given file. *)
let update_draft ~access_token ~app namespace_id draft_id draft_edit =
  get_draft ~access_token ~app namespace_id draft_id >>= fun { dr_version } ->
  let draft_edit = { draft_edit with de_version = Some dr_version } in
  let body = Inbox_j.string_of_draft_edit draft_edit in
  let uri = api_path app ("/n/" ^ namespace_id ^ "/drafts/" ^ draft_id) in
  call_parse ~access_token ~body `PUT Inbox_j.draft_of_string uri

(** Deletes the latest version of the specified draft. *)
let delete_draft ~access_token ~app namespace_id draft_id =
  get_draft ~access_token ~app namespace_id draft_id >>= fun draft ->
  let uri = api_path app ("/n/" ^ namespace_id ^ "/drafts/" ^ draft_id) in
  let dd = Inbox_v.create_draft_delete ~dd_version:draft.dr_version () in
  let body = Inbox_j.string_of_draft_delete dd in
  call_parse ~access_token ~body `DELETE (fun x -> x) uri

let send_draft ~access_token ~app namespace_id draft =
  let body = Inbox_j.string_of_draft_send {
    ds_draft_id = draft.dr_id;
    ds_version  = draft.dr_version
  }
  in
  let uri = api_path app ("/n/" ^ namespace_id ^ "/send") in
  call_parse ~access_token ~body `POST Inbox_j.draft_of_string uri

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
       "form-data; name=\"" ^ filename ^ "\"; filename=\"" ^ filename ^ "\"");
      ("Content-Type", content_type)
    ];
    body = content
  }

let upload_file ~access_token ~app namespace_id content_type filename content =
  let file_part = part_of_file content_type filename content in
  let (header, body) = Multipart.request_of_parts "form-data" [file_part] in
  let headers = [
      header;
  ]
  in
  let uri = api_path app ("/n/" ^ namespace_id ^ "/files/") in
  call_parse ~access_token ~headers ~body `POST Inbox_j.file_list_of_string uri

let attach_file ~access_token ~app namespace_id file_id draft_id =
  get_draft ~access_token ~app namespace_id draft_id >>= fun { dr_files } ->
  let file_ids = List.map (fun { fi_id } -> fi_id) dr_files in
  let draft_edit =
    Inbox_v.create_draft_edit ~de_file_ids:(file_id::file_ids) ()
  in
  update_draft ~access_token ~app namespace_id draft_id draft_edit

(* TODO: Better error handling. *)
let send_with_file ~access_token ~app namespace_id message content_type filename content =
  create_draft ~access_token ~app namespace_id message >>= fun draft ->
  upload_file ~access_token ~app namespace_id content_type filename content >>= function
    | []      -> return None
    | file::_ -> attach_file ~access_token ~app namespace_id file.fi_id draft.dr_id >>= fun draft ->
  send_draft ~access_token ~app namespace_id draft >>= fun draft ->
  return (Some draft)

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
