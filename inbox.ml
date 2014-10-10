open Lwt
open Cohttp.Code
open Cohttp_lwt_unix

exception Error_code of Cohttp.Code.status_code

let api_uri = Uri.of_string "https://api.inboxapp.com"

let authentication_uri app_id user_email redirect_uri =
  let base = Uri.of_string "https://www.inboxapp.com/oauth/authorize" in
  Uri.add_query_param base [
    ("client_id", app_id);
    ("response_type", "code");
    ("scope", "email");
    ("login_hint", user_email);
    ("redirect_uri", Uri.to_string redirect_uri)
  ]

let get_string ?access_token uri =
  let uri = match access_token with
    | Some token -> Uri.add_query_param' uri ("access_token", token)
    | None       -> uri
  in
  Client.get uri >>= fun (response, body) ->
  match response.Client.Response.status, body with
  | `OK, `Stream body -> Lwt_stream.fold (^) body ""
  | `OK, `Empty       -> return ""
  | err, _            -> raise (Error_code err)

let get_namespaces () = get_string (Uri.of_string "https://api.inbox.com/n")

let get_namespace id = failwith "undefined"
