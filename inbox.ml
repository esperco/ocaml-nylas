open Lwt
open Cohttp.Code
open Cohttp_lwt_unix

exception Error_code of Cohttp.Code.status_code

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

let get_namespaces () = failwith "undefined"

let get_namespace id = failwith "undefined"
