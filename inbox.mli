open Core

type app = {
  api_url    : string;
  base_url   : string;
  app_id     : string;
  app_secret : string;
}

(** Creates a URI for authenticating the user via Inbox.
 *
 *  The idea is to send your user to this URI to have them sign in
 *  through the page appropriate to their Email service. The page then
 *  redirects to the given redirect_uri with a code which should be
 *  posted to the Inbox server via `post_authentication_code` in
 *  return for an OAuth access_token.
 *)
val authentication_uri : app -> ?user_email:email -> Uri -> Uri

(** Once you have a code from `authentication_uri`, post it to Inbox
 *  with this method to get an OAuth access token.
 *)
val post_authentication_code : app -> string -> access_token Lwt.t

(** Returns a list of all the namespaces defined in the Inbox app. *)
val get_namespaces : unit -> namespace list Lwt.t

(** Return the namespace with the given id or none if it doesn't
 *  exist.
 *)
val get_namespace : namespace_id -> namespace option Lwt.t

