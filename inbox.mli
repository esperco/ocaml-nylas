open Core

(** Creates a URI for authenticating the user via Inbox. *)
val authentication_uri : app_id -> ?user_email:email -> Uri -> Uri

(** Returns a list of all the namespaces defined in the Inbox app. *)
val get_namespaces : unit -> namespace list Lwt.t

(** Return the namespace with the given id or none if it doesn't
 *  exist.
 *)
val get_namespace : namespace_id -> namespace option Lwt.t

