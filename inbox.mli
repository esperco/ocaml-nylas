open Core

(** Returns a list of all the namespaces defined in the Inbox app. *)
val get_namespaces : unit -> namespace list Lwt.t

