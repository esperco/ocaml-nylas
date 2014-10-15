(** Creates a very simple Cohttp server for authenticating with
 *  Inbox.
 *)

open Lwt

open Cohttp
open Cohttp_lwt_unix
open Cohttp_lwt_unix.Server

open Printf

open App_config

let callback conn { Request.uri } body =
  printf "Serving a request!\n";
  printf "Uri: %s\n" (Uri.to_string uri);
  flush stdout;
  match Uri.get_query_param uri "code" with
  | None      -> respond_string ~status:`OK ~body:("Not authorized!\n No code returned.") ()
  | Some code ->
     Inbox.post_authentication_code app code >>= fun { Core_j.access_token } -> 
     respond_string ~status:`OK ~body:(sprintf "Authorized! Response: %s." access_token) ()

let conn_closed _ () = printf "Connection closed\n!"
         
let config = { callback; conn_closed };;

printf "Creating server!\n";
flush stdout;
Lwt_unix.run (Server.create ~mode:`TCP config ~address:"10.136.63.208" ~port:8042)
