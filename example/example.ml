(** Creates a very simple Cohttp server for authenticating with
 *  Inbox.
 *)

open Lwt

open Cohttp
open Cohttp_lwt_unix
open Cohttp_lwt_unix.Server

open Printf

let callback conn { Request.uri } body =
  printf "Serving a request!\n";
  printf "Uri: %s\n" (Uri.to_string uri);
  flush stdout;
  respond_string ~status:`OK ~body:(Uri.to_string uri) ()

let conn_closed _ () = printf "Connection closed\n!"
         
let config = { callback; conn_closed };;

printf "Creating server!\n";
flush stdout;
Lwt_unix.run (Server.create ~mode:`TCP config ~address:"localhost" ~port:8888)
