(** This module creates multipart requests that can be used to *
 *  transmit large amounts of data or bundle multiple requests into
 *  one. Using it requires both setting an appropriate heading and
 *  submitting the body as a string.  
 *
 *  This is just a temporary home for this module until it can either
 *  be integrated into cohttp proper or spun off into its own library.
 *)

(* TODO: Is there an idiomatic way to initialize Random? *)
Random.self_init ();;

(** For simplicity (ie, no need to quote), a boundary is a
 *  70-character random string of lowercase letters and numbers.
 *)
let generate_boundary () =
  let random () = match Random.int 36 with
    | x when x < 26 -> int_of_char 'a' + x
    | x             -> x - 26 + int_of_char '0'
  in
  String.map (fun _ -> char_of_int (random ())) (String.make 70 ' ')

(** Returns the header entry with the given boundary and multipart
  * subtype. Valid subtypes include "mixed", "alternate", "form-data"
  * and others.
  *)
let header sub_type boundary =
  ("Content-Type", "multipart/" ^ sub_type ^ "; boundary=" ^ boundary)

type part = {
  headers : (string * string) list;
  body    : string
}

(** Combines a bunch of requests into parts of the multipart query. *)
let combine_requests boundary parts =
  let combine body part =
    let append a (h, v) = a ^ "\n" ^ h ^ ": " ^ v in
    let headers = List.fold_left append "" part.headers ^ "\n" in
    body ^ "\n--" ^ boundary ^ "\n" ^ headers ^ part.body ^ "\n"
  in
  List.fold_left combine "" parts ^ "\n--" ^ boundary ^ "--"

let request_of_parts sub_type parts =
  let boundary = generate_boundary () in
  (header sub_type boundary, combine_requests boundary parts)
