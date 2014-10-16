open Inbox_t

(** Filters for retrieving multiple events. *)
type event = [
    `Limit         of int 
  | `Offset        of int 
  | `Event_id      of event_id 
  | `Calendar_id   of calendar_id 
  | `Title         of string 
  | `Description   of string 
  | `Location      of string 
  | `Starts_before of int 
  | `Starts_after  of int 
  | `Ends_before   of int 
  | `Ends_after    of int 
]

let to_param = function
  | `Limit n                 -> ("limit", string_of_int n)
  | `Offset n                -> ("offset", string_of_int n)
  | `Event_id id             -> ("event_id", id) 
  | `Calendar_id id          -> ("calendar_id", id)
  | `Title title             -> ("title", title)
  | `Description description -> ("description", description) 
  | `Location location       -> ("location", location) 
  | `Starts_before time      -> ("starts_before", string_of_int time) 
  | `Starts_after time       -> ("starts_after", string_of_int time)
  | `Ends_before time        -> ("ends_before", string_of_int time)
  | `Ends_after time         -> ("ends_after", string_of_int time)

let filter filters uri = Uri.add_query_params' uri (List.map to_param filters)
