(** Limit and offset are used to paginate results. *)
type page = [
    `Limit  of int
  | `Offset of int
]

(** Filters that apply to both threads *and* messages. *)
type email = [
    page
  | `Subject   of string
  | `Any_email of Nylas_t.email
  | `To        of Nylas_t.email
  | `From      of Nylas_t.email
  | `Cc        of Nylas_t.email
  | `Bcc       of Nylas_t.email
  | `Tag       of Nylas_t.tag_id
  | `Filename  of string
]

(** Filters that apply only to messages and not threads. *)
type message = [
    email
  | `Thread_id of Nylas_t.thread_id
]

(** Filters that apply only to threads and not messages. *)
type thread = [
    email
  | `Last_message_before of int
  | `Last_message_after  of int
  | `Started_before      of int
  | `Started_after       of int
]

(** Filters for retrieving multiple calendar events. *)
type event = [
    page
  | `Event_id      of Nylas_t.event_id
  | `Calendar_id   of Nylas_t.calendar_id
  | `Title         of string
  | `Description   of string
  | `Location      of string
  | `Starts_before of int
  | `Starts_after  of int
  | `Ends_before   of int
  | `Ends_after    of int
]

let to_param = function
  | `Limit n                  -> ("limit", string_of_int n)
  | `Offset n                 -> ("offset", string_of_int n)

  | `Subject s                -> ("subject", s)
  | `Any_email e              -> ("any", e)
  | `To e                     -> ("to", e)
  | `From e                   -> ("from", e)
  | `Cc e                     -> ("cc", e)
  | `Bcc e                    -> ("bcc", e)
  | `Tag id                   -> ("tag", id)
  | `Filename f               -> ("filename", f)

  | `Thread_id id             -> ("thread_id", id)

  | `Last_message_before time -> ("last_message_before", string_of_int time)
  | `Last_message_after time  -> ("last_message_after", string_of_int time)
  | `Started_before time      -> ("started_before", string_of_int time)
  | `Started_after time       -> ("started_after", string_of_int time)

  | `Event_id id              -> ("event_id", id)
  | `Calendar_id id           -> ("calendar_id", id)
  | `Title title              -> ("title", title)
  | `Description description  -> ("description", description)
  | `Location location        -> ("location", location)
  | `Starts_before time       -> ("starts_before", string_of_int time)
  | `Starts_after time        -> ("starts_after", string_of_int time)
  | `Ends_before time         -> ("ends_before", string_of_int time)
  | `Ends_after time          -> ("ends_after", string_of_int time)

let add_query filters uri = Uri.add_query_params' uri (List.map to_param filters)
