open Inbox_t

exception Malformed_time of [ time_type | `Some_time ]

(** Turns a some_time record full of options into a clean record in
 *  the times variant.
 *)
let clean_some_time some_time = match some_time.st_obj with
  | Some `Time -> (match some_time.st_time with
    | Some t_time -> `Time { t_obj = Some `Time; t_time }
    | None        -> raise (Malformed_time `Time))
  | Some `Timespan -> (match some_time.st_start_time, some_time.st_end_time with
    | Some ts_start_time, Some ts_end_time ->
       `Timespan { ts_obj = Some `Timespan; ts_start_time; ts_end_time }
    | _, _ -> raise (Malformed_time `Timespan))
  | Some `Date ->
      (match some_time.st_date with
      | Some d_start_date -> `Date { d_obj = Some `Date; d_start_date }
      | None ->
          (match some_time.st_start_date with
          | Some d_start_date -> `Date { d_obj = Some `Date; d_start_date }
          | None              -> raise (Malformed_time `Date)))
  | Some `Datespan -> (match some_time.st_start_date, some_time.st_end_date with
    | Some ds_start_date, Some ds_end_date ->
       `Datespan { ds_obj = Some `Datespan; ds_start_date; ds_end_date }
    | _, _ -> raise (Malformed_time `Datespan))
  | None -> raise (Malformed_time `Some_time)
