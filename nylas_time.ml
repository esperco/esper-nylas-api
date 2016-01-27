open Nylas_api_t

exception Malformed_time of [ time_type | `Some_time ]

(** Turns a some_time record full of options into a clean record in
 *  the times variant.
 *)
let clean_some_time some_time = match some_time with
  | {st_obj = Some `Time; st_time = Some t_time} ->
      `Time {t_obj = Some `Time; t_time}
  | {st_obj = Some `Timespan; st_start_time = Some ts_start_time;
                              st_end_time = Some ts_end_time} ->
      `Timespan {ts_obj = Some `Timespan; ts_start_time; ts_end_time}
  | {st_obj = Some `Date; st_date = Some d_date} ->
      `Date {d_obj = Some `Date; d_date}
  | {st_obj = Some `Datespan; st_start_date = Some ds_start_date;
                              st_end_date = Some ds_end_date} ->
      `Datespan {ds_obj = Some `Datespan; ds_start_date; ds_end_date}

  | {st_obj = Some (#time_type as typ)} -> raise (Malformed_time typ)
  | {st_obj = None} -> raise (Malformed_time `Some_time)
