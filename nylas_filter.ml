type timestamp = float

(** Limit and offset are used to paginate results. *)
type page = [
    `Limit  of int
  | `Offset of int
]

(** Filters that apply to both threads *and* messages. *)
type email = [
    page
  | `Subject   of string
  | `Any_email of Nylas_api_t.email
  | `To        of Nylas_api_t.email
  | `From      of Nylas_api_t.email
  | `Cc        of Nylas_api_t.email
  | `Bcc       of Nylas_api_t.email
  | `Tag       of Nylas_api_t.tag_id
  | `Filename  of string
]

(** Filters that apply only to messages and not threads. *)
type message = [
    email
  | `Thread_id of Nylas_api_t.thread_id
]

(** Filters that apply only to threads and not messages. *)
type thread = [
    email
  | `Last_message_before of timestamp
  | `Last_message_after  of timestamp
  | `Started_before      of timestamp
  | `Started_after       of timestamp
]

(** Filters for retrieving multiple calendar events. *)
type event = [
    page
  | `Event_id      of Nylas_api_t.event_id
  | `Calendar_id   of Nylas_api_t.calendar_id
  | `Title         of string
  | `Description   of string
  | `Location      of string
  | `Starts_before of timestamp
  | `Starts_after  of timestamp
  | `Ends_before   of timestamp
  | `Ends_after    of timestamp
  | `Expand_recurring of bool
  | `Show_canceled    of bool
]

type file = [
  | `Filename     of string
  | `Message_id   of string
  | `Content_type of string
]

let to_param filter =
  let timestamp x = Printf.sprintf "%.0f" x in
  match filter with
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

  | `Last_message_before time -> ("last_message_before", timestamp time)
  | `Last_message_after time  -> ("last_message_after", timestamp time)
  | `Started_before time      -> ("started_before", timestamp time)
  | `Started_after time       -> ("started_after", timestamp time)

  | `Event_id id              -> ("event_id", id)
  | `Calendar_id id           -> ("calendar_id", id)
  | `Title title              -> ("title", title)
  | `Description description  -> ("description", description)
  | `Location location        -> ("location", location)
  | `Starts_before time       -> ("starts_before", timestamp time)
  | `Starts_after time        -> ("starts_after", timestamp time)
  | `Ends_before time         -> ("ends_before", timestamp time)
  | `Ends_after time          -> ("ends_after", timestamp time)
  | `Expand_recurring b       -> ("expand_recurring", string_of_bool b)
  | `Show_canceled b          -> ("show_canceled", string_of_bool b)

  | `Content_type ct          -> ("content_type", ct)
  | `Message_id msgid         -> ("message_id", msgid)

let add_query filters uri =
  Uri.add_query_params' uri (List.map to_param filters)
