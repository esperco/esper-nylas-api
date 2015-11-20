(**
   Nylas API client.
   Full documentation is at https://nylas.com/docs/platform
*)

exception Error_code of Cohttp.Code.status_code

val authentication_uri : Nylas_app.t -> string -> Uri.t -> Uri.t

val post_authentication_code :
  Nylas_app.t -> string -> Nylas_api_t.authentication_result option Lwt.t

val get_account :
  access_token:string -> app:Nylas_app.t -> Nylas_api_t.account option Lwt.t

val get_threads :
  access_token:string ->
  app:Nylas_app.t ->
  Nylas_filter.thread list -> Nylas_api_t.thread_list option Lwt.t

val get_thread :
  access_token:string ->
  app:Nylas_app.t -> string -> Nylas_api_t.thread option Lwt.t

val get_messages :
  access_token:string ->
  app:Nylas_app.t ->
  Nylas_filter.message list -> Nylas_api_t.message_list option Lwt.t

val get_message :
  access_token:string ->
  app:Nylas_app.t -> string -> Nylas_api_t.message option Lwt.t

(** Returns the rfc2822 message, which is encoded as a base-64 string. *)
val get_raw_message_64 :
  access_token:string ->
  app:Nylas_app.t -> string -> Nylas_api_t.message_raw option Lwt.t

(** Gets the raw message as a normal string. *)
val get_raw_message :
  access_token:string -> app:Nylas_app.t -> string -> string option Lwt.t

(** Gets the raw message and parses it into a `complex_mime_message'. *)
val get_raw_message_mime :
  access_token:string ->
  app:Nylas_app.t -> string -> Nlmime.complex_mime_message option Lwt.t

(** Gets the global Message-id, if one exists. *)
val get_message_id_mime :
  access_token:string -> app:Nylas_app.t -> string -> string option Lwt.t

val get_thread_messages :
  access_token:string ->
  app:Nylas_app.t ->
  Nylas_api_t.thread -> Nylas_api_t.message_list option Lwt.t

(** Sends a message, creating a new thread. *)
val send_new_message :
  access_token:string ->
  app:Nylas_app.t ->
  Nylas_api_t.message_edit -> Nylas_api_t.message option Lwt.t

val get_drafts :
  access_token:string ->
  app:Nylas_app.t -> Nylas_api_t.draft_list option Lwt.t

val get_draft :
  access_token:string ->
  app:Nylas_app.t -> string -> Nylas_api_t.draft option Lwt.t

val create_draft :
  access_token:string ->
  app:Nylas_app.t ->
  Nylas_api_t.message_edit -> Nylas_api_t.draft option Lwt.t

(** Create a draft with the given message, replying to the specified
 *  thread. This clears the message's subject, because messages
 *  replying to a thread have their subject set automatically by the
 *  Nylas API.
 *)
val reply_draft :
  access_token:string ->
  app:Nylas_app.t ->
  Nylas_api_t.thread_id ->
  Nylas_api_t.message_edit -> Nylas_api_t.draft option Lwt.t

(** Updates the *latest version* of the given file. *)
val update_draft :
  access_token:string ->
  app:Nylas_app.t ->
  string -> Nylas_api_t.draft_edit -> Nylas_api_t.draft option Lwt.t

(** Deletes the latest version of the specified draft. *)
val delete_draft :
  access_token:string -> app:Nylas_app.t -> string -> string option Lwt.t

val send_draft :
  access_token:string ->
  app:Nylas_app.t -> Nylas_api_t.draft -> Nylas_api_t.draft option Lwt.t

val get_files :
  access_token:string ->
  app:Nylas_app.t ->
  Nylas_filter.file list -> Nylas_api_t.file_list option Lwt.t

(** Takes Nylas file metadata and produces a "part" for a multipart
 *  request that contains the necessary Content-Disposition and
 *  Content-Type headers.
 *)
val part_of_file : string -> string -> string -> Nylas_multipart.part

val upload_file :
  access_token:string ->
  app:Nylas_app.t ->
  string -> string -> string -> Nylas_api_t.file_list option Lwt.t

val attach_file :
  access_token:string ->
  app:Nylas_app.t ->
  Nylas_api_t.file_id -> string -> Nylas_api_t.draft option Lwt.t

val send_with_file :
  access_token:string ->
  app:Nylas_app.t ->
  Nylas_api_t.message_edit ->
  string -> string -> string -> Nylas_api_t.draft option Lwt.t

val get_calendars :
  access_token:string ->
  app:Nylas_app.t -> Nylas_api_t.calendar_list option Lwt.t

val get_calendar :
  access_token:string ->
  app:Nylas_app.t -> string -> Nylas_api_t.calendar option Lwt.t


val get_event :
  access_token:string ->
  app:Nylas_app.t -> string -> Nylas_api_t.event option Lwt.t

val get_events :
  access_token:string ->
  app:Nylas_app.t ->
  Nylas_filter.event list -> Nylas_api_t.event_list option Lwt.t

val create_event :
  access_token:string ->
  app:Nylas_app.t -> Nylas_api_t.event_edit -> Nylas_api_t.event option Lwt.t

val update_event :
  access_token:string ->
  app:Nylas_app.t ->
  string -> Nylas_api_t.event_edit -> Yojson.Safe.json option Lwt.t

val delete_event :
  access_token:string ->
  app:Nylas_app.t -> string -> Yojson.Safe.json option Lwt.t

val delta_sync_start :
  access_token:string ->
  app:Nylas_app.t ->
  Nylas_api_t.timestamp -> Nylas_api_t.cursor_response option Lwt.t

val delta_sync_update :
  access_token:string ->
  app:Nylas_app.t ->
  ?exclude:string list -> string -> Nylas_api_t.delta_page option Lwt.t
