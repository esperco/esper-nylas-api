(*
   Nylas API client
*)

open Printf
open Log
open Lwt

open Nylas_api_t

module Http = Util_http_client

exception Error_code of Cohttp.Code.status_code

(** Default URIs, suitable for hosted Nylas instances. *)
let api_uri  = Uri.of_string "https://api.nylas.com"

let api_path { Nylas_app.api_uri } path = Uri.with_path api_uri path

let authentication_uri ?state app user_email redirect_uri =
  let uri = api_path app "oauth/authorize" in
  let state =
    match state with
    | None -> []
    | Some x -> [ "state", x]
  in
  let required_param = [
    "client_id", app.Nylas_app.app_id;
    "response_type", "code";
    "scope", "email";
    "login_hint", user_email;
    "redirect_uri", Uri.to_string redirect_uri;
  ] in
  Uri.add_query_params' uri (required_param @ state)

let make_headers ?access_token ?(headers=[]) () =
  match access_token with
  | Some token ->
      ("Authorization",
       "Basic " ^ Nlencoding.Base64.encode (token ^ ":"))::headers
  | None -> headers

let handle_response status headers body parse_body =
  match status, body with
  | `OK, body ->
      return (Some (parse_body body))
  | `Not_found, _ ->
      return None
  | `Bad_request, _ ->
      (* Hopefully temporary band-aid for Nylas bug.
         Nylas in some cases returns event IDs
         that are rejected in other requests
         such as bd9usrpwfh6p5r9naicge3xvp_20160826T040000Z
         This was reported to Nylas under the subject
         "Invalid event ID for instance of recurring event"
      *)
      let error_msg =
        sprintf "Bad Nylas request. Response body: %s" body
      in
      Apputil_error.report_error "Bad Nylas request" error_msg >>= fun () ->
      return None
  | err, body ->
      logf `Error "Nylas API call failed with error %d: %s\n%!"
        (Cohttp.Code.code_of_status err) body;
      Http_exn.service_unavailable "3rd-party service is unavailable"

let get_opt ?access_token ?headers uri parse_body =
  let headers = make_headers ?access_token ?headers () in
  Http.get ~headers uri >>= fun (status, headers, body) ->
  handle_response status headers body parse_body

let delete_opt ?access_token ?headers ?body uri parse_body =
  let headers = make_headers ?access_token ?headers () in
  Http.delete ~headers ?body uri >>= fun (status, headers, body) ->
  handle_response status headers body parse_body

let post_opt ?access_token ?headers ?body uri parse_body =
  let headers = make_headers ?access_token ?headers () in
  Http.post ~headers ?body uri >>= fun (status, headers, body) ->
  handle_response status headers body parse_body

let put_opt ?access_token ?headers ?body uri parse_body =
  let headers = make_headers ?access_token ?headers () in
  Http.put ~headers ?body uri >>= fun (status, headers, body) ->
  handle_response status headers body parse_body

let not_found_is_an_error = function
  | None -> Http_exn.not_found `Nylas_not_found "Resource not found"
  | Some x -> return x

let get ?access_token ?headers uri parse_body =
  get_opt ?access_token ?headers uri parse_body >>=
  not_found_is_an_error

let delete ?access_token ?headers ?body uri parse_body =
  delete_opt ?access_token ?headers ?body uri parse_body >>=
  not_found_is_an_error

let post ?access_token ?headers ?body uri parse_body =
  post_opt ?access_token ?headers ?body uri parse_body >>=
  not_found_is_an_error

let put ?access_token ?headers ?body uri parse_body =
  put_opt ?access_token ?headers ?body uri parse_body >>=
  not_found_is_an_error

let post_authentication_code app code =
  (* NOTE: The leading slash in /oauth/token is necessary. *)
  let base = api_path app "/oauth/token" in
  let uri  = Uri.add_query_params' base [
      ("client_id", app.Nylas_app.app_id);
      ("client_secret", app.Nylas_app.app_secret);
      ("grant_type", "authorization_code");
      ("code", code)
    ]
  in
  post_opt uri Nylas_api_j.authentication_result_of_string

let get_account ~access_token ~app =
  let uri = api_path app "/account" in
  get_opt ~access_token uri Nylas_api_j.account_of_string


(* Email APIs *)

(* Threads *)
let get_threads ~access_token ~app filters =
  let uri =
    Nylas_filter.add_query filters (api_path app "/threads")
  in
  get_opt ~access_token uri Nylas_api_j.thread_list_of_string

let get_thread ~access_token ~app thread_id =
  let uri = api_path app ("/threads/" ^ thread_id) in
  get_opt ~access_token uri Nylas_api_j.thread_of_string

(* Message *)
let get_messages ~access_token ~app filters =
  let uri =
    Nylas_filter.add_query filters (api_path app "/messages")
  in
  get_opt ~access_token uri Nylas_api_j.message_list_of_string

let get_message ~access_token ~app message_id =
  let uri = api_path app ("/messages/" ^ message_id) in
  get_opt ~access_token uri Nylas_api_j.message_of_string

let get_raw_message_64 ~access_token ~app message_id =
  let uri = api_path app ("/messages/" ^ message_id ^ "/rfc2822") in
  get_opt ~access_token uri Nylas_api_j.message_raw_of_string

let get_raw_message ~access_token ~app message_id =
  get_raw_message_64 ~access_token ~app message_id >>= function
  | None -> return None
  | Some { mr_rfc2822 } -> return (Some (Nlencoding.Base64.decode mr_rfc2822))

let get_raw_message_mime ~access_token ~app message_id =
  get_raw_message ~access_token ~app message_id >>= function
  | None -> return None
  | Some str ->
      let input =
        new Nlstream.input_stream (new Nlchannels.input_string str) in
      return (Some (Nlmime.read_mime_message input))

let get_message_id_mime ~access_token ~app message_id =
  get_raw_message_mime ~access_token ~app message_id >>= function
  | None -> return None
  | Some (headers, _) ->
      let id =
        try Some (List.assoc "Message-Id" headers#fields)
        with Not_found -> None
      in
      return id

let get_thread_messages ~access_token ~app thread =
  get_messages ~access_token ~app [`Thread_id thread.tr_id]

let send_new_message ~access_token ~app message =
  let body = Nylas_api_j.string_of_message_edit message in
  let uri = api_path app "/send" in
  post_opt ~access_token ~body uri Nylas_api_j.message_of_string

let send_new_raw_message ~access_token ~app body =
  let uri = api_path app "/send" in
  let headers = ["Content-Type", "message/rfc822"] in
  post_opt ~access_token ~headers ~body uri Nylas_api_j.message_of_string

(* Drafts *)
let get_drafts ~access_token ~app =
  let uri = api_path app "/drafts" in
  get_opt ~access_token uri Nylas_api_j.draft_list_of_string

let get_draft ~access_token ~app draft_id =
  let uri = api_path app ("/drafts/" ^ draft_id) in
  get_opt ~access_token uri Nylas_api_j.draft_of_string

let create_draft ~access_token ~app message =
  let body = Nylas_api_j.string_of_message_edit message in
  let uri = api_path app "/drafts" in
  post_opt ~access_token ~body uri Nylas_api_j.draft_of_string

let reply_draft ~access_token ~app thread_id message =
  let message =
    { message with me_subject = None; me_thread_id = Some thread_id } in
  create_draft ~access_token ~app message

let update_draft ~access_token ~app draft_id draft_edit =
  get_draft ~access_token ~app draft_id >>= function
  | None -> return None
  | Some { dr_version } ->
      let draft_edit = { draft_edit with de_version = Some dr_version } in
      let body = Nylas_api_j.string_of_draft_edit draft_edit in
      let uri = api_path app ("/drafts/" ^ draft_id) in
      put_opt ~access_token ~body uri Nylas_api_j.draft_of_string

let delete_draft ~access_token ~app draft_id =
  get_draft ~access_token ~app draft_id >>= function
  | None -> return None
  | Some draft ->
      let uri = api_path app ("/drafts/" ^ draft_id) in
      let dd =
        Nylas_api_v.create_draft_delete ~dd_version:draft.dr_version () in
      let body = Nylas_api_j.string_of_draft_delete dd in
      delete_opt ~access_token ~body uri (fun x -> x)

let send_draft ~access_token ~app draft =
  let body = Nylas_api_j.string_of_draft_send {
    ds_draft_id = draft.dr_id;
    ds_version  = draft.dr_version
  }
  in
  let uri = api_path app "/send" in
  post_opt ~access_token ~body uri Nylas_api_j.draft_of_string

(* Files *)
let get_files ~access_token ~app filters =
  let uri =
    Nylas_filter.add_query filters (api_path app "/files")
  in
  get_opt ~access_token uri Nylas_api_j.file_list_of_string

let part_of_file content_type filename content =
  {
    Nylas_multipart.headers = [
      ("Content-Disposition",
       "form-data; name=\"" ^ filename ^ "\"; filename=\"" ^ filename ^ "\"");
      ("Content-Type", content_type)
    ];
    body = content
  }

let upload_file ~access_token ~app content_type filename content =
  let file_part = part_of_file content_type filename content in
  let (header, body) =
    Nylas_multipart.request_of_parts "form-data" [file_part] in
  let headers = [
      header;
  ]
  in
  let uri = api_path app "/files/" in
  post_opt
    ~access_token ~headers ~body uri Nylas_api_j.file_list_of_string

let attach_file ~access_token ~app file_id draft_id =
  get_draft ~access_token ~app draft_id >>= function
  | None -> return None
  | Some { dr_files } ->
      let file_ids = List.map (fun { fi_id } -> fi_id) dr_files in
      let draft_edit =
        Nylas_api_v.create_draft_edit ~de_file_ids:(file_id::file_ids) ()
      in
      update_draft ~access_token ~app draft_id draft_edit

(* TODO: Better error handling. *)
let send_with_file ~access_token ~app message content_type filename content =
  create_draft ~access_token ~app message >>= function
  | None -> return None
  | Some draft ->
      upload_file ~access_token ~app content_type filename content >>= function
      | None | Some [] -> return None
      | Some (file :: _) ->
          attach_file ~access_token ~app file.fi_id draft.dr_id >>= function
          | None -> return None
          | Some draft ->
              send_draft ~access_token ~app draft >>= function
              | None -> return None
              | x -> return x

(* Contacts APIs *)
let get_contacts ~access_token ~app filters =
  let uri =
    Nylas_filter.add_query filters (api_path app "/contacts")
  in
  get_opt ~access_token uri Nylas_api_j.contact_list_of_string

(* Calendar APIs *)
let get_calendars ~access_token ~app =
  let uri = api_path app "/calendars" in
  get_opt ~access_token uri Nylas_api_j.calendar_list_of_string

let get_calendar ~access_token ~app calendar_id=
  let uri = api_path app ("/calendars/" ^ calendar_id) in
  get_opt ~access_token uri Nylas_api_j.calendar_of_string

let get_event ~access_token ~app event_id =
  let uri = api_path app ("/events/" ^ event_id) in
  get_opt ~access_token uri Nylas_api_j.event_of_string

let get_events ~access_token ~app filters =
  let uri =
    Nylas_filter.add_query filters (api_path app "/events")
  in
  get_opt ~access_token uri Nylas_api_j.event_list_of_string

let create_event ~access_token ~app event_edit =
  let uri = api_path app "/events" in
  let body = Nylas_api_j.string_of_event_edit event_edit in
  post ~access_token ~body uri Nylas_api_j.event_of_string

let update_event ~access_token ~app event_id event_edit =
  let uri = api_path app ("/events/" ^ event_id) in
  let body = Nylas_api_j.string_of_event_edit event_edit in
  put_opt ~access_token ~body uri Yojson.Safe.from_string

let delete_event ~access_token ~app event_id =
  let uri = api_path app ("/events/" ^ event_id) in
  delete_opt ~access_token uri Yojson.Safe.from_string

(* Delta Sync *)
let delta_sync_start ~access_token ~app timestamp =
  let uri = api_path app "/delta/generate_cursor" in
  let body = Nylas_api_j.string_of_start_time { start = timestamp } in
  post_opt
    ~access_token ~body uri Nylas_api_j.cursor_response_of_string

let delta_sync_update ~access_token ~app ?(exclude = []) cursor =
  let base = api_path app "/delta" in
  let with_cursor = Uri.add_query_params' base ["cursor", cursor] in
  let uri =
    if exclude = [] then with_cursor
    else
      let filter = String.concat "," exclude in
      Uri.add_query_params' with_cursor ["exclude_types", filter]
  in
  get_opt ~access_token uri Nylas_api_j.delta_page_of_string
