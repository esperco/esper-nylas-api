open Log
open Lwt

open Nylas_api_t

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

let call_string http_method ?access_token ?(headers=[]) ?body uri =
  let headers = match access_token with
    | Some token ->
       ("Authorization",
        "Basic " ^ Nlencoding.Base64.encode (token ^ ":"))::headers
    | None       -> headers
  in
  let headers = Cohttp.Header.of_list headers in
  (match body with
  | None -> return ""
  | Some b -> Cohttp_lwt_body.to_string b
  ) >>= fun b ->
  Printf.eprintf "Making Nylas API call: %s %s %s\n%!"
    (Uri.to_string uri) (Cohttp.Code.string_of_method http_method) b;
  Cohttp_lwt_unix.Client.call
    ~headers ?body http_method uri >>= fun (response, body) ->
  match response.Cohttp.Response.status, body with
  | `OK, body ->
      Cohttp_lwt_body.to_string body >>= fun s ->
      Printf.eprintf "Nylas API call succeeded with response: %s\n%!" s;
      return (Some s)
  | `Not_found, _ ->
      return None
  | `Bad_request, _ ->
      failwith "Bad Nylas request"
  | err, body ->
      Cohttp_lwt_body.to_string body >>= fun s ->
      logf `Error "Nylas API call failed with error %d: %s\n%!"
        (Cohttp.Code.code_of_status err) s;
      Http_exn.service_unavailable "3rd-party service is unavailable"

let call_parse_opt http_method parse_fn ?access_token ?headers ?body uri =
  let body = match body with
    | None      -> None
    | Some body -> Some (Cohttp_lwt_body.of_string body)
  in
  call_string ?access_token ?headers ?body http_method uri >>= function
  | None -> return None
  | Some response -> return (Some (parse_fn response))

let call_parse http_method parse_fn ?access_token ?headers ?body uri =
  let body = match body with
    | None      -> None
    | Some body -> Some (Cohttp_lwt_body.of_string body)
  in
  call_string ?access_token ?headers ?body http_method uri >>= function
  | None -> Http_exn.not_found "Resource not found"
  | Some response -> return (parse_fn response)

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
  call_parse_opt `POST Nylas_api_j.authentication_result_of_string uri

let get_account ~access_token ~app =
  let uri = api_path app "/account" in
  call_parse_opt ~access_token `GET Nylas_api_j.account_of_string uri


(* Email APIs *)

(* Threads *)
let get_threads ~access_token ~app filters =
  let uri =
    Nylas_filter.add_query filters (api_path app "/threads")
  in
  call_parse_opt ~access_token `GET Nylas_api_j.thread_list_of_string uri

let get_thread ~access_token ~app thread_id =
  let uri = api_path app ("/threads/" ^ thread_id) in
  call_parse_opt ~access_token `GET Nylas_api_j.thread_of_string uri

(* Message *)
let get_messages ~access_token ~app filters =
  let uri =
    Nylas_filter.add_query filters (api_path app "/messages")
  in
  call_parse_opt ~access_token `GET Nylas_api_j.message_list_of_string uri

let get_message ~access_token ~app message_id =
  let uri = api_path app ("/messages/" ^ message_id) in
  call_parse_opt ~access_token `GET Nylas_api_j.message_of_string uri

let get_raw_message_64 ~access_token ~app message_id =
  let uri = api_path app ("/messages/" ^ message_id ^ "/rfc2822") in
  call_parse_opt ~access_token `GET Nylas_api_j.message_raw_of_string uri

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
  call_parse_opt ~access_token ~body `POST Nylas_api_j.message_of_string uri

let send_new_raw_message ~access_token ~app body =
  let uri = api_path app "/send" in
  let headers = ["Content-Type", "message/rfc822"] in
  call_parse_opt ~access_token ~headers ~body `POST
    Nylas_api_j.message_of_string uri

(* Drafts *)
let get_drafts ~access_token ~app =
  let uri = api_path app "/drafts" in
  call_parse_opt ~access_token `GET Nylas_api_j.draft_list_of_string uri

let get_draft ~access_token ~app draft_id =
  let uri = api_path app ("/drafts/" ^ draft_id) in
  call_parse_opt ~access_token `GET Nylas_api_j.draft_of_string uri

let create_draft ~access_token ~app message =
  let body = Nylas_api_j.string_of_message_edit message in
  let uri = api_path app "/drafts" in
  call_parse_opt ~access_token ~body `POST Nylas_api_j.draft_of_string uri

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
      call_parse_opt ~access_token ~body `PUT Nylas_api_j.draft_of_string uri

let delete_draft ~access_token ~app draft_id =
  get_draft ~access_token ~app draft_id >>= function
  | None -> return None
  | Some draft ->
      let uri = api_path app ("/drafts/" ^ draft_id) in
      let dd =
        Nylas_api_v.create_draft_delete ~dd_version:draft.dr_version () in
      let body = Nylas_api_j.string_of_draft_delete dd in
      call_parse_opt ~access_token ~body `DELETE (fun x -> x) uri

let send_draft ~access_token ~app draft =
  let body = Nylas_api_j.string_of_draft_send {
    ds_draft_id = draft.dr_id;
    ds_version  = draft.dr_version
  }
  in
  let uri = api_path app "/send" in
  call_parse_opt ~access_token ~body `POST Nylas_api_j.draft_of_string uri

(* Files *)
let get_files ~access_token ~app filters =
  let uri =
    Nylas_filter.add_query filters (api_path app "/files")
  in
  call_parse_opt ~access_token `GET Nylas_api_j.file_list_of_string uri

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
  call_parse_opt
    ~access_token ~headers ~body `POST Nylas_api_j.file_list_of_string uri

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
  call_parse_opt ~access_token `GET Nylas_api_j.contact_list_of_string uri

(* Calendar APIs *)
let get_calendars ~access_token ~app =
  let uri = api_path app "/calendars" in
  call_parse_opt ~access_token `GET Nylas_api_j.calendar_list_of_string uri

let get_calendar ~access_token ~app calendar_id=
  let uri = api_path app ("/calendars/" ^ calendar_id) in
  call_parse_opt ~access_token `GET Nylas_api_j.calendar_of_string uri

let get_event ~access_token ~app event_id =
  let uri = api_path app ("/events/" ^ event_id) in
  call_parse_opt ~access_token `GET Nylas_api_j.event_of_string uri

let get_events ~access_token ~app filters =
  let uri =
    Nylas_filter.add_query filters (api_path app "/events")
  in
  call_parse_opt ~access_token `GET Nylas_api_j.event_list_of_string uri

let create_event ~access_token ~app event_edit =
  let uri = api_path app "/events" in
  let body = Nylas_api_j.string_of_event_edit event_edit in
  call_parse ~access_token ~body `POST Nylas_api_j.event_of_string uri

let update_event ~access_token ~app event_id event_edit =
  let uri = api_path app ("/events/" ^ event_id) in
  let body = Nylas_api_j.string_of_event_edit event_edit in
  call_parse_opt ~access_token ~body `PUT Yojson.Safe.from_string uri

let delete_event ~access_token ~app event_id =
  let uri = api_path app ("/events/" ^ event_id) in
  call_parse_opt ~access_token `DELETE Yojson.Safe.from_string uri

(* Delta Sync *)
let delta_sync_start ~access_token ~app timestamp =
  let uri = api_path app "/delta/generate_cursor" in
  let body = Nylas_api_j.string_of_start_time { start = timestamp } in
  call_parse_opt
    ~access_token ~body `POST Nylas_api_j.cursor_response_of_string uri

let delta_sync_update ~access_token ~app ?(exclude = []) cursor =
  let base = api_path app "/delta" in
  let with_cursor = Uri.add_query_params' base ["cursor", cursor] in
  let uri =
    if exclude = [] then with_cursor
    else
      let filter = String.concat "," exclude in
      Uri.add_query_params' with_cursor ["exclude_types", filter]
  in
  call_parse_opt ~access_token `GET Nylas_api_j.delta_page_of_string uri
