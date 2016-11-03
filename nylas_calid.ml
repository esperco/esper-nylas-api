(*
   Abstract type for Nylas calendar IDs.
   We don't do much validation on them but having a dedicated type
   allows mixups.
*)

type t = string

let of_string s =
  match s with
  | "" -> invalid_arg "Nylas_calid.of_string"
  | s -> s

let to_string s = s

let wrap = of_string
let unwrap = to_string
