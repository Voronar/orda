open Js_of_ocaml

type ('json_value, 'json_error, 'text_error, 'blob_error) response = <
  json: unit -> ('json_value, 'json_error) Promise.t Js.meth;
  text: unit -> (string, 'text_error) Promise.t Js.meth;
  formData: unit -> (Form.formData, 'json_error) Promise.t Js.meth;
  blob: unit -> (File.blob, 'blob_error) Promise.t Js.meth;
  status: int Js.readonly_prop
> Js.t

type request_method =
  | Get
  | Post
  | Put
  | Delete
  | Update
  | Options
  | Other of string

let parse_method = function
  | Get -> "GET"
  | Post -> "POST"
  | Put -> "PUT"
  | Delete -> "DELETE"
  | Update -> "UPDATE"
  | Options -> "OPTIONS"
  | Other s -> s

type request_mode =
  | Cors
  | NoCors
  | SameOriginMode

let parse_mode = function
  | Cors -> "cors"
  | NoCors -> "no-cors"
  | SameOriginMode -> "same-origin"

type request_body =
  | Text of string
  | FormData of Form.formData
  | Blob of File.blob
  | BufferSource of Typed_array.arrayBuffer

let parse_body = function
  | Text t -> Js.Unsafe.inject t
  | FormData fd -> Js.Unsafe.inject fd
  | Blob b -> Js.Unsafe.inject b
  | BufferSource bs -> Js.Unsafe.inject bs

type request_credentials =
  | Omit
  | SameOriginCredential
  | Include

let parse_credentials = function
  | Omit -> "omit"
  | SameOriginCredential-> "same-origin"
  | Include -> "include"

let fetch
  ?method_
  ?body
  ?credentials
  ?mode
  (url: string)
: (('json_value, 'json_error, 'text_error, 'blob_error) response, 'r_error) Promise.t =
  let method_arr = match method_ with
    | Some m -> [|("method", Js.Unsafe.inject @@ parse_method m)|] | None -> [||] in

  let mode_arr = match mode with
    | Some m -> [|("mode", Js.Unsafe.inject @@ parse_mode m)|] | None -> [||] in

  let credentials_arr = match credentials with
    | Some c -> [|("credentials", Js.Unsafe.inject @@ parse_credentials c)|] | None -> [||] in

  let body_arr = match body with
    | Some v -> [|("body", parse_body v)|] | None -> [||] in

  let opts = Array.concat
    [method_arr; mode_arr; body_arr; credentials_arr]
    |> Js.Unsafe.obj in

  Js.Unsafe.fun_call (Js.Unsafe.js_expr "fetch") [|Js.Unsafe.inject url; Js.Unsafe.inject opts|]
