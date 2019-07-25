open Js_of_ocaml

(* JavaScript specific utils *)
module Global = struct
  let console_log arg: unit = Js.Unsafe.fun_call (
    Js.Unsafe.js_expr "console.log") [|Js.Unsafe.inject arg|]

  let setInterval fn d = Dom_html.window##setInterval (Js.Unsafe.callback fn) d
  let setTimeout fn d = Dom_html.window##setTimeout (Js.Unsafe.callback fn) d
end

module Event = struct
  let keyboard_target (e: Dom_html.keyboardEvent Js.t): Dom_html.inputElement Js.t option =
    match (Js.Opt.to_option e##.target) with
    | Some s -> Some (Js.Unsafe.coerce s)
    | None -> None
end
