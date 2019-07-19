open Js_of_ocaml

let render (element: Reactjs_ml.element) (node: Dom_html.element Js.t ): Dom_html.element Js.t =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "ReactDOM.render") [|
    Js.Unsafe.inject element;
    Js.Unsafe.inject node
  |]