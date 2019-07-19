open Js_of_ocaml
let to_js = Js.Unsafe.inject

type element

type 'a component =
  | HtmlTagComponent of string
  | FunctionalComponent of ('a -> element)

let create_element (component_or_tag) (props) (children: element list) : element =
  let comp = match component_or_tag with
    | HtmlTagComponent s -> to_js @@ Js.string s
    | FunctionalComponent fn -> to_js @@ Js.Unsafe.callback fn
  in
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.createElement") [|
    comp;
    to_js props;
    to_js @@ Js.array (Array.of_list children)
  |]

module Html = struct
  module Attr = struct
    type mouse_event = <
      nativeEvent: Dom_html.mouseEvent Js.t Js.readonly_prop
    > Js.t

    type attr =
      | OnClick of (mouse_event -> unit)
      | Key of string
      | Style of Dom_html.cssStyleDeclaration Js.t

    let props_to_attr = function
      | OnClick fn -> ("onClick", to_js @@ Js.Unsafe.callback fn)
      | Key k -> ("key", to_js @@ k)
      | Style s -> ("style", to_js @@ s)

    let create_js_obj attrs = List.map props_to_attr attrs
      |> Array.of_list
      |> Js.Unsafe.obj

    let inline_style s: Dom_html.cssStyleDeclaration Js.t = Js.Unsafe.coerce s
  end

  let html_element_factory tag p c = create_element (HtmlTagComponent tag) (Attr.create_js_obj p) c
  let div p c = html_element_factory "div" p c
  let span p c = html_element_factory "span" p c
end

let fc (fc: 'a -> element) (props: 'a) ch = create_element (FunctionalComponent fc) props ch
let (</>) component props = fc component props
let string str: element = Js.Unsafe.eval_string (Printf.sprintf {|"%s"|} str)
let null (): element = Js.Unsafe.eval_string (Printf.sprintf {|null|})
let component_children (props: 'a Js.t): element list =
  Js.Unsafe.get props "children"

let use_effect (fn: unit -> (unit -> unit) option Js.optdef): unit =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "React.useEffect")
    [|to_js @@ Js.Unsafe.callback (fn)|]

let use_effect0 (fn: unit -> (unit -> unit) option Js.optdef): unit =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.useEffect") [|
    to_js @@ Js.Unsafe.callback fn;
    to_js @@ Js.array [||]
  |]

let use_effect1 (fn: unit -> (unit -> unit) option Js.optdef) dep1 : unit =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.useEffect") [|
    to_js @@ Js.Unsafe.callback (fn);
    to_js @@ Js.array [|dep1|]
  |]

let use_effect2 (fn: unit -> (unit -> unit) option Js.optdef) dep1 dep2 : unit =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.useEffect") [|
    to_js @@ Js.Unsafe.callback fn;
    to_js @@ Js.array [|dep1, dep2|]
  |]

let use_state (init_value: unit -> 'a): ('a * (('a -> 'a) -> unit)) =
  let state_tuple = Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "React.useState")
    [|to_js @@ Js.Unsafe.callback init_value|] in
  (Js.Unsafe.get state_tuple "0", Js.Unsafe.get state_tuple "1")