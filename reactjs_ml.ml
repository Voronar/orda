open Js_of_ocaml
let to_any_js = Js.Unsafe.inject

(* JavaScript specific utils *)
module Utils = struct
  module Global = struct
    let console_log (s): unit = Js.Unsafe.fun_call (
      Js.Unsafe.js_expr "console.log") [|to_any_js s|]

    let setInterval fn d = Dom_html.window##setInterval (Js.Unsafe.callback fn) d
    let setTimeout fn d = Dom_html.window##setTimeout (Js.Unsafe.callback fn) d
  end
  module Event = struct
    let keyboard_target (e: Dom_html.keyboardEvent Js.t): Dom_html.inputElement Js.t option =
      match (Js.Opt.to_option e##.target) with
      | Some s -> Some (Js.Unsafe.coerce s)
      | None -> None
  end
end

(* Element, Component *)
type element

type 'a component =
  | HtmlTagComponent of string
  | FunctionalComponent of ('a -> element)

let create_element (component_or_tag) (props) (children: element list option) : element =
  let comp = match component_or_tag with
    | HtmlTagComponent s -> to_any_js @@ Js.string s
    | FunctionalComponent fn -> to_any_js @@ Js.Unsafe.callback fn
  in
  let children_array = (match children with
    | Some c -> if List.length c = 0
      then [to_any_js Js.Optdef.empty]
      else List.map to_any_js c
    | None -> [to_any_js Js.Optdef.empty])
  |> Array.of_list in

  Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.createElement") (
    Array.append [|comp; to_any_js props|] children_array)

(* Context *)
type 'a provider_props = < value: 'a Js.readonly_prop > Js.t
type 'a context_provider = 'a provider_props -> element
type 'a context_js

let create_context (dft_value: 'a): 'a context_js =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.createContext") [| to_any_js dft_value|]

let create_context_value v = object%js val value = v end
let get_provider (ctx_js: 'a context_js): 'a context_provider = Js.Unsafe.get ctx_js "Provider"

(* HTML building blocks *)
module Html = struct
  module InlineStyle = struct
    type style =
      | Display of string
      | Width of string
      | Height of string
      | BackgroundColor of string
      | Color of string

    let props_to_attr = function
      | Display s -> ("display", to_any_js s)
      | Width s -> ("width", to_any_js s)
      | Height s -> ("height", to_any_js s)
      | BackgroundColor s -> ("backgroundColor", to_any_js s)
      | Color s -> ("color", to_any_js s)
    let create_js_obj attrs = List.map props_to_attr attrs
      |> Array.of_list
      |> Js.Unsafe.obj
    let create attrs: Dom_html.cssStyleDeclaration Js.t = create_js_obj attrs
  end

  module Attr = struct
    type mouse_event = <
      nativeEvent: Dom_html.mouseEvent Js.t Js.readonly_prop
    > Js.t
    type keyboard_event = <
      nativeEvent: Dom_html.keyboardEvent Js.t Js.readonly_prop
    > Js.t

    type attr =
      | OnClick of (mouse_event -> unit)
      | OnInput of (keyboard_event -> unit)
      | Key of string
      | Value of string
      | Style of Dom_html.cssStyleDeclaration Js.t
      | ClassName of string

    let props_to_attr = function
      | OnClick fn -> ("onClick", to_any_js @@ Js.Unsafe.callback fn)
      | OnInput fn -> ("onChange", to_any_js @@ Js.Unsafe.callback fn)
      | Key k -> ("key", to_any_js @@ k)
      | Style s -> ("style", to_any_js @@ s)
      | Value v -> ("value", to_any_js @@ v)
      | ClassName cn -> ("value", to_any_js @@ cn)

    let create_js_obj attrs = List.map props_to_attr attrs
      |> Array.of_list
      |> Js.Unsafe.obj
    let style attr = Style (InlineStyle.create attr)
  end
  let el_factory tag at ch = create_element (HtmlTagComponent tag) (Attr.create_js_obj at) (Some ch)
  let div at ch = el_factory "div" at ch
  let span at ch = el_factory "span" at ch
  let ul at ch = el_factory "ul" at ch
  let ol at ch = el_factory "ol" at ch
  let li at ch = el_factory "li" at ch
  let h1 at ch = el_factory "h1" at ch
  let h2 at ch = el_factory "h2" at ch
  let h3 at ch = el_factory "h3" at ch
  let input at = create_element (HtmlTagComponent "input") (Attr.create_js_obj at) None
end

(* Component utils *)
let fc (fc: 'a -> element) (props: 'a) ch = create_element (FunctionalComponent fc) props (Some ch)
let (<@>) component props = fc component props
let (</@>) comp_props (ch: element list): element = comp_props ch
let string str: element = Js.Unsafe.eval_string (Printf.sprintf {|"%s"|} str)
let null (): element = Js.Unsafe.pure_js_expr "null"
let get_children props: element list = Js.Unsafe.get props "children"

(* Hooks *)
let use_effect (fn: unit -> (unit -> unit) Js.optdef): unit =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "React.useEffect")
    [|to_any_js @@ Js.Unsafe.callback (fn)|]

let use_effect0 (fn: unit -> (unit -> unit) Js.optdef): unit =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.useEffect") [|
    to_any_js @@ Js.Unsafe.callback fn;
    to_any_js @@ Js.array [||]
  |]

let use_effect1 (fn: unit -> (unit -> unit) Js.optdef) dep1 : unit =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.useEffect") [|
    to_any_js @@ Js.Unsafe.callback (fn);
    to_any_js @@ Js.array [|dep1|]
  |]

let use_effect2 (fn: unit -> (unit -> unit) Js.optdef) dep1 dep2 : unit =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.useEffect") [|
    to_any_js @@ Js.Unsafe.callback fn;
    to_any_js @@ Js.array [|dep1, dep2|]
  |]

let use_state (init_value: unit -> 'a): ('a * (('a -> 'a) -> unit)) =
  let state_tuple = Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "React.useState")
    [|to_any_js @@ Js.Unsafe.callback init_value|] in
  (Js.Unsafe.get state_tuple "0", Js.Unsafe.get state_tuple "1")

let use_context (ctx: 'a context_js): 'a =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.useContext") [|to_any_js ctx|]

(* Others *)
let memo (fn: 'a -> element): 'a -> element =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.memo") [| to_any_js @@ Js.Unsafe.callback fn|]

(* ReactDom *)
module Dom = struct
  let render (element: element) (node: Dom_html.element Js.t ): Dom_html.element Js.t =
    Js.Unsafe.fun_call (Js.Unsafe.js_expr "ReactDOM.render") [|
      to_any_js element;
      to_any_js node
    |]
end
