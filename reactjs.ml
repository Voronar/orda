open Js_of_ocaml

(* Element, Component *)
type element

type 'a component =
  | HtmlTagComponent of string
  | FunctionalComponent of ('a -> element)

let create_element (component_or_tag) (props) (children: element list option) : element =
  let comp = match component_or_tag with
    | HtmlTagComponent s -> Js.Unsafe.inject @@ Js.string s
    | FunctionalComponent fn -> Js.Unsafe.inject @@ Js.Unsafe.callback fn
  in
  let children_array = (match children with
    | Some c -> if List.length c = 0
      then [Js.Unsafe.inject Js.Optdef.empty]
      else List.map Js.Unsafe.inject c
    | None -> [Js.Unsafe.inject Js.Optdef.empty])
  |> Array.of_list in

  Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.createElement") (
    Array.append [|comp; Js.Unsafe.inject props|] children_array)

(* Context *)
type 'a provider_props = < value: 'a Js.readonly_prop > Js.t
type 'a context_provider = 'a provider_props -> element
type 'a context_js

let create_context (dft_value: 'a): 'a context_js =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.createContext") [| Js.Unsafe.inject dft_value|]

let create_context_value v = object%js val value = v end
let get_provider (ctx_js: 'a context_js): 'a context_provider = Js.Unsafe.get ctx_js "Provider"

let use_context (ctx: 'a context_js): 'a =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.useContext") [|Js.Unsafe.inject ctx|]

(* functional component ONLY mutable refs *)
type 'a ref_object = < current: 'a Js.prop > Js.t
let (!) (ref: 'a ref_object): 'a = Js.Unsafe.get ref "current"
let (:=) (ref: 'a ref_object) (v: 'a): unit = Js.Unsafe.set ref "current" v

let use_ref (value: 'a): 'a ref_object =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.useRef") [|Js.Unsafe.inject value|]

(* HTML building blocks *)
module Html = struct
  module InlineStyle = struct
    type style =
      | Display of string
      | Width of string
      | Height of string
      | BackgroundColor of string
      | Color of string
      | TextDecoration of string

    let props_to_attr = function
      | Display s -> ("display", Js.Unsafe.inject s)
      | Width s -> ("width", Js.Unsafe.inject s)
      | Height s -> ("height", Js.Unsafe.inject s)
      | BackgroundColor s -> ("backgroundColor", Js.Unsafe.inject s)
      | Color s -> ("color", Js.Unsafe.inject s)
      | TextDecoration s -> ("textDecoration", Js.Unsafe.inject s)
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
      | OnKeyDown of (keyboard_event -> unit)
      | OnKeyUp of (keyboard_event -> unit)
      | Key of string
      | Value of string
      | Style of Dom_html.cssStyleDeclaration Js.t
      | ClassName of string

    let props_to_attr = function
      | OnClick fn -> ("onClick", Js.Unsafe.inject @@ Js.Unsafe.callback fn)
      | OnInput fn -> ("onChange", Js.Unsafe.inject @@ Js.Unsafe.callback fn)
      | OnKeyDown fn -> ("onKeyDown", Js.Unsafe.inject @@ Js.Unsafe.callback fn)
      | OnKeyUp fn -> ("onKeyUp", Js.Unsafe.inject @@ Js.Unsafe.callback fn)
      | Key k -> ("key", Js.Unsafe.inject @@ k)
      | Style s -> ("style", Js.Unsafe.inject @@ s)
      | Value v -> ("value", Js.Unsafe.inject @@ v)
      | ClassName cn -> ("value", Js.Unsafe.inject @@ cn)

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
    [|Js.Unsafe.inject @@ Js.Unsafe.callback (fn)|]

let use_effect0 (fn: unit -> (unit -> unit) Js.optdef): unit =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.useEffect") [|
    Js.Unsafe.inject @@ Js.Unsafe.callback fn;
    Js.Unsafe.inject @@ Js.array [||]
  |]

let use_effect1 (fn: unit -> (unit -> unit) Js.optdef) dep1 : unit =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.useEffect") [|
    Js.Unsafe.inject @@ Js.Unsafe.callback (fn);
    Js.Unsafe.inject @@ Js.array [|dep1|]
  |]

let use_effect2 (fn: unit -> (unit -> unit) Js.optdef) dep1 dep2 : unit =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.useEffect") [|
    Js.Unsafe.inject @@ Js.Unsafe.callback fn;
    Js.Unsafe.inject @@ Js.array [|dep1, dep2|]
  |]

let use_state (init_value: unit -> 'a): ('a * (('a -> 'a) -> unit)) =
  let state_tuple = Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "React.useState")
    [|Js.Unsafe.inject @@ Js.Unsafe.callback init_value|] in
  (Js.Unsafe.get state_tuple "0", Js.Unsafe.get state_tuple "1")

(* Others *)
let memo (fn: 'a -> element): 'a -> element =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.memo") [| Js.Unsafe.inject @@ Js.Unsafe.callback fn|]

(* ReactDom *)
module Dom = struct
  let render (element: element) (node: Dom_html.element Js.t ): Dom_html.element Js.t =
    Js.Unsafe.fun_call (Js.Unsafe.js_expr "ReactDOM.render") [|
      Js.Unsafe.inject element;
      Js.Unsafe.inject node
    |]
end
