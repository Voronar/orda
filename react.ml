open Js_of_ocaml

(* Element, Component *)
type element
type fragment_component
type class_component

type 'a component =
  | HtmlTagComponent of string
  | FragmentComponent of fragment_component
  | ClassComponent of class_component
  | FunctionalComponent of ('a -> element)

let parse_children ch = (match ch with
  | Some c -> if List.length c = 0
    then [Js.Unsafe.inject Js.Optdef.empty]
    else List.map Js.Unsafe.inject c
  | None -> [Js.Unsafe.inject Js.Optdef.empty])
|> Array.of_list

let create_element component props (children: element list option) : element =
  let comp = match component with
    | HtmlTagComponent s -> Js.Unsafe.inject @@ Js.string s
    | FragmentComponent frc -> Js.Unsafe.inject frc
    | ClassComponent cc -> Js.Unsafe.inject cc
    | FunctionalComponent fn -> Js.Unsafe.inject @@ Js.Unsafe.callback fn
  in
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.createElement") (Array.append
    [|comp; Js.Unsafe.inject props|]
    (parse_children children))

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
      nativeEvent: Dom_html.mouseEvent Js.t Js.readonly_prop;
      preventDefault: unit -> unit Js.meth
    > Js.t
    type keyboard_event = <
      nativeEvent: Dom_html.keyboardEvent Js.t Js.readonly_prop;
      preventDefault: unit Js.meth
    > Js.t

    type 'a attr =
      | OnClick of (mouse_event -> unit)
      | OnContextMenu of (mouse_event -> unit)
      | OnInput of (keyboard_event -> unit)
      | OnKeyDown of (keyboard_event -> unit)
      | OnKeyUp of (keyboard_event -> unit)
      | Key of string
      | Ref of 'a Js.Opt.t ref_object
      | Value of string
      | Style of Dom_html.cssStyleDeclaration Js.t
      | ClassName of string

    let props_to_attr = function
      | OnClick fn -> ("onClick", Js.Unsafe.inject @@ Js.Unsafe.callback fn)
      | OnContextMenu fn -> ("onContextMenu", Js.Unsafe.inject @@ Js.Unsafe.callback fn)
      | OnInput fn -> ("onChange", Js.Unsafe.inject @@ Js.Unsafe.callback fn)
      | OnKeyDown fn -> ("onKeyDown", Js.Unsafe.inject @@ Js.Unsafe.callback fn)
      | OnKeyUp fn -> ("onKeyUp", Js.Unsafe.inject @@ Js.Unsafe.callback fn)
      | Key v -> ("key", Js.Unsafe.inject @@ v)
      | Ref v -> ("ref", Js.Unsafe.inject @@ v)
      | Style v -> ("style", Js.Unsafe.inject @@ v)
      | Value v -> ("value", Js.Unsafe.inject @@ v)
      | ClassName v -> ("className", Js.Unsafe.inject @@ v)

    let create_js_obj props_to_attr_fn attrs = List.map props_to_attr_fn attrs
      |> Array.of_list
      |> Js.Unsafe.obj

    let create_reg_attr attr = create_js_obj props_to_attr attr
    let style attr = Style (InlineStyle.create attr)
  end
  let html_factory tag at ch = create_element (HtmlTagComponent tag) (Attr.create_reg_attr at) (Some ch)
  let div (at: Dom_html.divElement Attr.attr list) ch = html_factory "div" at ch
  let span (at: Dom_html.htmlElement Attr.attr list) ch = html_factory "span" at ch
  let ul (at: Dom_html.uListElement Attr.attr list) ch = html_factory "ul" at ch
  let ol (at: Dom_html.oListElement Attr.attr list) ch = html_factory "ol" at ch
  let li (at: Dom_html.liElement Attr.attr list) ch = html_factory "li" at ch
  let table (at: Dom_html.tableElement Attr.attr list) ch = html_factory "table" at ch
  let tbody (at: Dom_html.tableElement Attr.attr list) ch = html_factory "tbody" at ch
  let thead (at: Dom_html.tableElement Attr.attr list) ch = html_factory "thead" at ch
  let tr (at: Dom_html.tableRowElement Attr.attr list) ch = html_factory "tr" at ch
  let th (at: Dom_html.tableCellElement Attr.attr list) ch = html_factory "th" at ch
  let td (at: Dom_html.tableCellElement Attr.attr list) ch = html_factory "td" at ch
  let h1 (at: Dom_html.headingElement Attr.attr list) ch = html_factory "h1" at ch
  let h2 (at: Dom_html.headingElement Attr.attr list) ch = html_factory "h2" at ch
  let h3 (at: Dom_html.headingElement Attr.attr list) ch = html_factory "h3" at ch
  let select (at: Dom_html.selectElement Attr.attr list) ch = html_factory "select" at ch
  let option (at: Dom_html.optionElement Attr.attr list) ch = html_factory "option" at ch
  let input (at: Dom_html.inputElement Attr.attr list) = create_element (HtmlTagComponent "input") (Attr.create_reg_attr at) None
  let textarea (at: Dom_html.textAreaElement Attr.attr list) = create_element (HtmlTagComponent "textarea") (Attr.create_reg_attr at) None
end

(* Component utils *)
let fc (fc: 'a -> element) (props: 'a) ch = create_element (FunctionalComponent fc) props (Some ch)
let (<@>) component props = fc component props
let (</@>) comp_props (ch: element list): element = comp_props ch
let string str: element = Js.Unsafe.eval_string (Printf.sprintf {|"%s"|} str)
let null (): element = Js.Unsafe.pure_js_expr "null"
let get_children props: element list = Js.Unsafe.get props "children"

let react_fragment: fragment_component = Js.Unsafe.js_expr "React.Fragment"
let fragment ?key ch =
  let props = match key with
    | Some s ->  Js.Unsafe.inject @@ object%js val key = Js.string s end
    | None -> Js.Unsafe.inject Js.Optdef.empty
  in
  create_element (FragmentComponent react_fragment) props (Some ch)

(* Hooks *)
let use_effect (fn: unit -> (unit -> unit) Js.optdef): unit = Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.useEffect")
  [|Js.Unsafe.inject @@ Js.Unsafe.callback fn|]

let use_effect0 (fn: unit -> (unit -> unit) Js.optdef): unit = Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.useEffect")
  [|Js.Unsafe.inject @@ Js.Unsafe.callback fn;
    Js.Unsafe.inject @@ Js.array [||]|]

let use_effect1 (fn: unit -> (unit -> unit) Js.optdef) d1 : unit = Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.useEffect")
  [|Js.Unsafe.inject @@ Js.Unsafe.callback (fn); Js.Unsafe.inject @@ Js.array [|d1|]|]

let use_effect2 (fn: unit -> (unit -> unit) Js.optdef) ((d1, d2): ('a * 'b)) : unit = Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.useEffect")
  [|Js.Unsafe.inject @@ Js.Unsafe.callback fn;
    Js.Unsafe.inject @@ Js.array [|Js.Unsafe.inject d1; Js.Unsafe.inject d2|]|]

let use_effect3 (fn: unit -> (unit -> unit) Js.optdef) ((d1, d2, d3): ('a * 'b * 'c)) : unit = Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.useEffect")
  [|Js.Unsafe.inject @@ Js.Unsafe.callback fn;
    Js.Unsafe.inject @@ Js.array [|Js.Unsafe.inject d1; Js.Unsafe.inject d2; Js.Unsafe.inject d3|]|]

let use_state (init_value: unit -> 'a): ('a * (('a -> 'a) -> unit)) =
  let state_tuple = Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.useState")
    [|Js.Unsafe.inject @@ Js.Unsafe.callback init_value|] in
  (Js.Unsafe.get state_tuple "0", Js.Unsafe.get state_tuple "1")

let use_reducer
  ?init:(init = fun (x: 'a) -> x)
  (reducer: 'a -> 'b -> 'a)
  (init_value: 'a)
:('a * ('b -> unit)) =
  let state_tuple = Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.useReducer")
    [|Js.Unsafe.inject @@ Js.Unsafe.callback reducer;
      Js.Unsafe.inject @@ Js.Unsafe.inject init_value;
      Js.Unsafe.inject @@ Js.Unsafe.callback init|] in
  (Js.Unsafe.get state_tuple "0", Js.Unsafe.get state_tuple "1")

let use_callback0 (fn: 'a -> 'b): 'a -> 'b = Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.useCallback")
  [|Js.Unsafe.inject @@ Js.Unsafe.callback fn; Js.Unsafe.inject @@ Js.array [||]|]

let use_callback1 (fn: 'a -> 'b) (d1: 'c): 'a -> 'b = Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.useCallback")
  [|Js.Unsafe.inject @@ Js.Unsafe.callback fn; Js.Unsafe.inject @@ Js.array [|d1|]|]

let use_callback2 (fn: 'a -> 'b) ((d1, d2): ('c * 'd)): 'a -> 'b = Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.useCallback")
  [|Js.Unsafe.inject @@ Js.Unsafe.callback fn;
    Js.Unsafe.inject @@ Js.array [|Js.Unsafe.inject d1; Js.Unsafe.inject d2|]|]

let use_callback3 (fn: 'a -> 'b) ((d1, d2, d3): ('c * 'd * 'e)): 'a -> 'b = Js.Unsafe.fun_call
  (Js.Unsafe.js_expr "React.useCallback") [|Js.Unsafe.inject @@ Js.Unsafe.callback fn;
    Js.Unsafe.inject @@ Js.array [|Js.Unsafe.inject d1; Js.Unsafe.inject d2; Js.Unsafe.inject d3|]|]

(* Others *)
let memo (fn: 'a -> element): 'a -> element =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "React.memo") [|Js.Unsafe.inject @@ Js.Unsafe.callback fn|]

(* ReactDom *)
module Dom = struct
  let render (element: element) (node: Dom_html.element Js.t ): Dom_html.element Js.t =
    Js.Unsafe.fun_call (Js.Unsafe.js_expr "ReactDOM.render")
    [|Js.Unsafe.inject element;
      Js.Unsafe.inject node|]
end

(* error catching helper component *)
let error_bound_component: class_component = Js.Unsafe.js_expr "ErrorBoundComponent"

let error_bound
  ?error_fallback:((error_fallback: element list) = [])
  ?on_error:(on_error = fun () -> ())
  ch =
    let props = object%js
      val errorFallback = Js.Unsafe.inject error_fallback
      method onError = Js.Unsafe.callback on_error
    end in
    create_element (ClassComponent error_bound_component) props (Some ch)
