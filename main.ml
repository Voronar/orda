open Js_of_ocaml
module Html = Reactjs_ml.Html
module Attr = Reactjs_ml.Html.Attr
module Utils = Reactjs_ml.Utils
module Style = Reactjs_ml.Html.InlineStyle

type todo = {
  name: string;
  _done: bool
}
[@@deriving yojson]

type todo_list_props = {
  data: todo list
}

type ctx_type = {
  value: int;
  setValue: int -> unit;
}
let ctx = Reactjs_ml.create_context { value = 0; setValue = fun _ -> () }
let ctx_provider = Reactjs_ml.get_provider ctx

let todo_list (props: todo_list_props) =
  let cxt_value = Reactjs_ml.use_context ctx in
  Utils.Global.console_log cxt_value.value;
  Html.ul [] (List.mapi (fun i -> fun item -> 
    Html.li [
      Attr.Key (string_of_int i);
      Attr.OnClick (fun _ -> cxt_value.setValue i);
      Attr.style [
        if item._done then Style.Color "green" else Style.Color "red"
      ]
    ] [Reactjs_ml.string item.name]
  ) props.data)

type memo_test_props = {
  value: int;
}

let memo_test = Reactjs_ml.memo (fun (props: memo_test_props) ->
  print_endline "memoized component test only one render";
  Html.h1 [] [Reactjs_ml.string @@ "memo test value: " ^ string_of_int props.value])

let app () = let open Reactjs_ml in
  let (cxt_value, set_ctx_value) = Reactjs_ml.use_state (fun () -> 1) in
  let (text, setText) = Reactjs_ml.use_state (fun () -> "") in
  let (todos, _setTodos) = Reactjs_ml.use_state (fun () -> [
    { name = "go to gym"; _done = false };
    { name = "walk at forest"; _done = true }
  ]) in
  let ctx_value = Reactjs_ml.create_context_value {
    value = cxt_value;
    setValue = fun v -> set_ctx_value (fun _ -> v)
  } in

  Reactjs_ml.use_effect0 (fun _ -> (
    Js.Optdef.return @@ fun () -> (
      print_endline "buy";
    )
  ));

  ctx_provider <@> ctx_value </@> [
    Html.div [] [
      memo_test <@> { value = 22 } </@> [];
      Html.h1 [] [Reactjs_ml.string "My todos"];
      Html.input [ Attr.Value text; Attr.OnInput (fun e -> 
        let a = match (Utils.Event.keyboard_target e##.nativeEvent) with
          | Some s -> Js.to_string s##.value
          | None -> "" in
        setText (fun _ -> a)
      )];
      Html.div [] [Reactjs_ml.string @@ "" ^ text];
      todo_list <@> { data = todos } </@> []
    ]
  ]

let _start = let open Reactjs_ml in
  let el = app <@> () </@> [] in
  Reactjs_ml.Dom.render el (Dom_html.getElementById "root")