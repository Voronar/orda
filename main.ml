open! Js_of_ocaml
module Html = Jsoo_react.Html
module Attr = Jsoo_react.Html.Attr
module Style = Jsoo_react.Html.InlineStyle

let console_log = Jsoo_browser.Global.console_log

type todo = {
  name: string;
  _done: bool
}
[@@deriving yojson]


type ctx_type = {
  value: int;
  setValue: int -> unit;
}
let ctx = Jsoo_react.create_context { value = 0; setValue = fun _ -> () }
let ctx_provider = Jsoo_react.get_provider ctx

type todo_list_props = {
  todos: todo list;
  on_item_click: int -> unit
}

let todo_list (props: todo_list_props) = let open Jsoo_react in
  (* let cxt_value = use_context ctx in *)
  let mutable_ref = use_ref 1 in

  use_effect0 (fun () -> (
    print_endline "Mutate ref!";
    console_log !mutable_ref;
    mutable_ref := 123;
    console_log !mutable_ref;
    Js.Optdef.empty
  ));

  Html.ul [] (List.mapi (fun i -> fun item -> 
    Html.li [
      Attr.Key (string_of_int i);
      Attr.OnClick (fun _ -> props.on_item_click i);
      Attr.style [
        if item._done then Style.TextDecoration "line-through" else Style.TextDecoration "none";
        if item._done then Style.Color "grey" else Style.Color "black"
      ]
    ] [string item.name]
  ) props.todos)

type memo_test_props = {
  value: string;
}

let memo_test = let open Jsoo_react in memo (fun (props: memo_test_props) ->
  print_endline "memoized component test only one render";
  Jsoo_react.fragment ~key:"asd" [
    Html.div [] [string "wrapperd with fragment"];
    Html.div [] [string @@ "memo test value: " ^ props.value];
  ])

let app () = let open Jsoo_react in
  let dom_node = use_ref Js.Opt.empty in
  let (cxt_value, set_ctx_value) = use_state (fun () -> 1) in
  let (text, setText) = use_state (fun () -> "") in
  let (todos, setTodos) = use_state (fun () -> [
    { name = "go to gym"; _done = false };
    { name = "walk at forest"; _done = true };
    { name = "go to js meetup"; _done = false }
  ]) in
  let ctx_value = create_context_value {
    value = cxt_value;
    setValue = fun v -> set_ctx_value (fun _ -> v)
  } in

  let on_item_click idx = setTodos (fun tds -> List.mapi (fun i -> fun item -> (
    if idx <> i then item else { item with _done = not item._done }
  )) tds) in

  (match Js.Opt.to_option !dom_node with
    | Some s -> console_log s
    | None -> console_log @@ !dom_node);

  use_effect0 (fun _ -> (

    Js.Optdef.return @@ fun () -> (
      print_endline "buy";
    )
  ));

  error_bound [
    ctx_provider <@> ctx_value </@> [
      Html.div [] [
        memo_test <@> { value = "const value" } </@> [];
        Html.h1 [Attr.Ref dom_node] [string "My todos"];
        Html.input [
          Attr.Value text;
          Attr.OnKeyDown (fun e -> match Js.Optdef.to_option e##.nativeEvent##.code with
            | Some s -> (match (Jsoo_browser.Event.keyboard_target e##.nativeEvent) with
              | Some t ->
                let code = Js.to_string s
                and value = Js.to_string t##.value in
                if code = "Enter" && String.length value > 0
                then (
                  setTodos (fun tds -> List.append tds [{ name = value; _done = false }]);
                  setText (fun _ -> ""))
              | None -> ())
            | None -> ()
          );
          Attr.OnInput (fun e -> 
            let a = match (Jsoo_browser.Event.keyboard_target e##.nativeEvent) with
              | Some s -> Js.to_string s##.value
              | None -> "" in
            setText (fun _ -> a)
          )
        ];
        todo_list <@> { todos; on_item_click } </@> []
      ]
    ]
  ]

let _start = let open Jsoo_react in
  let el = app <@> () </@> [] in
  Dom.render el (Dom_html.getElementById "root")

let url = "https://jsonplaceholder.typicode.com/todos"

let _main =
  let _r = Lwt.try_bind (
    fun _ -> Js_of_ocaml_lwt.XmlHttpRequest.perform_raw_url url) (fun v ->
    print_endline "ok";
    print_endline @@ string_of_int v.code;
    print_endline @@ v.content;

    (match v.headers "Origin" with
      | Some s -> print_endline ("origin: " ^ s)
      | None -> print_endline "no headers");

    Lwt.return_unit
  ) (fun ex -> print_endline "wrong"; console_log ex; Lwt.return_unit) in
  ()