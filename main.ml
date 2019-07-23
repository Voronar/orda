open Js_of_ocaml
module Html = Reactjs.Html
module Attr = Reactjs.Html.Attr
module Style = Reactjs.Html.InlineStyle

type todo = {
  name: string;
  _done: bool
}
[@@deriving yojson]


type ctx_type = {
  value: int;
  setValue: int -> unit;
}
let ctx = Reactjs.create_context { value = 0; setValue = fun _ -> () }
let ctx_provider = Reactjs.get_provider ctx

type todo_list_props = {
  todos: todo list;
  on_item_click: int -> unit
}

let todo_list (props: todo_list_props) = let open Reactjs in
  (* let cxt_value = use_context ctx in *)
  let mutable_ref = use_ref 1 in

  use_effect0 (fun () -> (
    print_endline "Mutate ref!";
    Browser.Global.console_log !mutable_ref;
    mutable_ref := 123;
    Browser.Global.console_log !mutable_ref;
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

let memo_test = let open Reactjs in memo (fun (props: memo_test_props) ->
  print_endline "memoized component test only one render";
  Html.h1 [] [string @@ "memo test value: " ^ props.value])

let app () = let open Reactjs in
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

  use_effect0 (fun _ -> (
    Js.Optdef.return @@ fun () -> (
      print_endline "buy";
    )
  ));

  ctx_provider <@> ctx_value </@> [
    Html.div [] [
      memo_test <@> { value = "const value" } </@> [];
      Html.h1 [] [string "My todos"];
      Html.input [
        Attr.Value text;
        Attr.OnKeyDown (fun e -> match Js.Optdef.to_option e##.nativeEvent##.code with
          | Some s -> (match (Browser.Event.keyboard_target e##.nativeEvent) with
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
          let a = match (Browser.Event.keyboard_target e##.nativeEvent) with
            | Some s -> Js.to_string s##.value
            | None -> "" in
          setText (fun _ -> a)
        )
      ];
      todo_list <@> { todos; on_item_click } </@> []
    ]
  ]

let _start = let open Reactjs in
  let el = app <@> () </@> [] in
  Dom.render el (Dom_html.getElementById "root")
