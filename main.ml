open! Js_of_ocaml
module Html = Reactjs_ml.Html

let consolelog (s): unit =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.log") [|Js.Unsafe.inject s|]

let jsonsparse (s : Js.Unsafe.any): Js.Unsafe.any =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "JSON.parse") [|s|]


let useIntervalHook initial =
  let (state, setState) = Reactjs_ml.use_state (fun () -> initial) in

  Reactjs_ml.use_effect0 (fun () -> 
    let _ = Dom_html.window##setInterval (Js.Unsafe.callback (fun () -> (
      setState (fun s -> s + 1)
    ))) 1000.0 in

    Js.Optdef.option None
  );
  (state)

type p = <
  key: string Js.readonly_prop;
  initial: int Js.readonly_prop;
  title: string Js.readonly_prop;
> Js.t

let counter (props: p) =
  let (state) = useIntervalHook props##.initial in
  Reactjs_ml.string (props##.title ^ (string_of_int state))

let app () = let open Reactjs_ml in
  let (state, setState) = Reactjs_ml.use_state (fun () -> 0) in
  let handle_click = fun _ -> setState (fun s -> s + 1) in

  Html.div [
    Html.Attr.OnClick handle_click;
    Style (Reactjs_ml.Html.Attr.inline_style (object%js
      val display = "flex"
      val flexDirection = "column"
      val background = "skyblue"
    end))
  ] [
    Html.div [] [Reactjs_ml.string @@ "Root click counter: " ^ string_of_int state];
    Html.div [] [
      (counter </> object%js
        val initial = 10
        val title = "Timer interval counter: "
        val key = "asd"
      end) []
    ];
  ]

let _start = let open Reactjs_ml in
  let el = (app </> ()) [] in
  Reactjs_ml_dom.render el (Dom_html.getElementById "root")