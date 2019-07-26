# Orda

**O**rdinary **R**eactJS **D**evelopment **A**pproach (powered by OCaml)

**WARNING** Library is under active development and things may changes very often!

## Features

- only functional components with hooks
- standalone approach to write components only from scratch (because of tedious interoperability with JavaScript objects from third party components)

## TODO

- state managment solution (hooks or redux, that is the question)
- url routing solution
- maybe css in js(ocaml)

## Components using example

```ocaml
  (* components/Simple.ml *)
  module React = Orda.React
  module Html = React.Html
  module Attr = React.Html.Attr

  module Simple: React.ComponentSignature with type t = unit = struct
    type t = unit

    let make = let open React in memo @@ fun () ->
      Html.div
        [Attr.ClassName "class"]
        [string "Simple component"]
  end

  include Simple

  (* main.ml *)
  module App: React.ComponentSignature with type t = unit = struct
    type t = unit

    let make () = let open React in
      (module Components.Simple) <@> () </@> []
  end

  let _main = let open React in
    let el = (module App) <@> () </@> [] in
    Dom.render el (Dom_html.getElementById "root")
```

## Building process

Not actual, comming soon working one

```sh
  make js-deps
  make js-build
  make
```

## Running

Comming soon
