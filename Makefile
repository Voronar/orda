build:
	dune build @example

js-deps:
	npm i --prefix js

js-build:
	npm run --prefix js build

clean:
	rm -r _build
	rm -r js/dist
