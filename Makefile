build:
	dune build @default

js-deps:
	./js-deps.sh

js-build:
	./js-build.sh

clean:
	rm -r _build
	rm -r js/dist
