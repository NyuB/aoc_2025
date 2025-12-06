default: fmt build test

test:
	dune test
test-promote:
	dune test --auto-promote
build:
	dune build
fmt:
	dune fmt

day-%:
	dune exec ./daily_template.exe $*