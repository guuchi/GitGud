main:
	dune build @install
	dune install GitGud

clean:
	rm -r -f _build/
