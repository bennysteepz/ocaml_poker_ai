test:
	ocamlbuild -use-ocamlfind gamestate_test.byte && ./gamestate_test.byte

play:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte

check:
	bash checkenv.sh && bash checktypes.sh

zip:
	zip texas.zip *.ml* *.mli Makefile *.sh _tags

zipcheck:
	bash checkzip.sh

clean:
	ocamlbuild -clean
	rm -f checktypes.ml
	rm -f texas.zip
