.PHONY: clean check vaast

vaast:
	dune build vaast.install

check:
	make -C check

clean:
	dune clean
	make -C check clean
