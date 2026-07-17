.PHONY: all bootstrap tests clean

all: tests bootstrap

bootstrap: tests
	dune exec src/bootstrap.exe tests

tests:
	make -C tests

clean:
	make -C tests clean
