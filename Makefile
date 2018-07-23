OCB_FLAGS = -lib str
OCB = ocamlbuild $(OCB_FLAGS)

all: clean build

clean:
	$(OCB) -clean

run:
	./rpg.byte

build:
	$(OCB) rpg.byte


