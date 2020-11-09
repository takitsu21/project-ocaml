CC=ocamlc
FILE_OUT=game.out
FLAGS=-thread unix.cma threads.cma graphics.cma -o $(FILE_OUT)
FILE_IN=labyrinthe.ml

all: compile run clean

compile:
	$(CC) $(FLAGS) labyrinthe.ml
clean:
	rm -rf *.cmo *.cma *.out *.cmi
run:
	./$(FILE_OUT)
