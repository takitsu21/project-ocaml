CC=ocamlc
FILE_OUT=game.out
FLAGS=-thread unix.cma threads.cma graphics.cma -o $(FILE_OUT)
ML_FILE=labyrinthe.ml

all: compile run clean-all

compile:
	$(CC) $(FLAGS) $(ML_FILE)
clean-all:
	rm -rf *.cmo *.cma *.cmi *.out
clean:
	rm -rf *.cmo *.cma *.cmi
run:
	./$(FILE_OUT)
