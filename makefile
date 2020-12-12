CC=ocamlc
FILE_OUT=game.out
FLAGS=-thread unix.cma threads.cma graphics.cma -o $(FILE_OUT)
ML_FILE=game.ml

all: compile run clean

compile:
	$(CC) $(FLAGS) $(ML_FILE)
clean:
	rm -rf *.cmo *.cma *.cmi *.out
run:
	./$(FILE_OUT)
