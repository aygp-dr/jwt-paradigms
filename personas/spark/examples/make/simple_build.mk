main.o: main.c defs.h
	gcc -c main.c

utils.o: utils.c utils.h defs.h
	gcc -c utils.c

main: main.o utils.o
	gcc -o main main.o utils.o
