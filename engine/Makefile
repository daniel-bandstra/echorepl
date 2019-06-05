all:		libengine.so

sample.o:	sample.c sample.h
		gcc -O3 -Wall -Wextra -fpic -c sample.c

tape.o:		tape.c tape.h
		gcc -O3 -Wall -Wextra -fpic -c tape.c

moment.o:	moment.c moment.h
		gcc -O3 -Wall -Wextra -fpic -c moment.c

libengine.so:	sample.o tape.o moment.o
		gcc -shared -o libengine.so sample.o tape.o moment.o

clean:
		rm -f *.o *.so
