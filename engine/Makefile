all:		libengine.so

callback.o:	callback.c callback.h
		gcc -O3 -Wall -Wextra -fpic -c callback.c

tape.o:		tape.c tape.h
		gcc -O3 -Wall -Wextra -fpic -c tape.c

libengine.so:	callback.o tape.o
		gcc -shared -o libengine.so callback.o tape.o

clean:
		rm -f *.o *.so
