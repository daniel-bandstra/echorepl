all:		libpedal.so

pedal.o:	pedal.c pedal.h
		gcc -O3 -Wall -Wextra -fpic -c pedal.c

libpedal.so:	pedal.o
		gcc -shared -o libpedal.so pedal.o

clean:
		rm -f *.o *.so
