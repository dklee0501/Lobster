CC = g++
CFLAGS =-O2 -Wfatal-errors -Wshadow -Wall -I/usr/local/include -I../ntl-11.3.2/include -I../HElib/src

LDLIBS = -L. -L/usr/local/lib -lntl -lgmp -lm

./main: main.cpp
	$(CC) $(CFLAGS) -o $@ $< $(LDLIBS)

clean:
	rm -f *.o main

