.SUFFIXES:
FC=gfortran
CFLAGS=-fdefault-real-8 -Wall -O3 #-g

all: main

main: main.o
	$(FC) $(CFLAGS) main.o -o main

%.o : %.f03
	$(FC) $(CFLAGS) -c  $<

.PHONY: clean
clean:
	-rm -f *.o *.mod *.smod main
