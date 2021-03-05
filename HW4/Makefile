.SUFFIXES:
FC=gfortran
CFLAGS=-fdefault-real-8 -Wall -O3 -g

all: main

main: main.o optimization.o libnewuoa
	$(FC) $(CFLAGS) main.o optimization.o -L./newuoa -lnewuoa -o main

main.o: main.f03 optimization.mod
	$(FC) $(CFLAGS) -c $<

%.o : %.f03
	$(FC) $(CFLAGS) -c  $<

optimization.mod : optimization.f03
	$(FC) $(CFLAGS) -c optimization.f03

libnewuoa: force
	cd newuoa && make

.PHONY: clean
clean:
	-rm -f *.o *.mod *.smod main
	cd newuoa && make clean

force:
	true
