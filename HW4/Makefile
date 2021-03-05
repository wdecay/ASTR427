.SUFFIXES:
FC=gfortran
CFLAGS=-fdefault-real-8 -Wall -O3 -g

all: main

main: main.o optimization.o dataio.o libnewuoa
	$(FC) $(CFLAGS) main.o optimization.o dataio.o -L./newuoa -lnewuoa -o main

main.o: main.f03 optimization.mod dataio.mod
	$(FC) $(CFLAGS) -c $<

%.o : %.f03
	$(FC) $(CFLAGS) -c  $<

optimization.mod : optimization.f03
	$(FC) $(CFLAGS) -c optimization.f03

dataio.mod : dataio.f03
	$(FC) $(CFLAGS) -c dataio.f03

libnewuoa: force
	cd newuoa && make

.PHONY: clean
clean:
	-rm -f *.o *.mod *.smod main
	cd newuoa && make clean

force:
	true
