.SUFFIXES:
FC=gfortran
CFLAGS=-fdefault-real-8 -g

all: main

main: main.o findroot.o
	$(FC) $(CFLAGS) main.o findroot.o -o main

main.o: main.f03 findroot.mod
	$(FC) $(CFLAGS) -c $<

%.o : %.f03
	$(FC) $(CFLAGS) -c  $<
findroot.mod : findroot.f03
	$(FC) $(CFLAGS) -c findroot.f03

.PHONY: clean
clean:
	-rm -f *.o *.mod *.smod problem?
