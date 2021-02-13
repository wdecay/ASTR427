.SUFFIXES:
FC=gfortran
CFLAGS=-fdefault-real-8 #-g

all: problem1 problem2

problem1: problem1.o findroot.o
	$(FC) $(CFLAGS) problem1.o findroot.o -o problem1

problem2: problem2.o findroot.o
	$(FC) $(CFLAGS) problem2.o findroot.o -o problem2

problem1.o: problem1.f03 findroot.mod
	$(FC) $(CFLAGS) -c $<

problem2.o: problem2.f03 findroot.mod
	$(FC) $(CFLAGS) -c $<

%.o : %.f03
	$(FC) $(CFLAGS) -c  $<
findroot.mod : findroot.f03
	$(FC) $(CFLAGS) -c findroot.f03

.PHONY: clean
clean:
	-rm -f *.o *.mod *.smod problem?
