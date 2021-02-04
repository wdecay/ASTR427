.SUFFIXES:
FC=gfortran
CFLAGS=-fdefault-real-8

all: problem2

problem2: problem2.o
	$(FC) $(CFLAGS) problem2.o ode.o -o problem2


problem2.o: problem2.f03 ode.mod
	$(FC) $(CFLAGS) -c $<

%.o : %.f03
	$(FC) $(CFLAGS) -c  $<
ode.mod : ode.f03
	$(FC) $(CFLAGS) -c ode.f03

.PHONY: clean
clean:
	-rm -f *.o *.mod *.smod problem?
