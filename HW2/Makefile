.SUFFIXES:
FC=gfortran
CFLAGS=-fdefault-real-8 #-g

all: problem2 problem3

problem2: problem2.o ode.o
	$(FC) $(CFLAGS) problem2.o ode.o -o problem2

problem3: problem3.o ode.o
	$(FC) $(CFLAGS) problem3.o ode.o -o problem3

problem2.o: problem2.f03 ode.mod
	$(FC) $(CFLAGS) -c $<

problem3.o: problem3.f03 ode.mod
	$(FC) $(CFLAGS) -c $<

%.o : %.f03
	$(FC) $(CFLAGS) -c  $<
ode.mod : ode.f03
	$(FC) $(CFLAGS) -c ode.f03

.PHONY: clean
clean:
	-rm -f *.o *.mod *.smod problem?
