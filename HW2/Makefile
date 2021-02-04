.SUFFIXES:
FC=gfortran
CFLAGS=-fdefault-real-8

all: problem1 # problem2 problem3

problem1: problem1.o
	$(FC) problem1.o ode.o -o problem1


problem1.o: problem1.f03 ode.mod
	$(FC) -c $<

# problem2: problem2.o
# 	$(FC) problem2.o -o problem2

# problem3: problem3.o
#	$(FC) problem3.o -o problem3

%.o : %.f03
	$(FC) -c  $<
ode.mod : ode.f03
	$(FC) -c ode.f03

.PHONY: clean
clean:
	-rm -f *.o *.mod *.smod problem?
