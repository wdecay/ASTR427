.SUFFIXES:
FC=gfortran

all: problem1 # problem2 problem3

problem1: problem1.o
	$(FC) problem1.o rk4.o -o problem1


problem1.o: problem1.f03 rk4.mod
	$(FC) -c $<

# problem2: problem2.o
# 	$(FC) problem2.o -o problem2

# problem3: problem3.o
#	$(FC) problem3.o -o problem3

%.o : %.f03
	$(FC) -c  $<
rk4.mod : rk4.f03
	$(FC) -c rk4.f03

.PHONY: clean
clean:
	-rm -f *.o *.mod *.smod problem?
