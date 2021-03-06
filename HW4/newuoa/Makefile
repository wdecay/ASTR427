FC = gfortran
CFLAGS =-std=legacy #-g
AR=ar
ARFLAGS=cr
OBJS=bigden.o biglag.o newuoa.o newuob.o trsapp.o update.o

all : libnewuoa.a

libnewuoa.a: $(OBJS)
	$(AR) $(ARFLAGS) $@ $(OBJS)

%.o : %.f
	$(FC) $(CFLAGS) -c  $<

clean:
	rm -f *.o *.a
