FC = gfortran
DFLAGS = -w -g -p -ggdb -ffpe-trap=invalid,zero,overflow,underflow -fbacktrace -fcheck=all

test: fortfilt.o
	$(FC) $(DFLAGS) -o test $^ test.f90

fortfilt.o: fortfilt.f90
	$(FC) $(DFLAGS) -c -o $@ $<

clean:
	rm -rf fortfilt.o fortfilt.mod gmon.out

