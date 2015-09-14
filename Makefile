FC = gfortran
DFLAGS = -w -g -p -ggdb -ffpe-trap=invalid,zero,overflow,underflow -fbacktrace -fcheck=all

test: fortfilt.o test.f90
	$(FC) $(DFLAGS) -o test $^

fortfilt.o: fortfilt.f90
	$(FC) $(DFLAGS) -c -o $@ $<

clean:
	rm -rf fortfilt.o fortfilt.mod gmon.out

