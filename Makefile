executables = xmat_gfort.exe
FC     = gfortran
FFLAGS = -O0 -Wall -Werror=unused-parameter -Werror=unused-variable -Werror=unused-function -Wno-maybe-uninitialized -Wno-surprising -fbounds-check -static -g -fmodule-private
obj    = kind.o mat.o xmat.o

all: $(executables)

# Compile .f90 to .o
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

xmat_gfort.exe: kind.o mat.o xmat.o
	$(FC) -o xmat_gfort.exe kind.o mat.o xmat.o $(FFLAGS)

run: $(executables)
	./xmat_gfort.exe

clean:
	rm -f $(executables) $(obj)

