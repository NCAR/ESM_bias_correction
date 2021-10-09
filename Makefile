FC=gfortran

NETCDF_LIB=-L${NETCDF}/lib -lnetcdf -lnetcdff
NETCDF_INC=-I${NETCDF}/include

# SRC=src/*.f90
SRC=src/constants.f90 src/io_routines.f90 src/atmosphere_dataset.f90 src/initialize.f90 src/bias_correction.f90

FFLAGS=-g -fbounds-check -fbacktrace -finit-real=nan -ffree-line-length-none -ffpe-trap=invalid

esm_bias_correction: ${SRC}
	${FC} ${FFLAGS} $^ -o $@ ${NETCDF_INC} ${NETCDF_LIB}
