FC=gfortran
RM=rm -f
ECHO=echo

NETCDF_LIB=-L${NETCDF}/lib -lnetcdf -lnetcdff
NETCDF_INC=-I${NETCDF}/include

# SRC=src/*.f90
SRC=src/constants.f90 \
	src/data_structures.f90 \
	src/io_routines.f90 \
	src/string.f90 \
	src/time_delta_obj.f90 \
	src/time_h.f90 \
	src/time_obj.f90 \
	src/time_io.f90 \
	src/output_dataset.f90 \
	src/geographic_interpolation.f90 \
	src/vinterp.f90 \
	src/vertical_interpolation.f90 \
	src/time_period.f90 \
	src/atmosphere_dataset.f90 \
	src/initialize.f90 \
	src/bias_correction.f90

FFLAGS=-g -fbounds-check -fbacktrace -finit-real=nan -ffree-line-length-none -ffpe-trap=invalid -J build/ -I build/

esm_bias_correction: ${SRC}
	${FC} ${FFLAGS} $^ -o $@ ${NETCDF_INC} ${NETCDF_LIB}

clean:
	${RM} build/*.o build/*.mod build/*.smod esm_bias_correction
	${RM} -r esm_bias_correction.dSYM
