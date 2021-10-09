FC=gfortran

NETCDF_LIB=-L${NETCDF}/lib -l netcdff
NETCDF_INC=-I${NETCDF}/include

esm_bias_correction: src/*.f90
    ${FC} $^ -o $@ $NETCDF_INC $NETCDF_LIB
