# ----------------------------------------------------------------
# Make file to compile esm_bias_correction code
#
# User changes might include the fortran compiler (to ifort) and with that the COMPILER_FFLAGS
#
# Right now, needs the netcdf prefix to be specified as the NETCDF variable
# User can use make MODE=debug to turn on debugging flags
#
# If specific flags need to be passed for netcdf libraries, set NETCDF_LIB
#
# ----------------------------------------------------------------

FC=gfortran

# default to optimized compile
COMPILER_FFLAGS= -Ofast -fopenmp -lgomp
ifeq ($(MODE),serial)
	COMPILER_FFLAGS=-Ofast
endif


ifeq ($(MODE),debug)
	COMPILER_FFLAGS=-g -Wall -fbounds-check -fbacktrace -finit-real=nan -ffree-line-length-none -ffpe-trap=invalid -fopenmp -lgomp
endif

ifeq ($(MODE),debugserial)
	COMPILER_FFLAGS=-g -Wall -fbounds-check -fbacktrace -finit-real=nan -ffree-line-length-none -ffpe-trap=invalid
endif

ifeq ($(MODE),profile)
	COMPILER_FFLAGS=-g -pg -ffree-line-length-none -O3 -fopenmp -lgomp
endif


# -f forces rm to remove non-existant files too (so it doesn't print lots of warning messages if there aren't any)
RM=rm -f

ifndef (NETCDF_LIB)
	NETCDF_LIB=-L$(NETCDF)/lib -lnetcdf -lnetcdff
endif
ifndef (NETCDF_INC)
	NETCDF_INC=-I$(NETCDF)/include
endif

# specify the location of source code files
SRCDIR   = src
# specify the location of output files
OBJDIR   = build

# find all *.f90 files to compile
SOURCES  := $(wildcard $(SRCDIR)/*.f90)
# create a list of output objects by swaping src for build and .f90 for .o
OBJECTS  := $(SOURCES:$(SRCDIR)/%.f90=$(OBJDIR)/%.o)

FFLAGS=$(COMPILER_FFLAGS) -J $(OBJDIR) -I $(OBJDIR)

esm_bias_correction: ${OBJECTS}
	$(FC) $(FFLAGS) $^ -o $@ $(NETCDF_LIB)

clean:
	$(RM) build/*.o build/*.mod build/*.smod esm_bias_correction
	$(RM) -r esm_bias_correction.dSYM

# general rule to compile a src/*.f90 file into a build/*.o file
$(OBJDIR)/%.o: $(SRCDIR)/%.f90
	$(FC) $(FFLAGS) -c $< -o $@ $(NETCDF_INC)

# Specify individual file dependencies
build/atmosphere_dataset.o:	src/atmosphere_dataset.f90 build/constants.o build/io_routines.o \
	build/output_dataset.o build/time_period.o build/quantile_map.o build/vertical_interpolation.o \
	build/geographic_interpolation.o

build/bias_correction.o: src/bias_correction.f90 build/atmosphere_dataset.o build/output_dataset.o build/initialize.o

build/geo_reader.o: src/geo_reader.f90 build/data_structures.o build/constants.o

build/geographic_interpolation.o: src/geographic_interpolation.f90 build/data_structures.o build/geo_reader.o

build/initialize.o: src/initialize.f90 build/atmosphere_dataset.o build/string.o build/io_routines.o build/output_dataset.o build/constants.o

build/output_dataset.o: src/output_dataset.f90 build/io_routines.o build/constants.o

build/quantile_map.o: src/quantile_map.f90 build/data_structures.o build/quantile_map_utility.o

build/quantile_map_utility.o: src/quantile_map_utility.f90 build/sorting.o build/data_structures.o

build/string.o: src/string.f90 build/constants.o

build/time_delta_obj.o: src/time_delta_obj.f90 build/constants.o

build/time_h.o: src/time_h.f90 build/time_delta_obj.o build/constants.o

build/time_obj.o: src/time_obj.f90 build/time_h.o

build/time_io.o: src/time_io.f90 build/constants.o build/time_h.o build/time_delta_obj.o \
	build/string.o build/io_routines.o

build/time_period.o: src/time_period.f90 build/constants.o build/geographic_interpolation.o \
	build/io_routines.o build/time_io.o build/time_h.o

build/vertical_interpolation.o: src/vertical_interpolation.f90 build/vinterp.o build/data_structures.o

build/vinterp.o: src/vinterp.f90 build/data_structures.o
