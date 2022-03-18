#================================================================
#                 PREPARE SFLUX MAKEFILE
#================================================================

# Project and case definition
RUNFILE 	      = run_prepare_sflux
ARCH              = intel-fortran
NETCDFLIBS        = hpc-icmat
#================================================================

#================================================================
# ***************************************************************
#================================================================

# NetCDF libraries
ifeq ($(NETCDFLIBS),none)
LIB_DIR =
INC_DIR =
ORM_FLAGS += -Dno_netcdf

else ifeq ($(NETCDFLIBS),automatic)
LIB_DIR = $(shell nc-config --flibs)
INC_DIR = -I$(shell nc-config --includedir)

else ifeq ($(NETCDFLIBS),automatic-44)
LIB_DIR = $(shell nf-config --flibs)
INC_DIR = $(shell nf-config --cflags)

else ifeq ($(NETCDFLIBS),hpc-icmat)
LIB_DIR = -L/LUSTRE/apps/netcdf/intel/fortran/4.4.5/lib -lnetcdff
#LIB_DIR = -L/LUSTRE/apps/netcdf/gfortran/8.1.0/4.5.2/lib -lnetcdff -L/LUSTRE/apps/netcdf/gcc/c/4.7.0/lib -lnetcdf -lnetcdf -ldl
INC_DIR = -I/LUSTRE/apps/netcdf/intel/fortran/4.4.5/include 
#INC_DIR = -I/LUSTRE/apps/netcdf/gfortran/8.1.0/4.5.2/include

else
NCDF_ROOT = /BGFS/DISASTER/garcgui/build/netcdf-c-4.6.1

LIB_DIR = -L$(NCDF_ROOT)/lib -lnetcdf -lnetcdff
INC_DIR	= -I$(NCDF_ROOT)/include

endif

# Fortran compiler and flags
ifeq ($(ARCH),intel-fortran)
FC = /LUSTRE/apps/Intel/2019/compilers_and_libraries_2019.1.144/linux/bin/intel64/ifort
FF = -g -Ofast -traceback -warn all

else
FC = gfortran
FF = -g -O0 -fbacktrace -Wall -Wno-maybe-uninitialized -Wno-unused-dummy-argument

endif

all: runfile

#================================================================

# Object definitions
OBJDIR := _build

objects := $(addprefix $(OBJDIR)/,mod_precision.o mod_const.o mod_datetime.o \
	mod_nc_tools.o mod_strings.o mod_nc_sflux.o PREPARE_SFLUX.o)

$(OBJDIR)/%.o : %.f90
		$(FC) $(FF) -c $(ORM_FLAGS) $(PROJECT_FLAG) $(CASE_FLAG) $(INC_DIR) $(LIB_DIR) $< -o $@

$(objects) : | $(OBJDIR)

$(OBJDIR):
			mkdir -p $(OBJDIR)

#================================================================

runfile : $(objects)
	$(FC) $(FF) $(ORM_FLAGS) -o $(RUNFILE) $(objects) $(INC_DIR) $(LIB_DIR)

test :

#	sed '42s~.*~NCDIR="$(LIB_DIR) $(INC_DIR)" ~' src/_funit/runtest.sh > runtest.sh
#	chmod +x runtest.sh

.PHONY : clean

clean:
	-rm -f *.mod *.o $(RUNFILE)
	-rm -rf *.o *.mod *.out *.dSYM *.csv fort.* *.x *.in
	-rm -rf _build
#	test -s runtest.sh && rm runtest.sh || true
#	test -s $(RUNFILE) && rm $(RUNFILE) || true

.PHONY : help

help :
	@echo
	@echo "make       : Generate CONTOUR_EVOLUTION_2D runfile '$(RUNFILE).x'."
	@echo "make test  : Generate test-suite runscripts 'runtest.sh'. (not implemented yet)"
	@echo "make clean : Remove auto-generated files."
	@echo
