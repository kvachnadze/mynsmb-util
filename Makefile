#
# Makefile for nsmb4.4 utilities program under IPM
# (c) Eric Sevault, Vincent Van Kemenade (CERFACS)
#     Jan B. Vos (LMF/EPFL)
#
# This makefile should be Ok for these computers:
#
#       RS6K : IBM RS6000, IBM SP2
#       SUN4 : Sparc workstation
#       SGI  : Silicon Graphics/Origin 2000
#       HPPA : HP-9000 700 series
#       SX8  : NEC SX8
#       IFC8 : Intel IFC compiler
#       LINUX: PC under LINUX

#######################################################################
#  Begining of customized section
#
NSMBDIR   = /scratch/kvachnadze/nsmb6.08.09.231014
include $(NSMBDIR)/Install/nsmb.common
#
#
CLEAN     = yes
#
# for MEMCOM 6.5, use  -I$(MEMCOM)/include
# for MEMCOM 6.6, use  -I$(MEMCOM)/include/$(MCARCH)
# for MEMCOM 6.61, use -I$(MEMCOM)/include/memcom6
# for MEMCOM 7.0, use  -I$(MEMCOM)/include
# 
DEFS      = -DUTIL -DIJK -DMC7 -I$(MEMCOM)/include -DICEM -DCGNS -I$(ICEM_ACN)/icemcfd/output-interfaces/include   -D__TECBIN__
PWD       = $(NSMBDIR)/UTIL
#
# End of customized section
#######################################################################
#
LIBICEMD  = 
LIBICEMT  = 
LIBNSMB   = $(NSMBDIR)/lib/$(IPM_ARCH)/libnsmb${NSMBVER}.a
#
# cgns lib
#
LIBCGNS  = $(CGNS)/lib/libcgns.a -L/scratch/kvachnadze/cgnslib_3.1.3/lib
#
# for MEMCOM 6.61 use $(MEMCOM)/lib/libmemcom6.a
# for MEMCOM 7.0  use $(MEMCOM)/lib/libmemcomf.a $(MEMCOM)/lib/libmemcom.a
#
LIBMEM = $(MEMCOM)/lib/libmemcomf.a $(MEMCOM)/lib/libmemcom.a
#
# ICEMCFD libraries are only used when -DICEM is specified in the
# DEFS of this makefile
#
LIBICEMD  = $(ICEM_ACN)/icemcfd/output-interfaces/lib/domainlib.a
LIBICEMT  = $(ICEM_ACN)/icemcfd/output-interfaces/lib/topolib.a
LIBICEMB  = $(ICEM_ACN)/icemcfd/output-interfaces/lib/bocolib.a
#
# Libraries
#
LIBS = $(LIBNSMB) $(LIBMEM) $(OLIBS)
#
MAINO = 
#
NSMBO = 
#
FLAGS = $(FFLAGS) -I$(MPI_HOME)/include -I$(INCLUDE)
#
#util : 
#	make "PWD=$(PWD)" mpi_dummy
#	make all
#
all :  post_script post_script_2 post_script_vokeloc tauw_x auto_tauw  \
       coor_splitter rmsfrommon unf_splittocsv surfgrid_extract
#
post_script: post_script.o $(NSMBO)
	$(F77) $(LFLAGS) -o $@ $@.o $(NSMBO) mpi_dummy.o $(LIBS)
	chmod og+rx $@
post_script_2: post_script_2.o $(NSMBO)
	$(F77) $(LFLAGS) -o $@ $@.o $(NSMBO) mpi_dummy.o $(LIBS)
	chmod og+rx $@
post_script_vokeloc: post_script_vokeloc.o $(NSMBO)
	$(F77) $(LFLAGS) -o $@ $@.o $(NSMBO) mpi_dummy.o $(LIBS)
	chmod og+rx $@
tauw_x: tauw_x.o $(NSMBO)
	$(F77) $(LFLAGS) -o $@ $@.o $(NSMBO) mpi_dummy.o $(LIBS)
	chmod og+rx $@
auto_tauw:  auto_tauw.o $(NSMBO)
	mpif90 $(LFLAGS) -o $@ $@.o $(NSMBO) mpi_dummy.o $(LIBS)
	chmod og+rx $@
auto_tauw.o: auto_tauw.f90
	mpif90 -c auto_tauw.f90
coor_splitter:  coor_splitter.o $(NSMBO)
	mpif90 $(LFLAGS) -o $@ $@.o $(NSMBO) mpi_dummy.o $(LIBS)
	chmod og+rx $@
coor_splitter.o: coor_splitter.f90
	mpif90 -c coor_splitter.f90
rmsfrommon:  rmsfrommon.o $(NSMBO)
	mpif90 $(LFLAGS) -o $@ $@.o $(NSMBO) mpi_dummy.o $(LIBS)
	chmod og+rx $@
rmsfrommon.o: rmsfrommon.f90
	mpif90 -c rmsfrommon.f90
unf_splittocsv:  unf_splittocsv.o $(NSMBO)
	mpif90 $(LFLAGS) -o $@ $@.o $(NSMBO) mpi_dummy.o $(LIBS)
	chmod og+rx $@
unf_splittocsv.o: unf_splittocsv.f90
	mpif90 -c unf_splittocsv.f90
surfgrid_extract:  surfgrid_extract.o $(NSMBO)
	mpif90 $(LFLAGS) -o $@ $@.o $(NSMBO) mpi_dummy.o $(LIBS)
	chmod og+rx $@
surfgrid_extract.o: surfgrid_extract.f90
	mpif90 -c surfgrid_extract.f90



#
#include $(INCLUDE)/makefile.h
#
#mpi_dummy :
#	-rm -f mpif.h mpi_dummy.f
#	ln -s $(INCLUDE)/mpif.h.dummy ./mpif.h
#	-if  (test `grep -ls 'MPI' $(MPI_HOME)/include/mpif.h`) ; then \
#	rm -f mpif.h ; ln -s $(MPI_HOME)/include/mpif.h .  ;  fi
#	ln -sf $(NSMBDIR)/include/mpi_dummy.f .
#	$(F77) $(FLAGS) -c mpi_dummy.f
#	-ln -sf $(NSMBDIR)/include/ctype.c .
#	$(CC)  $(CFLAGS) -c ctype.c
