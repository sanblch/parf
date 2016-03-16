##### Configuration section

### Choose a C compiler and options
CC = cc
CFLAGS = -Wall -g -pg --static

### Choose a Fortran 90 compiler and options
FORT = gfortran
ifeq (${FORT},gfortran)
	FC = gfortran
	FFLAGS = -fno-range-check -g -pg --static
endif
ifeq (${FORT},ifort)
	FC = /opt/intel_fc_80/bin/ifort
	FFLAGS = -g -pg -CB -traceback --static
endif
### For MPI: the MPI Fortran compilation command
ifeq (${FORT},mpif90)
	FC = mpif90
	FFLAGS = -fno-range-check -g -pg
endif

##### End of configuration section
# 
# No changes should be necessary below this line
# ---------------------------------------------------------------------
MODSOURCES=trees.f90 bitvectors.f90 instancesets.f90 options.f90 \
	utilities.f90 bootstraps.f90 forests.f90 importances.f90 \
	prototypes.f90 graphics.f90
CSOURCES=support.c
COBJECTS=${CSOURCES:.c=.o}
MODOBJECTS=${MODSOURCES:.f90=.o}
ADDOBJECTS=${ADDSOURCES:.f=.o}
PROJECT=parf
DIR=$(notdir ${PWD})

${PROJECT}: main.f90 parallel.o ${MODOBJECTS} ${ADDOBJECTS} ${COBJECTS}
	${FC} ${FFLAGS} -o ${PROJECT} $+

parallel.o: parallel/${PAR}.f90
	${FC} ${FFLAGS} -c -o parallel.o $<

%.o: %.f90
	${FC} ${FFLAGS} -c $<

%.o: %.c
	${CC} ${CFLAGS} -c $<

main.o: Makefile options.o instancesets.o utilities.o forests.o \
	importances.o prototypes.o parallel.o
forests.o: Makefile trees.o instancesets.o bootstraps.o bitvectors.o \
	importances.o prototypes.o
trees.o: Makefile bitvectors.o instancesets.o bootstraps.o utilities.o
instancesets.o: Makefile utilities.o bitvectors.o \
	options.o parallel.o support.o
importances.o: Makefile instancesets.o graphics.o
bitvectors.o: Makefile utilities.o
utilities.o: Makefile support.o
options.o: Makefile support.o utilities.o parallel.o
#compatibility.o: Makefile
parallel.o: Makefile
bootstraps.o: Makefile instancesets.o utilities.o
prototypes.o: Makefile instancesets.o utilities.o options.o
graphics.o: Makefile utilities.o options.o
support.o: Makefile

clean:
	rm -f *.mod *.o ${PROJECT} gmon.out

dist:
	rm -f ${PROJECT}.tgz
	cd .. && \
		tar zcf ${DIR}/${PROJECT}.tgz ${DIR}/Makefile \
		${DIR}/*.f90 ${DIR}/*.c ${DIR}/farg ${DIR}/parallel \
		${DIR}/*.pl ${DIR}/LICENSE

.PHONY: clean dist
