c-----------------------------------------------------------------------
c
c     mpif.h dummy used for serial execution of NSMB
c
c     Creation Date: 01 - Jul - 1998  
c     -------------
c
c     Author: J.B. Vos / IMHEF
c     ------
c
c     Modification history:
c     --------------------
c
c     date           programmer      description
c
c
c-----------------------------------------------------------------------
      integer MPI_STATUS_SIZE
      parameter (MPI_STATUS_SIZE  = 6)
      integer MPI_ERRORS_ARE_FATAL
      integer MPI_ANY_SOURCE,MPI_SOURCE
      integer MPI_ANY_TAG,MPI_TAG
      integer MPI_SUM,MPI_MAX,MPI_MAXLOC,MPI_2DOUBLE_PRECISION,
     *        MPI_2REAL,MPI_MIN,MPI_LOR
      integer MPI_BSEND_OVERHEAD
      integer MPI_COMM_WORLD,MPI_BYTE,MPI_CHARACTER,MPI_LOGICAL,
     *        MPI_INTEGER4,MPI_REAL4,MPI_REAL8,MPI_COMPLEX,
     *        MPI_DOUBLE_COMPLEX

