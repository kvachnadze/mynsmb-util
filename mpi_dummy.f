      subroutine MPI_INIT(ipm_info)
!
      include 'mpif.h'
      include 'com.h'
!
      ipm_info = 0
      ipm_node = 1
      ipm_rank = 0
!
!     MPI_COMM_WORLD = 0
!
!     MPI_BYTE      = 1
!     MPI_CHARACTER = 2
!     MPI_LOGICAL   = 3 
!     MPI_INTEGER4  = 4
!     MPI_REAL4     = 5
!     MPI_REAL8     = 6
!     MPI_COMPLEX   = 7
!     MPI_DOUBLE_COMPLEX = 8
!
      return
      end
      subroutine MPI_COMM_RANK( ipm_comm, ipm_rank, ipm_info )
      ipm_info = 0
      ipm_rank = 0
      return
      end
      subroutine MPI_COMM_SIZE( ipm_comm, ipm_node, ipm_info )
      ipm_info = 0
      ipm_node = 1
      return
      end
      subroutine MPI_TYPE_SIZE(ipm_in,ipm_out,ipm_info)
      ipm_info = 0
      return
      end
      subroutine MPI_ABORT(ipm_comm, ipm_in, ipm_info )
      ipm_info = 0
      return
      end
      subroutine MPI_BCAST(ipm_comm, ipm_in1, ipm_in2, ipm_in3,
     *                     ipm_in4,  ipm_info )
      ipm_info = 0
      return
      end
      subroutine MPI_BCAST_BYTE(ipm_comm, ipm_in1, ipm_in2, ipm_in3,
     *                          ipm_in4,  ipm_info )
      ipm_info = 0
      return
      end
      subroutine MPI_BARRIER(ipm_comm,ipm_info )
      ipm_info = 0
      return
      end
      subroutine MPI_SEND(ipm_in1,ipm_in2,ipm_in3,ipm_in4,ipm_in5,
     *                     imp_in6,ipm_info)
      ipm_info = 0
      return
      end
      subroutine MPI_BSEND(ipm_in1,ipm_in2,ipm_in3,ipm_in4,ipm_in5,
     *                     imp_in6,ipm_info)
      ipm_info = 0
      return
      end
      subroutine MPI_ISEND(ipm_in1,ipm_in2,ipm_in3,ipm_in4,ipm_in5,
     *                     imp_in6,ipm_in7,ipm_info)
      ipm_info = 0
      return
      end
      subroutine MPI_RECV(ipm_in1,ipm_in2,ipm_in3,ipm_in4,ipm_in5,
     *                    imp_in6,imp_in7,ipm_info)
      ipm_info = 0
      return
      end
      subroutine MPI_ALLREDUCE(ipm_in1,ipm_in2,ipm_in3,ipm_in4,ipm_in5,
     *                         imp_in7,ipm_info)
      ipm_info = -999
      return
      end
      subroutine MPI_REDUCE(ipm_in1,ipm_in2,ipm_in3,ipm_in4,ipm_in5,
     *                      ipm_in6,imp_in7,ipm_info)
      ipm_info = -999
      return
      end
      subroutine MPI_IPROBE(ipm_in1,ipm_in2,ipm_in3,ipm_in4,ipm_in5,
     *                      ipm_info)
      ipm_info = 0
      return
      end
      subroutine MPI_GET_COUNT(ipm_in1,ipm_in2,ipm_in3,ipm_info)
      ipm_info = 0
      return
      end
      subroutine MPI_FINALIZE(ipm_info)
      ipm_info = 0
      return
      end
      subroutine MPI_WAIT
      return
      end
      subroutine MPI_PACK_SIZE(ipm_in1,ipm_in2,ipm_in3,ipm_in4,ipm_info)
      ipm_info = 0
      return
      end
      subroutine MPI_BUFFER_ATTACH(ipm_in1,ipm_in2,ipm_info)
      ipm_info = 0
      return
      end
      subroutine MPI_BUFFER_DETACH(ipm_in1,ipm_in2,ipm_info)
      ipm_info = 0
      return
      end
      subroutine MPI_WTIME()
      end
      subroutine MPI_WTICK()
      end
      subroutine PMPI_WTIME()
      end
      subroutine PMPI_WTICK()
      end
      subroutine MPI_NEC_BLOCK_TIME()
      end
      subroutine MPI_NULL_COPY_FN()
      end
      subroutine MPI_NULL_DELETE_FN()
      end
      subroutine MPI_DUP_FN()
      end
      subroutine MPI_COMM_NULL_COPY_FN()
      end
      subroutine MPI_COMM_NULL_DELETE_FN()
      end
      subroutine MPI_COMM_DUP_FN()
      end
      subroutine MPI_TYPE_NULL_COPY_FN()
      end
      subroutine MPI_TYPE_NULL_DELETE_FN()
      end
      subroutine MPI_TYPE_DUP_FN()
      end
      subroutine MPI_WIN_NULL_COPY_FN()
      end
      subroutine MPI_WIN_NULL_DELETE_FN()
      end
      subroutine MPI_WIN_DUP_FN()
      end
