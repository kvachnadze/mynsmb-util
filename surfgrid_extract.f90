      program surfgrid_extract
      implicit none
        character *60 dbname2,filename,filename2
        character *30 cbuf
        integer umemcom2,istat2
        integer n1,n2,n3,nblock
        integer myunit,igrid2,myunit2
        integer np_coor,np,i,nb
        integer blockset(4)
        data blockset /9,12,10,11/
        real(kind=8), dimension(:,:,:,:), allocatable :: coor,coorc
        integer last(2),nptot(4)
!
        integer, parameter :: jfix1=10,jfix2=41
!
!
        dbname2 = 'omesh_coarse.db'
        myunit = 26
        myunit2 = 27
        filename = 'surfgrid_1.dat'
        filename2 = 'splitparams.dat'
        umemcom2 = 1 
        igrid2 = 0
        nptot = 0
        last = 0
!
        call initmc(1,6,6)
        call opdidb(umemcom2,dbname2,1,' ',istat2)
        call getdes(umemcom2,'ADIR',0,istat2)
        call getbwi('NBR',nblock,istat2)
!
        open(unit=myunit,file=filename)
        open(unit=myunit2,file=filename2)
!
        do i=1,4
          nb = blockset(i)
          call makelabl('MDES',nb,igrid2,0,cbuf,istat2)
          call gettab(umemcom2,cbuf,0,istat2)
          call getbwi('N1',n1,istat2)
          call getbwi('N2',n2,istat2)
          call getbwi('N3',n3,istat2)
!
          np_coor = (n1+1)*(n2+1)*(n3+1)
          np = (n1+2)*(n2+2)*(n3+2)
          nptot(i) = n1*n2
!
          allocate(coor(3,n1+1,n2+1,n3+1))
          allocate(coorc(3,0:n1+1,0:n2+1,0:n3+1))
!
          call m2c_iomemcom(coor,'COOR',3*np_coor,1,  &
                           nb,igrid2,0,               &
                           umemcom2,6,1,istat2)
!     calculate cell center coordinates
          call center3d(coor,coorc,n1,n2,n3)
!     write surface file
          call writesurface(myunit,nb,n1,n2,n3,blockset,jfix1,jfix2,coorc)
          deallocate(coor,coorc)
        enddo
!
        do i=1,4
          nb = blockset(i)
          call writefacecon(nb,blockset,nptot(i),myunit,last)
          write(myunit2,*) nptot(i)
        enddo
!
        close(myunit)
        close(myunit2)
!
      call endadb
!
      end program

      subroutine writesurface(myunit,nb,n1,n2,n3,blockset,jfix1,jfix2,coorc)
      implicit none
!
!
!----------------------------------------------------------------------
! --- ARGUMENTS
!----------------------------------------------------------------------
!
      integer, intent(in) :: myunit,nb,jfix1,jfix2,blockset(4)
      integer, intent(in) :: n1,n2,n3
      real(kind=8), intent(in) :: coorc(3,0:n1+1,0:n2+1,0:n3+1)
!
!----------------------------------------------------------------------
! --- LOCAL VARIABLES
!----------------------------------------------------------------------
!
      integer      :: i,j
      real(kind=8), parameter :: one=1.0, zero = 0.
      character(len=30) :: line1,line2,line3
      integer :: jfix
      real(kind=8) :: myout(3)
!
          line1 = '[Data]'
          line2 = 'X [ m ], Y [ m ], Z [ m ]'
          line3 = '[Faces]'

          if (nb.eq.blockset(1)) then
            write(myunit,'(A)') line1
            write(myunit,'(A)') line2
            jfix=jfix1
            do i=1,n1
              do j=1,n2                 
                myout(1)=coorc(1,i,j,jfix)
                myout(2)=coorc(2,i,j,jfix)
                myout(3)=coorc(3,i,j,jfix)
               write(myunit,102) myout(1),',',myout(2),',',myout(3)
              enddo
            enddo
          endif
!
          if (nb.eq.blockset(2)) then
            jfix=jfix2   
            do j=1,n2
              do i=1,n1
                myout(1)=coorc(1,i,j,jfix)
                myout(2)=coorc(2,i,j,jfix)
                myout(3)=coorc(3,i,j,jfix)
               write(myunit,102) myout(1),',',myout(2),',',myout(3)
             enddo
            enddo
          endif
!
          if (nb.eq.blockset(3)) then
            jfix=jfix2  
            do i=n1,1,-1 
              do j=1,n2 
                myout(1)=coorc(1,i,j,jfix)
                myout(2)=coorc(2,i,j,jfix)
                myout(3)=coorc(3,i,j,jfix)
               write(myunit,102) myout(1),',',myout(2),',',myout(3)
              enddo
            enddo
          endif
!
          if (nb.eq.blockset(4)) then
            jfix=jfix1   
            do j=n2,1,-1
              do i=1,n1
                myout(1)=coorc(1,i,j,jfix)
                myout(2)=coorc(2,i,j,jfix)
                myout(3)=coorc(3,i,j,jfix)
               write(myunit,102) myout(1),',',myout(2),',',myout(3)
              enddo
            enddo
          endif
!
 102  format(e16.8,A,e16.8,A,e16.8)
!
!
      end
!
      subroutine writefacecon(nb,blockset,nptot,myunit,last)
!
        implicit none
        integer :: n,nf4,ost4
        integer,intent(in) :: nptot,myunit,nb,blockset(4)
        integer,intent(inout) :: last(2)
        integer, dimension(:,:), allocatable :: a
!
!
        nf4 = int(nptot/4)
        ost4 = mod(nptot, 4)
!
        allocate(a(nf4+1,4))
!
        if (nb.eq.blockset(1)) then       
          write(myunit,*)      
          write(myunit,'(A)') '[Faces]'   

!          a(1,3) = 2
!          a(1,4) = 3
!          last(1) = a(1,3)
!          last(2) = a(1,4)        
          do n=1,nf4
            a(n,1) = 1 + (n-1)*2
            a(n,2) = (n-1)*2
            a(n,3) = 2 + (n-1)*2
            a(n,4) = 3 + (n-1)*2
          enddo
          a(1,1) = 0 
          a(1,2) = 1 
        else
          do n=1,nf4
            a(n,1) = last(2) + (n-1)*2
            a(n,2) = last(1) + (n-1)*2
            a(n,3) = (last(2)+1) + (n-1)*2
            a(n,4) = (last(2)+2) + (n-1)*2
          enddo
        endif
!
!      
        if (ost4.eq.2) then
          if (nb.eq.blockset(4)) then
            a(nf4,3) = 1
            a(nf4,4) = 0
          else
            a(nf4,3) = a(nf4,1) + 1
            a(nf4,4) = a(nf4,1) + 2
          endif            
            last(1) = a(nf4,3)
            last(2) = a(nf4,4)
          do n=1,nf4
            write(myunit,101) a(n,1),',',a(n,2),',',a(n,3),',',a(n,4)
          enddo
        else            
          a(nf4+1,1) = a(nf4,4)
          a(nf4+1,2) = a(nf4,3)
          if (nb.eq.blockset(4)) then
            a(nf4+1,3) = 1
            a(nf4+1,4) = 0
          else
            a(nf4+1,3) = a(nf4,4) + 1
            a(nf4+1,4) = a(nf4,4) + 2
          endif
          last(1) = a(nf4+1,3)
          last(2) = a(nf4+1,4)
          do n=1,nf4+1
            write(myunit,101) a(n,1),',',a(n,2),',',a(n,3),',',a(n,4)
          enddo
        endif
!
 101  format(i3,A,i3,A,i3,A,i3)
        deallocate(a)
! 
      end
!
      subroutine m2c_iomemcom(a,name,np,ndim,nb,igrid,icycle, &
                              umemcom,uerror,iact,istat)
!
!     This routine is used to do I/0 on the memcom data base
!
!     Creation date: 18 - apr - 1996
!
!     Author:        J.B. Vos/CFS Engineering
!
!     Modification history:
!
!     date           programmer      description
!
!     Input:
!
!     a      = for iact = 2: array to write to MEMCOM
!     iact   = 1: read from MEMCOM
!              2: write to MEMCOM
!     icycle = grid number
!     igrid  = grid number
!     nb     = block number
!     np     = number of points 
!     ndim   = number of dimentions in wall distance array
!     uerror = error output unit number
!     umemcom= MEMCOM output unit number
!
!     Output:
!
!     a      = for iact = 1: array to read from MEMCOM
!     istat  = 0 everything went ok
!
      implicit none
!
      integer np,ndim,nb,icycle,igrid,umemcom,uerror,istat,nw,iact
!
      character name*(*),label*30,type*4
!
      real*8 a(1:np,1:ndim)
!
!     local variables
!
      integer iex,iset,iadr
! 
      istat = 0
!
      call makelabl(name,nb,igrid,icycle,label,istat)
!
!     read dataset
!
      if (iact .eq. 1) then
!
!     check if array is stored in MEM-COM
!
         call inqsdb(umemcom,label,0,iex,istat)
!
         if (iex .eq. 1) then
!
!     read attributes of the data set
!
            call prpdb(umemcom,label,0,iset,type,nw,iadr)
!
            if (np*ndim .ne. nw) then
               write (uerror,'(3a,//,2(a,i4,/))') &
                  ' *** FATAL ERROR in m2c_iomemdb for dataset ', &
                    label,' ***', &
                  ' array size declared    = ',np*ndim, &
                  ' size found on data set = ',nw
               goto 105
            end if
!
!     read results from MEMCOM
!      
            call getdb(umemcom,label,0,a,type,nw,istat)
         else
            istat = -1
         end if
!
!     write dataset
!
      else if (iact .eq. 2) then
!
!     write results to dataset
!
         type = 'F'
         call dssndb(umemcom,label,np,ndim,type,0,istat)
         call putdb(umemcom,label,0,a,type,np*ndim,istat)
!
         if (istat .ne. 0) then
            write (uerror,'(3a)') &
               ' *** FATAL error in m2c_iomemdb when writing ', &
               label,' to MEM-COM'
            goto 105
         end if
      else
         write (uerror,'(a,i4,a)') &
         ' *** FATAL error in m2c_iomemdb, iact=',iact,' not defined'
         goto 105
      end if
!
      return

  105 continue
      call endadb
      stop
      end
