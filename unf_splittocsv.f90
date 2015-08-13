      program unfsplittocsv
      implicit none
      integer nptot(4)
      character *60 fname(4),dummy,cname
      integer i,j,n
      integer, parameter :: iter=10,nstep=1000
      integer nfile,unit1,us(4)
!      character(30), dimension(:), allocatable :: cname
      integer, dimension(:), allocatable :: un1,un2
      character(100) :: fileplace1, fileplace2
      integer nsurf,cstep
!      real*8 time
      real*8 rho(4),p(4),u(4),v(4),w(4)


!        call findlinetotal(10,'intsurface.81',n)
!        print *, n

        do i=1,4
          fname(i) = 'intsurface.8'//str(i)
          us(i) = i+10
          open(unit=us(i),status='old',file=trim(adjustl(fname(i))),form='unformatted')
        enddo

!         read(us(1),*) dummy
!         print *, dummy       
!         stop


      call system('mkdir csvfiles1')
      call system('mkdir csvfiles2')

      fileplace1 = "/scratch/kvachnadze/3Dsplitter/FWH_test/cylinder/csvfiles1/"
      fileplace2 = "/scratch/kvachnadze/3Dsplitter/FWH_test/cylinder/csvfiles2/"


      nfile = nstep/iter
!      allocate(cname(nfile))
      allocate(un1(nfile))
      allocate(un2(nfile))
      do i=1,nfile
!        cname(i) = trim(adjustl(str(iter*i)))//'.csv'
        call getcsvname(nstep,iter*i,cname)
        un1(i) = iter*i + 101
        un2(i) = iter*i + 102
!        print
        open(unit=un1(i),file=trim(adjustl(fileplace1))//trim(cname))
        open(unit=un2(i),file=trim(adjustl(fileplace2))//trim(cname))
      enddo    
!        print *, trim(adjustl(cname(i)))


      do j=1,4
        i=0
      do while (.true.)
        i=i+1
!      do i=1,n
!        print *,'j,  n  ',j,n
        read(us(j),END=999) nsurf,cstep,rho(j),p(j),u(j),v(j),w(j)
        n=i
        unit1 = cstep+nsurf+100
        write(unit1,103) rho(j),p(j),u(j),v(j),w(j)
      enddo
        999 continue
        print *,'j,  n,  ',j,n
      enddo




      do i=1,4
        close(us(i))
      enddo
      do i=1,nfile
        close(un1(i))
        close(un2(i))
      enddo      
!
      deallocate(un1,un2)
!
! 102  format(1x,i2,1x,i7,e13.6,4(1x,i4),5(30e15.7))
 103  format(5(30e15.7))    
!  
      contains
!
      function str(k)
        !   "Convert an integer to string."
        character(len=20)   :: str
        integer, intent(in) :: k        
        write (str, *) k
        str = trim(adjustl(str))
      end function str

      end program

      subroutine getcsvname(nstep,istep,cname)
      implicit none
      integer, intent(in) :: nstep,istep
      character(*), intent(out) :: cname
      character*20 cdum
      integer ndig, ndum
      real rstep

!     calculate the number of digits in nstep
      rstep = 1.0*nstep
      ndig = int(log10(rstep)) + 1
!     calculate dummy integer with ndig+1 digits to cut after
      ndum = 10**ndig + istep
!     adjusting string to the left side
      cdum = adjustl(str(ndum))
!     cutting the first digit to have a proper name
      cname = trim(cdum(2:(ndig+1)))//'.csv'

      contains
!
      function str(k)
        !   "Convert an integer to string."
        character(len=20)   :: str
        integer, intent(in) :: k        
        write (str, *) k
        str = trim(adjustl(str))
      end function str

      end

