      program unfsplittocsv
      implicit none
!     PARAMETERS
      integer, parameter :: iter=10,nstep=27000,intset=8
      integer, parameter :: surftot=3
!
      character *60 fname,dummy,cname
      integer i,j,n
      integer nfile,unit1,us(intset)
!      character(30), dimension(:), allocatable :: cname
!      integer, dimension(:), allocatable :: un1,un2
      integer, dimension(:,:), allocatable :: un
      character(100) :: fileplace(surftot)
      character*15 :: command
      character*1 :: nsdum
      integer nsurf,cstep
!      real*8 time
      real*8 rho,p,u,v,w


!        call findlinetotal(10,'intsurface.81',n)
!        print *, n

        do i=1,intset
          fname = 'intsurface.8'//str(i)
          us(i) = i+10
          open(unit=us(i),status='old',file=trim(adjustl(fname)),form='unformatted', &
                    access='sequential',convert='big_endian')
        enddo

!         read(us(1),*) dummy
!         print *, dummy       
!         stop

      do i=1,surftot
        nsdum = trim(adjustl(str(i)))
        command = 'mkdir csvfiles'//nsdum
        call system(command)
        fileplace(i) = "/naslx/ptmp/12/di34pip2/Omesh_fine_FWH/csvfiles"//nsdum//"/"
      enddo


      nfile = nstep/iter
!      allocate(cname(nfile))
! !     allocate(un1(nfile))
!      allocate(un2(nfile))
      allocate(un(surftot,nfile))
      do i=1,nfile
        call getcsvname(nstep,iter*i,cname)
        do j=1,surftot
          un(j,i) = iter*i + j + 100
!          un1(i) = iter*i + 101
!          un2(i) = iter*i + 102
           open(unit=un(j,i),file=trim(adjustl(fileplace(j)))//trim(cname))
!          open(unit=un1(i),file=trim(adjustl(fileplace1))//trim(cname))
!          open(unit=un2(i),file=trim(adjustl(fileplace2))//trim(cname))
        enddo
      enddo    
!        print *, trim(adjustl(cname(i)))


      do j=1,intset
        i=0
      do while (.true.)
        i=i+1
        read(us(j),END=999) nsurf,cstep,rho,p,u,v,w
        n=i
        unit1 = cstep+nsurf+100
        write(unit1,103) rho,p,u,v,w
      enddo
        999 continue
!        print *,'j,  n,  ',j,n
      enddo



      do i=1,intset       
        close(us(i))
      enddo

      do j=1,surftot
        do i=1,nfile         
          close(un(j,i))
        enddo
      enddo
!      
!
      deallocate(un)
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

