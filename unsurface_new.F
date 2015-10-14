      subroutine unsurface_new(myunit,myi,w,p,
     &                   n1,n2,n3,nnu,
     &                   nstep,nstepold)
!
      implicit none
!
!
!----------------------------------------------------------------------
! --- ARGUMENTS
!----------------------------------------------------------------------
!
      integer, intent(in) :: n1,n2,n3,nnu,myunit,
     &                       nstep,nstepold,myi
!      real(kind=8), intent(in) :: time
!
      real(kind=8), intent(in) ::
     &              w      (-1:n1+2,-1:n2+2,-1:n3+2,1:nnu),
     &              p      (-1:n1+2,-1:n2+2,-1:n3+2) 
!
!----------------------------------------------------------------------
! --- LOCAL VARIABLES
!----------------------------------------------------------------------
!
!    nsurf - number of surfaces in a main blockset
      integer, parameter :: nsurf = 2
!    nsurf2 - number of surfaces in a second blockset (see example)
      integer, parameter :: nsurf2 = 1
      integer      :: i,j,k
!      real(kind=8) :: invt,local_statrms(1:nvar)
      real(kind=8), parameter :: one=1.0, zero = 0.

!KV
!      character(len=50) :: myfilename,command
      integer :: myj,isf,jfix
      integer :: jf(nsurf,2),jf2(nsurf2,2)
      integer :: myn1,myn2,myn3
      real(kind=8) :: myout(5)
!
!    EXAMPLE
!         integer blockset(4)
!         data blockset /9,12,10,11/
!    OR (IF WE NEED MORE BLOCKSETS)
!         integer blockset(8)
!         data blockset /9,12,10,11,5,8,6,7/
!
!    So, RESTRICTED TO 2 BLOCKSETS
!    CAN BE REWRITTEN FOR MORE
!    BUT IT IS NOT SO USEFUL
!

!
!!! MODIFICATIONS
! 15/09 - commented the zero's for the 1st surface. I am going to use 
! velocity data because they are not on the surface directly
!

!
!----------------------------------------------------------------------
! --- START ROUTINE
!----------------------------------------------------------------------
!
!     OUTPUT (7 numbers in a line):
!     Nsurf, Nstep, DENS, PRES, VELO(u,v,w)
!
!     Writing out the surface#1 which is 10(can be defined)
!     cells out of the cylinder
!     Blocks in which this surface is - defined manually 
!     here in "blockset" and in module convergence monitor variables
!     i,j,k inidices can be mixed (like here) that is why
!     jfix is found manually (jfix=10 and 41 in this case)
!
!     Writing out the surface#2 which is cylinder surface.
!     In this case - without interpolation 
!     we are writing out 1st cell dens and pres values

!    Introduce jfix for 2 surfaces
!
! data for O_mesh_coarse
!          jf(1,1) = 10
!          jf(1,2) = 41
!          jf(2,1) = 49
!          jf(2,2) = 1
!
!          jf2(1,1) = 10
!          jf2(1,2) = 41
!
! data for O_mesh_fine
          jf(1,1) = 50
          jf(1,2) = 51
          jf(2,1) = 70
          jf(2,2) = 31
!
          jf2(1,1) = 394
          jf2(1,2) = 107
!
!
          do isf = 1,nsurf
!
!         isf = 2 HERE FOR CYLINDER SURFACE
!         IF NOT - COMMENT THE APPROPRIATE IF/THEN BELOW
!
          if (myi.eq.1) then
            jfix=jf(isf,1)
            do i=1,n1
              do j=1,n2                 
                myout(1)=w(i,j,jfix,1)
                myout(2)=p(i,j,jfix)
!                if (isf.eq.1) then
                  myout(3)=w(i,j,jfix,2)/w(i,j,jfix,1)
                  myout(4)=w(i,j,jfix,3)/w(i,j,jfix,1)
                  myout(5)=w(i,j,jfix,4)/w(i,j,jfix,1)
!                else
!                  myout(3)=zero
!                  myout(4)=zero
!                  myout(5)=zero
!                endif 
                write(myunit) isf,nstep+nstepold,
     &                            (myout(myj),myj=1,5)
              enddo
            enddo
          endif
!
          if (myi.eq.2) then
            jfix=jf(isf,2)   
            do j=1,n2
              do i=1,n1
                myout(1)=w(i,j,jfix,1)
                myout(2)=p(i,j,jfix)
!                if (isf.eq.1) then
                  myout(3)=w(i,j,jfix,2)/w(i,j,jfix,1)
                  myout(4)=w(i,j,jfix,3)/w(i,j,jfix,1)
                  myout(5)=w(i,j,jfix,4)/w(i,j,jfix,1)
!                else
!                  myout(3)=zero
!                  myout(4)=zero
!                  myout(5)=zero
!                endif
                write(myunit) isf,nstep+nstepold,
     &                            (myout(myj),myj=1,5)
              enddo
            enddo
          endif
!
          if (myi.eq.3) then
            jfix=jf(isf,2)
            do i=n1,1,-1   
              do j=1,n2             
                myout(1)=w(i,j,jfix,1)
                myout(2)=p(i,j,jfix)
!                if (isf.eq.1) then
                  myout(3)=w(i,j,jfix,2)/w(i,j,jfix,1)
                  myout(4)=w(i,j,jfix,3)/w(i,j,jfix,1)
                  myout(5)=w(i,j,jfix,4)/w(i,j,jfix,1)
!                else
!                  myout(3)=zero
!                  myout(4)=zero
!                  myout(5)=zero
!                endif
                write(myunit) isf,nstep+nstepold,
     &                            (myout(myj),myj=1,5)
              enddo
            enddo
          endif
!
          if (myi.eq.4) then
            jfix=jf(isf,1)   
            do j=n2,1,-1
              do i=1,n1
                myout(1)=w(i,j,jfix,1)
                myout(2)=p(i,j,jfix)
!                if (isf.eq.1) then
                  myout(3)=w(i,j,jfix,2)/w(i,j,jfix,1)
                  myout(4)=w(i,j,jfix,3)/w(i,j,jfix,1)
                  myout(5)=w(i,j,jfix,4)/w(i,j,jfix,1)
!                else
!                  myout(3)=zero
!                  myout(4)=zero
!                  myout(5)=zero              
!                endif
                write(myunit) isf,nstep+nstepold,
     &                            (myout(myj),myj=1,5)
              enddo
            enddo
          endif
!


!
!
          enddo

! IN THE CASE IF WE WANT TO EXTRACT SURFACES FROM 
! DIFFERENT BLOCKSETS THEN
! ONE CAN CHANGE THE intset AND blockset var's IN 
! convergence_monitor_variables module AND USE nsurf2

       if (nsurf2.gt.0) then

          do isf = 1,nsurf2

          if (myi.eq.5) then
            jfix=jf2(isf,1)
            do i=1,n1
              do j=1,n2                 
                myout(1)=w(i,j,jfix,1)
                myout(2)=p(i,j,jfix)
                  myout(3)=w(i,j,jfix,2)/w(i,j,jfix,1)
                  myout(4)=w(i,j,jfix,3)/w(i,j,jfix,1)
                  myout(5)=w(i,j,jfix,4)/w(i,j,jfix,1)
                write(myunit) isf+nsurf,nstep+nstepold,
     &                            (myout(myj),myj=1,5)
              enddo
            enddo
          endif
!
          if (myi.eq.6) then
            jfix=jf2(isf,2)   
            do j=1,n2
              do i=1,n1
                myout(1)=w(i,j,jfix,1)
                myout(2)=p(i,j,jfix)
                  myout(3)=w(i,j,jfix,2)/w(i,j,jfix,1)
                  myout(4)=w(i,j,jfix,3)/w(i,j,jfix,1)
                  myout(5)=w(i,j,jfix,4)/w(i,j,jfix,1)
                write(myunit) isf+nsurf,nstep+nstepold,
     &                            (myout(myj),myj=1,5)
              enddo
            enddo
          endif
!
          if (myi.eq.7) then
            jfix=jf2(isf,2)
            do i=n1,1,-1   
              do j=1,n2             
                myout(1)=w(i,j,jfix,1)
                myout(2)=p(i,j,jfix)
                  myout(3)=w(i,j,jfix,2)/w(i,j,jfix,1)
                  myout(4)=w(i,j,jfix,3)/w(i,j,jfix,1)
                  myout(5)=w(i,j,jfix,4)/w(i,j,jfix,1)
                write(myunit) isf+nsurf,nstep+nstepold,
     &                            (myout(myj),myj=1,5)
              enddo
            enddo
          endif
!
          if (myi.eq.8) then
            jfix=jf2(isf,1)   
            do j=n2,1,-1
              do i=1,n1
                myout(1)=w(i,j,jfix,1)
                myout(2)=p(i,j,jfix)
                  myout(3)=w(i,j,jfix,2)/w(i,j,jfix,1)
                  myout(4)=w(i,j,jfix,3)/w(i,j,jfix,1)
                  myout(5)=w(i,j,jfix,4)/w(i,j,jfix,1)
                write(myunit) isf+nsurf,nstep+nstepold,
     &                            (myout(myj),myj=1,5)
              enddo
            enddo
          endif
! 
!
         enddo
!
       endif
!
!
!          print *,'I am here2'
!
! 102  format(1x,i2,1x,i7,e13.6,4(1x,i4),5(30e15.7))
!      call dump_file(myunit)
!
!      return
      end
