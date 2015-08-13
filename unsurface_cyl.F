      subroutine unsurface_cyl(myunit,myi,w,p,
     &                   n1,n2,n3,nnu,
     &                   time,nstep,nstepold)
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
      real(kind=8), intent(in) :: time
!
      real(kind=8), intent(in) ::
     &              w      (-1:n1+2,-1:n2+2,-1:n3+2,1:nnu),
     &              p      (-1:n1+2,-1:n2+2,-1:n3+2) 
!
!----------------------------------------------------------------------
! --- LOCAL VARIABLES
!----------------------------------------------------------------------
!
      integer, parameter :: nsurf = 2
      integer      :: i,j,k,nb
!      real(kind=8) :: invt,local_statrms(1:nvar)
      real(kind=8), parameter :: one=1.0, zero = 0.

!KV
!      character(len=50) :: myfilename,command
      integer :: myj,isf,jfix,jf(nsurf,2)
      integer :: myn1,myn2,myn3
      real(kind=8) :: myout(5)

         integer blockset(4)
         data blockset /9,12,10,11/

!
!----------------------------------------------------------------------
! --- START ROUTINE
!----------------------------------------------------------------------
!
!     OUTPUT (12 numbers in a line):
!     Nsurf, Nstep, Time, Nblock, i, j, k, DENS, PRES, VELO(u,v,w)
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
          jf(1,1) = 10
          jf(1,2) = 41
          jf(2,1) = 49
          jf(2,2) = 1
!
          nb = blockset(myi)
!
!          print *,'I am here1'
!          print *, 'myi,n1,n2,n3',myi,n1,n2,n3
!
!
          do isf = 1,nsurf
!
          if (myi.eq.1) then
            jfix=jf(isf,1)
            do i=1,n1
              do j=1,n2                 
                myout(1)=w(i,j,jfix,1)
                myout(2)=p(i,j,jfix)
                if (isf.eq.1) then
                  myout(3)=w(i,j,jfix,2)/w(i,j,jfix,1)
                  myout(4)=w(i,j,jfix,3)/w(i,j,jfix,1)
                  myout(5)=w(i,j,jfix,4)/w(i,j,jfix,1)
                else
                  myout(3)=zero
                  myout(4)=zero
                  myout(5)=zero
                endif 
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
                if (isf.eq.1) then
                  myout(3)=w(i,j,jfix,2)/w(i,j,jfix,1)
                  myout(4)=w(i,j,jfix,3)/w(i,j,jfix,1)
                  myout(5)=w(i,j,jfix,4)/w(i,j,jfix,1)
                else
                  myout(3)=zero
                  myout(4)=zero
                  myout(5)=zero
                endif
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
                if (isf.eq.1) then
                  myout(3)=w(i,j,jfix,2)/w(i,j,jfix,1)
                  myout(4)=w(i,j,jfix,3)/w(i,j,jfix,1)
                  myout(5)=w(i,j,jfix,4)/w(i,j,jfix,1)
                else
                  myout(3)=zero
                  myout(4)=zero
                  myout(5)=zero
                endif
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
                if (isf.eq.1) then
                  myout(3)=w(i,j,jfix,2)/w(i,j,jfix,1)
                  myout(4)=w(i,j,jfix,3)/w(i,j,jfix,1)
                  myout(5)=w(i,j,jfix,4)/w(i,j,jfix,1)
                else
                  myout(3)=zero
                  myout(4)=zero
                  myout(5)=zero              
                endif
                write(myunit) isf,nstep+nstepold,
     &                            (myout(myj),myj=1,5)
              enddo
            enddo
          endif
!
!
!
          enddo
!
!
!          print *,'I am here2'
!
! 102  format(1x,i2,1x,i7,e13.6,4(1x,i4),5(30e15.7))
!      call dump_file(myunit)
!
!      return
      end
