     program coor_splitter
      implicit none
      integer un,i,nl,un2,pos,ll1,j
      real*8 x,z,rho,nu,diam
      character *30 filename,dumline
      character *160 header
      character *14 format
      real(kind=8), dimension(:), allocatable :: y,ywall,dy
      real(kind=8), dimension(:,:), allocatable :: yplus
      real*8 tauw(11),koef(11)

      format = '(15(1x,g14.7))'
      header = '#    Y            Y-Ywall            (Y-Ywall)/D            dy            yplus'
       ll1 = len_trim(header)

      un=10
      un2=11
      filename='coor_y'

      rho=1.1847274513
      nu=0.0029579478/rho
      diam=0.124465232
      pos=11
! 1,3,5,6,8,9,11
      tauw(1)=-4.546
      tauw(2)=-45.18
      tauw(3)=27.38
      tauw(4)=27.056
      tauw(5)=21.42
      tauw(6)=18.242
      tauw(7)=19.555
      tauw(8)=19.41
      tauw(9)=21.24
      tauw(10)=24.87
      tauw(11)=34.36
! calculate u_tau/nu
      do i=1,pos
        koef(i)=sqrt(abs(tauw(i))/rho)/nu
      enddo


      call findlinetotal(un,filename,nl)
      nl=nl-1
      allocate(y(nl))
      allocate(ywall(nl))
      allocate(dy(nl))
      allocate(yplus(pos,nl))

      open(un,file=filename)
        read(un,*) dumline
        do i=1,nl    
          read(un,*) x,y(i),z
        enddo
      close(un)

      
      open(un2,file='coor_y_yplus')
        write(un2,'(a)') header(1:ll1)
        do i=1,nl
          ywall(i)=y(i)-y(1)
          if (i.eq.1) then
            dy(1)=0
            do j=1,pos
              yplus(j,1)=0
            enddo
          else
            dy(i)=y(i)-y(i-1)
            do j=1,pos 
              yplus(j,i)=koef(j)*ywall(i)
            enddo
          endif
          write(un2,format) y(i),ywall(i),ywall(i)/diam,dy(i),(yplus(j,i),j=1,pos)
        enddo
      close(un2)     





     end

      subroutine findlinetotal(un,filename,n)   

!     This routine is used to determine the number of filled lines in a text file
!     created by KV

      implicit none

      integer, intent(in)    :: un ! file unit number 
      character filename*(*)       ! filename
      integer, intent(out)    :: n ! output

!     Local variables
      integer i
      character*30 dumline
      i=0

      open(un,file=filename)
        do while (.true.)
          i=i+1
          read(un,*,END=999) dumline
          n=i
        enddo
          999 continue
      close(un)

      end
