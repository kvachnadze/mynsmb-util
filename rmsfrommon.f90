     program rmsfrommon
      implicit none
      integer un,i,nl
      real*8 iter,var,time,rho,u0
      character *30 filename,dumline
!      character *160 header
!      character *14 format
      real(kind=8), dimension(:), allocatable :: u,v,w
      real(kind=8), dimension(:), allocatable :: uu,vv,ww,uv
      real*8 u_sum,v_sum,w_sum,uv_sum
      real*8 uu_sum,vv_sum,ww_sum,uuvv_sum
      real*8 u_av,uv_rms,u2_rms,v2_rms,w2_rms
!      real*8 rhoe,p,d1,d2,d3,d4,d5,d6,d7,d8,d9
!      real*8 d10,d11,d12,d13,d14,d15 
    
!      format = '(15(1x,g14.7))'
!      header = '#    Y            Y-Ywall            (Y-Ywall)/D            dy            yplus'
!       ll1 = len_trim(header)

      un=10
!      un2=11
      filename='monitor_point.85'

!      rho=1.1847274513
!      nu=0.0029579478/rho
!      diam=0.124465232
!      pos=11
      u0=69.2059
      u_sum = 0
      v_sum = 0
      w_sum = 0
      uu_sum = 0
      uv_sum = 0
      vv_sum = 0
      ww_sum = 0
      u_av = 0
      uv_rms=0
      u2_rms=0
      v2_rms=0
      w2_rms=0
      uuvv_sum=0

      call findlinetotal(un,filename,nl)
!      print *,'line numbers', nl
!      stop

!      nl=nl-1
!      allocate(time(nl))
      allocate(u(nl))
      allocate(v(nl))
      allocate(w(nl))
      allocate(uu(nl))
      allocate(vv(nl))
      allocate(ww(nl))
      allocate(uv(nl))

      open(un,file=filename)
!        read(un,*) dumline
        do i=1,nl    
          read(un,*) iter,var,time,rho,u(i),v(i),w(i)
!                     rhoe,p,d1,d2,d3,d4,d5,d6,d7,d8,d9,   &
!                     d10,d11,d12,d13,d14,d15
          u(i)=u(i)/rho
          v(i)=v(i)/rho
          w(i)=w(i)/rho
          uu(i)=u(i)**2
          vv(i)=v(i)**2
          ww(i)=w(i)**2
          uv(i)=u(i)*v(i)
        enddo
      close(un)

!        print *, 'check u(1),v(1),w(1)', u(1),v(1),w(1)
!        print *, 'check uu(1),vv(1),ww(1)', uu(1),vv(1),ww(1)
!        stop

      do i=1,nl
        u_sum = u_sum + u(i)
        v_sum = v_sum + v(i)
        w_sum = w_sum + w(i)

        uu_sum = uu_sum + uu(i)
        vv_sum = vv_sum + vv(i)
        ww_sum = ww_sum + ww(i)

        uv_sum = uv_sum + uv(i)
        uuvv_sum=uuvv_sum + uv(i)**2
      enddo

        print *, 'check u_sum, v_sum, w_sum', u_sum, v_sum, w_sum
        print *, 'check uu_sum, vv_sum, ww_sum', uu_sum, vv_sum, ww_sum


       u_av = u_sum / nl
       
!  Naive algorithm
       u2_rms=sqrt((uu_sum - u_sum**2/nl)/nl)/u0
       v2_rms=sqrt((vv_sum - v_sum**2/nl)/nl)/u0
       w2_rms=sqrt((ww_sum - w_sum**2/nl)/nl)/u0  
       uv_rms=sqrt((uuvv_sum - uv_sum**2/nl)/nl)/(u0**2)
       print *,'u_av/u0, u2_rms/u0, v2_rms/u0, w2_rms/u0, uv_rms/u0**2', u_av/u0, u2_rms, v2_rms, w2_rms, uv_rms

! 2-pass algorithm
       u2_rms=0
       v2_rms=0
       w2_rms=0
       uv_rms=0
      do i=1,nl
       u2_rms=u2_rms+(u(i)-u_av)*(u(i)-u_av)
      enddo  
       u2_rms=u2_rms/nl

       print *,'2pass algorithm u2_rms', u2_rms

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
