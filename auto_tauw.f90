      program auto_tauw
      implicit none
      integer ll1,i,nout,nbunit
      integer tcalc,dbset,nl
      character *30 label,cbuf
      character *60 dbname
      character type*4
      character(len=200):: header
      character(len=14) :: form
      character(len=80) :: filename,command,resultfile
!      logical :: exists
      real(kind=8) y,z,d1,d2,d3,d4,d5,d6,d7
      real(kind=8) rho,uinf,mu,y_wall,y1_c,dy_wall,dz
!  Memcom parametrs
      integer umemcom,istat,iex,igrid,ic
!
      integer, parameter :: dball=33,nl_max=1000

      character baseset(dball)*14
      real(kind=8) x_out(nl_max),Rex(nl_max)
      real(kind=8) tauwx(dball,nl_max), tauwx_sum(nl_max), tauwx_sum2(nl_max)
      real(kind=8) tauwx_av(nl_max), tauwx_av2(nl_max), tauwx_rms(nl_max)
      real(kind=8) cfx_av(nl_max), cfx_rms(nl_max),u_tau(nl_max)
      real(kind=8) yplus(nl_max),xplus(nl_max),dx(nl_max),zplus(nl_max)
      real timeset(dball)
      rho=1.1847274513
      mu=0.0029579478
      uinf=69.2058957026
      y_wall=0.062232616
      y1_c=0.06256562
      dy_wall=2*(y1_c-y_wall)
      dz=1.2*0.31116308/66
!
      nl=0
      ic=0
!  
      resultfile = 'auto_tauw_result'

      tauwx_sum = 0
      tauwx_sum2 = 0
      tauwx_av = 0
      tauwx_av2 = 0
      tauwx_rms = 0 
!
      igrid = 0
!

         header = '#    X            Rex            Tauwx_av'// &
         '            Tauvx_RMS            Cfx_av            Cfx_rms'// &
         '                 xplus                      yplus'// &
         '                 zplus'
       ll1 = len_trim(header)
       form = '(9(1x,g14.7))'
        
       nout=11
       nbunit=10
       umemcom=0

! fill array with name of databases
!      do i=1,dball
!        tcalc=70+i+(i-1)
!        baseset(i)='time_0.'//trim(str(tcalc))//'.db'
!        timeset(i)=tcalc/100
!        print *,baseset(i)
!      enddo

      do i=1,dball
        tcalc=44+i+(i-1)
        timeset(i)=tcalc/100.0
!        print *,tcalc,timeset(i)
        baseset(i)='time_'//trim(str_f(timeset(i)))//'.db'
        print *,baseset(i)
      enddo
!      stop

! Initialize Memcom
     call initmc(1,6,6)
!
 ! LOOP over databases
     do dbset=1,dball
        dbname=trim(baseset(dbset))

        call opdidb(umemcom+dbset,dbname,0,' ',istat)
! ... find the cycle number of the base
        do i=1,30
          call makelabl('CDES',0,igrid,i,cbuf,istat)
          call inqsdb(umemcom+dbset,cbuf,0,iex,istat) 
          if (iex == 1) then
            ic=i
            EXIT
          end if
        enddo
        print *,ic

      
      command = './script.exp '//trim(dbname)//' '//trim(str(ic))//''
! ... delete old output file if exists     
!      inquire(file=filename, exist=exists)
!      print *, exists
!      if (exists) then
!         CALL system(command)
!      end if
      CALL system(command)
      filename = 'extract.'//trim(dbname)//'.'//trim(str(ic))//''
      print *,'FILENAME = ', filename
      CALL findlinetotal(nout+dbset,filename,nl)
      print *,'NUM of lines = ',nl      

      open(nout+dbset,file=filename,status='OLD')
        read(nout+dbset,*)
        do i=1,nl-1
          read(nout+dbset,*) x_out(i),y,z,d1,tauwx(dbset,i),d2,d3,d4,d5,d6,d7
        enddo
      close(nout+dbset)
      do i=1,nl
        tauwx_sum(i)=tauwx_sum(i)+tauwx(dbset,i)
        tauwx_sum2(i)=tauwx_sum2(i)+tauwx(dbset,i)**2  
      enddo


!...  closing loop over databases
     end do
      
! calculate statistical values

      open(unit=nbunit,status='unknown',file=resultfile)
      write(nbunit,'(a)') header(1:ll1)


       do i=1,nl-1
         if ((x_out(i).lt.1.712).and.(x_out(i).ge.0.0)) then
           Rex(i)=rho*uinf*x_out(i)/mu    
           tauwx_av(i)=tauwx_sum(i)/dball
           tauwx_av2(i)=tauwx_sum2(i)/dball
           tauwx_rms(i)=sqrt(tauwx_av2(i)-tauwx_av(i)**2)            
           cfx_av(i)=tauwx_av(i)/(0.5*rho*uinf**2)
           cfx_rms(i)=tauwx_rms(i)/(0.5*rho*uinf**2)
           u_tau(i)=sqrt(abs(tauwx_av(i))/rho)
           yplus(i)=dy_wall*u_tau(i)*rho/mu
           dx(i)=x_out(i+1)-x_out(i)
           xplus(i)=dx(i)*u_tau(i)*rho/mu
           zplus(i)=dz*u_tau(i)*rho/mu
           write(nbunit,form) x_out(i),Rex(i),tauwx_av(i),tauwx_rms(i),cfx_av(i),cfx_rms(i),xplus(i),yplus(i),zplus(i)
         end if
       enddo

      close(nbunit)


      contains
!
      function str(k)
        !   "Convert an integer to string."
        character(len=20)   :: str
        integer, intent(in) :: k        
        write (str, *) k
        str = adjustl(str)
      end function str

      function str_f(k)
        !   "Convert a float to string."
        character(len=20)   :: str_f,format
        real, intent(in) :: k
        data format  /'(F10.2)'/
        write (str_f, format) k
        str_f = adjustl(str_f)
      end function str_f

!
      end program


      subroutine findlinetotal(un,filename,n)   

!     This routine is used to determine the number of filled lines in a text file
!     created by KV

      implicit none

      integer, intent(in)    :: un ! file unit number 
      character filename*(*)       ! filename
      integer, intent(out)    :: n ! output

!     Local variables
      integer i
      character*100 dumline
      i=0

      open(un,file=filename)
        do while (.true.)
          i=i+1
!          print *,'check n for nl',i
          read(un,*,END=999) dumline
          n=i
        enddo
          999 continue
      close(un)

      end

