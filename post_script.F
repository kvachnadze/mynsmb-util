      program post_script
      implicit none
      integer uerror,umemcom,umemcom2,uout,nset,nnu
      integer nxc,tcalc,n2glob,dbset,nbunit,uprnt
      integer nblock,n1,n2,n3,iprec,istat,nb,igrid,ic,np,istat2
      integer i,j,k,l,iex,ll1,ll2,n1def,n2def,igrid2,np_coor
      character *30 label,cbuf
      character *60 dbname,dbname2
      character type*4
      character(len=320):: header
      character(len=14) :: form
      character(len=80) :: filename,command,filevelo7
      character(len=10) :: i2char
      logical :: exists
      integer i2d, nsurf
      real(kind=8), dimension(:,:,:,:), allocatable :: velo,velo2
      real(kind=8), dimension(:,:,:)  , allocatable :: pscalar,pscalar2
      real(kind=8), dimension(:,:,:,:), allocatable :: coor,xy,coorc
      real(kind=8), dimension(:,:,:), allocatable :: u7,v7,w7,dudy
      real(kind=8), dimension(:,:), allocatable :: u7_sum,v7_sum,w7_sum
      real(kind=8), dimension(:,:), allocatable :: uu7_sum,vv7_sum,dudy_sum
      real(kind=8), dimension(:,:), allocatable :: dudy_sum,dudy_av
      real(kind=8), dimension(:,:), allocatable :: uv7_sum,uuvv7_sum
      real(kind=8), dimension(:,:), allocatable :: u7_av,v7_av,w7_av
      real(kind=8), dimension(:,:), allocatable :: uu7_av,vv7_av,ww7_av
      real(kind=8), dimension(:,:), allocatable :: uv7_av,uuvv7_av
      real(kind=8), dimension(:,:), allocatable :: u7rms,v7rms,w7rms
      real(kind=8), dimension(:,:), allocatable :: yc,ww7_sum,uv7rms
      real(kind=8), dimension(:), allocatable :: xc,utau,tauw
      real(kind=8), dimension(:,:,:,:), allocatable :: du,dv,dw,w,work
      real(kind=8), dimension(:,:,:,:,:), allocatable :: s     
      real(kind=8), dimension(:,:,:), allocatable :: vol,p
      real(kind=8), dimension(:,:), allocatable :: urmsplus,vrmsplus
      real(kind=8), dimension(:,:), allocatable :: wrmsplus,yplus
c
c
      real(kind=8) velosum,psum,u0,diam
c      real(kind=8), dimension(:,:,:)  , allocatable :: xyz
c      real(kind=8), dimension(:)  , allocatable :: y
      real(kind=8) rho,nu
c
      integer, parameter :: intset=22,nres=4,dball=33,intset_wall=11
      character baseset(dball)*24
      real timeset(dball)
      integer blockset(intset)
c
c
      u0=69.2059
      diam=0.124465232
      timeset = 0
      tcalc = 0
      rho=1.1847274513
      nu=0.0029579478/rho
c
c      data dataset /'DENS','PRES','TEMP'/
       data blockset /120,156,160,194,198,220,224,238,242,248,246,
     &                118,122,158,162,196,200,222,226,240,244,236/
c       data baseset /120,156,160,194,198,220,224,238,242,248,246/
         header = '#    X            Y            U_av'//
     &   '            V_av            W_av            Urms'//
     &   '            Vrms            Wrms            UVrms'//
     &   '            dU/dy            dU/dy*UVrms'//
     &   '        y+              Urms+         Vrms+'//
     &   '        Wrms+'
       ll1 = len_trim(header)
c       print *, ll1
       form = '(15(1x,g14.7))'
c      
      do i=1,dball
        tcalc=44+i+(i-1)
        timeset(i)=tcalc/100.0
c        print *,tcalc,timeset(i)
        baseset(i)='time_'//trim(str_f(timeset(i)))//'.db'
        print *,baseset(i)
      enddo
c      stop
c

      i2d = 0
c     i2d=0 for 3D calculation
      nsurf = 1
c     nsurf - velocity gradients of surfaces in the direction nsurf

      umemcom = 2
      umemcom2 = 1
      uerror = 6
      uprnt=6
      nnu=5
      uout = 10
      filename='extract_blayer_top_zaverage'
      command = 'rm '//trim(filename)
      type     = 'F'
c
c ... delete old output file if exists     
      inquire(file=filename, exist=exists)
      print *, exists
      if (exists) then
         CALL system(command)
      end if
c
      dbname2='convmesh.memcom.db'
c      dbname='convmesh.memcom.db.cycle5.3'
      igrid2=0
      igrid=0
c

c ... checking and reading the base with MESH
      call filedb(dbname2,istat2)
      !print *, istat
      if (istat2 .lt. 0) then
        print '(a,$)','Data base with mesh does not exist '
        stop
      endif
c
      call initmc(1,6,6)
      call opdidb(umemcom2,dbname2,1,' ',istat2)
c
c ... read ADIR descriptor
c
      call getdes(umemcom2,'ADIR',0,istat2)
      call getbwi('NBR',nblock,istat2)
      call gprmdb('NWFLOAT',iprec,istat2)
c ... obtain the number of cells in y direction (need global one)
      call makelabl('MDES',blockset(1),igrid2,0,cbuf,istat2)
      call gettab(umemcom2,cbuf,0,istat2)
      call getbwi('N2',n2glob,istat2)
c
c... allocating global arrays
      allocate(u7(dball,intset,n2glob))
      allocate(v7(dball,intset,n2glob))
      allocate(w7(dball,intset,n2glob))
      allocate(dudy(dball,intset,n2glob))
      allocate(u7_sum(intset,n2glob))
      allocate(v7_sum(intset,n2glob))
      allocate(w7_sum(intset,n2glob))
      allocate(dudy_sum(intset,n2glob))

      allocate(uu7_sum(intset,n2glob))
      allocate(vv7_sum(intset,n2glob))
      allocate(ww7_sum(intset,n2glob))
      allocate(uv7_sum(intset,n2glob))
      allocate(uuvv7_sum(intset,n2glob))
c
      allocate(u7_av(intset,n2glob))
      allocate(v7_av(intset,n2glob))
      allocate(w7_av(intset,n2glob))
      allocate(dudy_av(intset,n2glob))

      allocate(uv7_av(intset,n2glob))
      allocate(uuvv7_av(intset,n2glob))
      allocate(uu7_av(intset,n2glob))
      allocate(vv7_av(intset,n2glob))
      allocate(ww7_av(intset,n2glob))
      allocate(u7rms(intset,n2glob))
      allocate(v7rms(intset,n2glob))
      allocate(w7rms(intset,n2glob))
      allocate(uv7rms(intset,n2glob))
c
      allocate(yc(intset,n2glob))
      allocate(xc(intset))
c
      allocate(yplus(intset,n2glob))
      allocate(urmsplus(intset,n2glob))
      allocate(vrmsplus(intset,n2glob))
      allocate(wrmsplus(intset,n2glob))
      allocate(utau(intset))
      allocate(tauw(intset))
c
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
      do i=12,intset
        tauw(i)=tauw(i-11)
c        print *,tauw(i)
      enddo

c
      u7 = 0
      v7 = 0
      w7 = 0
      dudy = 0
      w7_sum = 0
      v7_sum = 0
      u7_sum = 0
      dudy_sum = 0
      ww7_sum = 0
      vv7_sum = 0
      uu7_sum = 0
      uv7_sum = 0
      uuvv7_sum = 0
c
      w7_av = 0
      v7_av = 0
      u7_av = 0
      dudy_av = 0
      ww7_av = 0
      vv7_av = 0
      uu7_av = 0
      uv7_av = 0
      uuvv7_av = 0
      u7rms = 0
      v7rms = 0
      w7rms = 0
      uv7rms = 0
      yc = 0
      xc = 0
c      filevelo7='bl_7positions'
c
c
c... loop over the solution databases
      do dbset=1,dball
c
      umemcom=1+dbset
      dbname=trim(baseset(dbset))
c ... initialize the database with solution     
      call opdidb(umemcom,dbname,0,' ',istat)      
      call filedb(dbname,istat)
c      print *, dbset
      if (istat .lt. 0) then
        print '(a,$)','Data base does not exist '
        stop
      endif
c ... find the cycle number of the base
      do i=1,20
        call makelabl('CDES',0,igrid,i,cbuf,istat)
        call inqsdb(umemcom,cbuf,0,iex,istat) 
        if (iex == 1) then
          ic=i
          EXIT
        end if
      enddo
      print *,ic
c
c
c ... looping over the set of blocks
      do nset=1,intset
c
c ... read MDES to obtain grid size
         nb=blockset(nset)
         call makelabl('MDES',nb,igrid2,0,cbuf,istat2)
         call gettab(umemcom2,cbuf,0,istat2)
c
         call getbwi('N1',n1,istat2)
         call getbwi('N2',n2,istat2)
         call getbwi('N3',n3,istat2)
c         print *,n2
         np_coor = (n1+1)*(n2+1)*(n3+1)
	 np = (n1+2)*(n2+2)*(n3+2)
         nxc = INT(n1/2)
c         filevelo7='velo7_block'//trim(str(nb))//'_time'//
c     &                    timeset(dbset)
c   
         allocate(pscalar(0:n1+1,0:n2+1,0:n3+1))
         allocate(velo(3,0:n1+1,0:n2+1,0:n3+1))
         allocate(coor(3,n1+1,n2+1,n3+1))
         allocate(coorc(3,0:n1+1,0:n2+1,0:n3+1))
         allocate(pscalar2(intset,0:n1+1,0:n2+1))
         allocate(velo2(intset,3,0:n1+1,0:n2+1))
         allocate(xy(intset,2,0:n1+1,0:n2+1))

        allocate(s(-1:n1+2,-1:n2+2,-1:n3+2,1:3,1:3))
        allocate(vol(-1:n1+2,-1:n2+2,-1:n3+2))
        allocate(work(1:n1+2,1:n2+2,1:n3+2,1:3))
        allocate(w(1:n1+2,1:n2+2,1:n3+2,nnu))
        allocate(p(1:n1+2,1:n2+2,1:n3+2))

        allocate(du(-1:n1+2,-1:n2+2,-1:n3+2,3))
        allocate(dv(-1:n1+2,-1:n2+2,-1:n3+2,3))
        allocate(dw(-1:n1+2,-1:n2+2,-1:n3+2,3))
c
c         obtain data from memcom datasets
         call m2c_iomemcom(pscalar,'PRES',np,1,
     &                           nb,igrid,ic,
     &                           umemcom,6,1,istat)

         call m2c_iomemcom(velo,'VELO',np,3,
     &                        nb,igrid,ic,
     &                        umemcom,6,1,istat)
         call m2c_iomemcom(coor,'COOR',3*np_coor,1,
     &                     nb,igrid2,0,
     &                     umemcom2,6,1,istat2)
c     calculate cell center coordinates
         call center3d(coor,coorc,n1,n2,n3)



c     obtain vol and s
        call metric3d(coor,vol,s,n1,n2,n3,0,nb,1,0,uerror,
     &                uprnt,istat) 
c     obtain state w vector
        call initwpg(w,p,work,n1,n2,n3,nnu,nb,igrid,ic,type,
     &               umemcom,uerror)     
c     calculate gradients
        call gradvelo(w,vol,s,du,dv,dw,n1,n2,n3,i2d,nsurf)
  
c     averaging in homogenious z-direction (velo, pressure)
            do j=1,n2
            do i=1,n1
              psum=0
               do k=1,n3
                 psum = psum + pscalar(i,j,k)
               enddo
              pscalar2(nset,i,j)=psum/(n3+1)              
            enddo
            enddo        
            do l=1,3
             do j=1,n2
             do i=1,n1
               velosum=0
                 do k=1,n3
                   velosum=velosum+velo(l,i,j,k)
                 enddo
               velo2(nset,l,i,j)=velosum/(n3+1)
             enddo
             enddo
            end do
c ...   fill 2D array with xy coordinates and block number
            do l=1,2
             do j=1,n2
             do i=1,n1
                   xy(nset,l,i,j)=coorc(l,i,j,1)
             enddo
             enddo
            enddo
c
            do j=1,n2
              u7(dbset,nset,j)=velo2(nset,1,nxc,j)
              v7(dbset,nset,j)=velo2(nset,2,nxc,j)
              w7(dbset,nset,j)=velo2(nset,3,nxc,j)
              dudy(dbset,nset,j)=DABS(du(nxc,j,1,2))
              dudy_sum(nset,j)=dudy_sum(nset,j) + dudy(dbset,nset,j)
              u7_sum(nset,j) = u7_sum(nset,j) + u7(dbset,nset,j)
              uu7_sum(nset,j) = uu7_sum(nset,j) + u7(dbset,nset,j)**2
              v7_sum(nset,j) = v7_sum(nset,j) + v7(dbset,nset,j)
              vv7_sum(nset,j) = vv7_sum(nset,j) + v7(dbset,nset,j)**2
              w7_sum(nset,j) = w7_sum(nset,j) + w7(dbset,nset,j)
              ww7_sum(nset,j) = ww7_sum(nset,j) + w7(dbset,nset,j)**2
              uv7_sum(nset,j) = uv7_sum(nset,j) + 
     &                    u7(dbset,nset,j)*v7(dbset,nset,j)
              uuvv7_sum(nset,j) = uuvv7_sum(nset,j) + 
     &                    (u7(dbset,nset,j)*v7(dbset,nset,j))**2
              yc(nset,j)=coorc(2,nxc,j,1)
            enddo
            if (nset.gt.11) then
              xc(nset)=xc(nset-11)
            else
              xc(nset)=coorc(1,nxc,1,1)
            end if
c
c ... write a test output file          
c            inquire(file=filename, exist=exists)
c            if (exists) then
c              open(unit=uout,file=filename,status='old',
c     &                        position='append')
c                do j=1,n2
c                do i=1,n1
c                  write(uout,form) xy(nset,1,i,j), xy(nset,2,i,j),
c     &                  pscalar2(nset,i,j), velo2(nset,1,i,j),
c     &                  velo2(nset,2,i,j), velo2(nset,3,i,j), nb 
c                enddo
c                enddo
c              close(uout)
c            else
c              open(unit=uout,file=filename,status='new')
c                write(uout,'(a)') header(1:ll1)
c                do j=1,n2
c                do i=1,n1
c                  write(uout,form) xy(nset,1,i,j), xy(nset,2,i,j),
c     &                  pscalar2(nset,i,j), velo2(nset,1,i,j),
c     &                  velo2(nset,2,i,j), velo2(nset,3,i,j), nb
c                enddo
c                enddo
c              close(uout)
c            end if
c
        deallocate(coor)
        deallocate(coorc)
        deallocate(pscalar)
        deallocate(velo)
        deallocate(velo2)
        deallocate(pscalar2)
        deallocate(xy)

        deallocate(s)
        deallocate(vol)
        deallocate(work)
        deallocate(w)
        deallocate(p)

        deallocate(du)
        deallocate(dv)
        deallocate(dw)   
c...  closing loop over blocks      
      end do
c ...

c...  closing loop over databases
      end do
      
c         print *, w7_sum(1,10)

         do nset=1,intset
           utau(nset)=sqrt(ABS(tauw(nset))/rho)
c           print *,utau(nset)
           do j=1,n2glob
             u7_av(nset,j)=u7_sum(nset,j)/dball
             v7_av(nset,j)=v7_sum(nset,j)/dball
             w7_av(nset,j)=w7_sum(nset,j)/dball
             dudy_av(nset,j) = dudy_sum(nset,j)/dball
             uu7_av(nset,j)=uu7_sum(nset,j)/dball
             vv7_av(nset,j)=vv7_sum(nset,j)/dball
             ww7_av(nset,j)=ww7_sum(nset,j)/dball
             u7rms(nset,j)=sqrt(uu7_av(nset,j)-u7_av(nset,j)**2)
             v7rms(nset,j)=sqrt(vv7_av(nset,j)-v7_av(nset,j)**2)
             w7rms(nset,j)=sqrt(ww7_av(nset,j)-w7_av(nset,j)**2)
             uv7_av(nset,j)=uv7_sum(nset,j)/dball
             uuvv7_av(nset,j)=uuvv7_sum(nset,j)/dball
             uv7rms(nset,j)=sqrt(uuvv7_av(nset,j)-uv7_av(nset,j)**2)
             urmsplus(nset,j)=u7rms(nset,j)/utau(nset)
             vrmsplus(nset,j)=v7rms(nset,j)/utau(nset)
             wrmsplus(nset,j)=w7rms(nset,j)/utau(nset)
             yplus(nset,j)=(yc(nset,j)-diam/2)*utau(nset)/nu
c             print *, yplus(nset,j)
           enddo
         enddo
c
c ... EXTRACT 20 points less from upper block
         do nset=1,intset           
           nb=blockset(nset)
           if (nset.eq.12) then
             n2glob=n2glob-20
           end if
           if (nset.gt.11) then
             nbunit=blockset(nset-11)
             filevelo7='velo7_pos'//trim(str(nset-11))
c     &                  trim(str(nbunit))
             open(unit=nbunit,status='old',file=filevelo7,
     &                  position='append')
              do j=1,n2glob
              write(nbunit,form) xc(nset),yc(nset,j)/diam-0.5,
     &                  u7_av(nset,j)/u0,v7_av(nset,j)/u0,
     &                  w7_av(nset,j)/u0,
     &                  u7rms(nset,j)/u0,v7rms(nset,j)/u0,
     &                  w7rms(nset,j)/u0,
     &                  uv7rms(nset,j)/(u0**2),dudy_av(nset,j)/u0,
     &                  uv7rms(nset,j)*dudy_av(nset,j)/(u0**3),
     &                  yplus(nset,j),urmsplus(nset,j),
     &                  vrmsplus(nset,j),wrmsplus(nset,j)
              enddo               
             close(nbunit)
           else
           filevelo7='velo7_pos'//trim(str(nset))
c           '_'//trim(str(nb))
             open(unit=nb,file=filevelo7,status='unknown')
c     &                        position='append')
            write(nb,'(a)') header(1:ll1)
            do j=1,n2glob
c              print *,'output',yplus(nset,j)
              write(nb,form) xc(nset),yc(nset,j)/diam-0.5,
     &                  u7_av(nset,j)/u0,v7_av(nset,j)/u0,
     &                  w7_av(nset,j)/u0,
     &                  u7rms(nset,j)/u0,v7rms(nset,j)/u0,
     &                  w7rms(nset,j)/u0,
     &                  uv7rms(nset,j)/(u0**2),dudy_av(nset,j)/u0,
     &                  uv7rms(nset,j)*dudy_av(nset,j)/(u0**3),
     &                  yplus(nset,j),urmsplus(nset,j),
     &                  vrmsplus(nset,j),wrmsplus(nset,j)
            enddo
             close(nb)
           end if
c
         end do
c
c
        deallocate(u7)
        deallocate(v7)
        deallocate(w7)
        deallocate(dudy)
       
        deallocate(u7_sum)
        deallocate(v7_sum)
        deallocate(w7_sum)
        deallocate(dudy_sum)

        deallocate(uu7_sum)
        deallocate(vv7_sum)
        deallocate(ww7_sum)
        deallocate(uv7_sum)
        deallocate(uuvv7_sum)
c
        deallocate(u7_av)
        deallocate(v7_av)
        deallocate(w7_av)
        deallocate(dudy_av)

        deallocate(uu7_av)
        deallocate(vv7_av)
        deallocate(ww7_av)
        deallocate(uv7_av)
        deallocate(uuvv7_av)
        deallocate(u7rms)
        deallocate(v7rms)
        deallocate(w7rms)
        deallocate(uv7rms)
        deallocate(xc)
        deallocate(yc)
        deallocate(utau)
        deallocate(tauw)
        deallocate(yplus)
        deallocate(urmsplus)
        deallocate(vrmsplus)
        deallocate(wrmsplus)
c
      call endadb
c
      contains
c
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

      end program

      subroutine m2c_iomemcom(a,name,np,ndim,nb,igrid,icycle,
     &                        umemcom,uerror,iact,istat)
c
c     This routine is used to do I/0 on the memcom data base
c
c     Creation date: 18 - apr - 1996
c
c     Author:        J.B. Vos/CFS Engineering
c
c     Modification history:
c
c     date           programmer      description
c
c     Input:
c
c     a      = for iact = 2: array to write to MEMCOM
c     iact   = 1: read from MEMCOM
c              2: write to MEMCOM
c     icycle = grid number
c     igrid  = grid number
c     nb     = block number
c     np     = number of points 
c     ndim   = number of dimentions in wall distance array
c     uerror = error output unit number
c     umemcom= MEMCOM output unit number
c
c     Output:
c
c     a      = for iact = 1: array to read from MEMCOM
c     istat  = 0 everything went ok
c
      implicit none
c
      integer np,ndim,nb,icycle,igrid,umemcom,uerror,istat,nw,iact
c
      character name*(*),label*30,type*4
c
      real*8 a(1:np,1:ndim)
c
c     local variables
c
      integer iex,iset,iadr
c 
      istat = 0
c
      call makelabl(name,nb,igrid,icycle,label,istat)
c
c     read dataset
c
      if (iact .eq. 1) then
c
c     check if array is stored in MEM-COM
c
         call inqsdb(umemcom,label,0,iex,istat)
c
         if (iex .eq. 1) then
c
c     read attributes of the data set
c
            call prpdb(umemcom,label,0,iset,type,nw,iadr)
c
            if (np*ndim .ne. nw) then
               write (uerror,'(3a,//,2(a,i4,/))') 
     *            ' *** FATAL ERROR in m2c_iomemdb for dataset ',
     *              label,' ***',
     *            ' array size declared    = ',np*ndim, 
     *            ' size found on data set = ',nw
               goto 105
            end if
c
c     read results from MEMCOM
c      
            call getdb(umemcom,label,0,a,type,nw,istat)
         else
            istat = -1
         end if
c
c     write dataset
c
      else if (iact .eq. 2) then
c
c     write results to dataset
c
         type = 'F'
         call dssndb(umemcom,label,np,ndim,type,0,istat)
         call putdb(umemcom,label,0,a,type,np*ndim,istat)
c
         if (istat .ne. 0) then
            write (uerror,'(3a)')
     *         ' *** FATAL error in m2c_iomemdb when writing ',
     *         label,' to MEM-COM'
            goto 105
         end if
      else
         write (uerror,'(a,i4,a)')
     *   ' *** FATAL error in m2c_iomemdb, iact=',iact,' not defined'
         goto 105
      end if
c
      return

  105 continue
      call endadb
      stop
      end
      subroutine initwpg(w,p,work,n1,n2,n3,nnu,ib,igrid,ic,type,
     *                   umemcom,uerror)
c
c     This subroutine is developed to initialize the state vector w 
c     for a caloric perfect gas calculation.
c
c     Creation date: 04 - sep - 1992
c
c     Author:        J.B. Vos/IMHEF
c
c     Modification history:
c
c     date           programmer         description
c
c     Input
c
c     ib      = block number
c     ic      = cycle number
c     igrid   = grid number
c     n1      = number of grid cells in I-direction      
c     n2      = number of grid cells in J-direction      
c     n3      = number of grid cells in K-direction      
c     nnu     = number of unknowns
c     type    = 'F' for float (nsmb4.0)
c     uerror  = error unit number
c     umemcom = memcom unit number
c     work    = work vector
c
c     Output
c
c     w       = state vector
c      
      implicit none
c
      integer ib,ic,igrid,n1,n2,n3,nnu,uerror,umemcom,n,np,istat
c
      real*8 w((n1+2)*(n2+2)*(n3+2),nnu),
     *       p((n1+2)*(n2+2)*(n3+2)),
     *       work((n1+2)*(n2+2)*(n3+2),1:3)
c
      real(kind=8), parameter :: half=0.5,one=1.0
      real*8 gamma0,vtot,ogm1,c,rgas,rmach,p0,dynpr
      character name*30,label*30,type*4
      logical mach,temp
c
c ... get gamma0 and rgas from the CDES table
c
      call makelabl('CDES',0,igrid,ic,label,istat)
      call gettab(umemcom,label,0,istat)
      call getbwf('RGAS',rgas,istat)
      call getbwf('GAMMA0',gamma0,istat)
      call getbwf('MACH',rmach,istat)
      call getbwf('P0',p0,istat)
      dynpr  = 0.5 * gamma0 * rmach * rmach * p0
c
      np   = (n1+2)*(n2+2)*(n3+2)
c
c     to initialize the state vector for a caloric perfect gas, it
c     is assumed velocities and pressure are stored in the database. The
c     missing variable is or the density or the Mach number or the
c     temperature
c
c     read pressure
c
      name = 'PRES'
      call readmem(p,work,name,n1,n2,n3,1,ib,igrid,ic,type,umemcom,
     *             uerror,istat)
c
c     if istat .ne. 0, then CP is stored
c
      if (istat .ne. 0) then
         name = 'CP'
         call readmem(p,work,name,n1,n2,n3,1,ib,igrid,ic,type,
     *                umemcom,uerror,istat)
c
         if (istat .ne. 0) goto 305
c
c     calculate pressure from Cp
c
         p = p0 + p * dynpr
      end if
c
      name = 'VELO'
      call readmem(w(1,2),work,name,n1,n2,n3,3,ib,igrid,ic,type,
     *             umemcom,uerror,istat)
c
      if (istat .ne. 0) goto 305
c
      mach = .false.
      temp = .false.
c
c     check if density is stored
c
      name = 'DENS'
      call readmem(w(1,1),work,name,n1,n2,n3,1,ib,igrid,ic,type,
     *             umemcom,uerror,istat)
c
      if (istat .ne. 0) then
c
c     check if TEMP is stored, and if this is the case, the density is
c     calculated from the temperature and pressure. The temperature
c     is stored in w(.,1) and used to calculate the density
c
         name = 'TEMP'
         call readmem(w(1,1),work,name,n1,n2,n3,1,ib,igrid,ic,type,
     *                umemcom,uerror,istat)
c
c     calculate density
c
         if (istat .eq. 0) then
            temp = .true.
         else 
c
c     check if MACH number is stored, if yes, the result is stored in
c     w(n,1), and used to calculate the density
c
            mach = .true.
            name = 'MACH'
            call readmem(w(1,1),work,name,n1,n2,n3,1,ib,igrid,ic,
     *                   type,umemcom,uerror,istat)
c
c     if istat = -1 then neither MACH, DENS and TEMP are stored
c
            if (istat .ne. 0) then
               name = 'MACH, TEMP or DENS'
               goto 305
            end if
         end if
      end if
c
      if (mach) then
         do 103 n=1,np
            vtot   = w(n,2)*w(n,2) + w(n,3)*w(n,3) + w(n,4)*w(n,4)
            c      = sqrt(vtot) / w(n,1)
            w(n,1) = gamma0 * p(n) / (c * c)
  103    continue
      end if
c
c     calculate density from pressure and temperature
c
      if (temp) then
         do 203 n=1,np
            w(n,1) = p(n) / (rgas * w(n,1))
  203    continue
      end if
c
c     calculate velocities times density and the total energy from the
c     pressure
c
      ogm1 = one / (gamma0 - one)
      do 303 n=1,np
         w(n,2) = w(n,1)*w(n,2)
         w(n,3) = w(n,1)*w(n,3) 
         w(n,4) = w(n,1)*w(n,4)
         w(n,5) = p(n) * ogm1 + half*(w(n,2)**2 + w(n,3)**2 + 
     &                                w(n,4)**2) / w(n,1)
  303 continue
c
      return
c
  305 continue
c
c     write error messages and stop program
c
      call makelabl(name,ib,igrid,ic,label,istat)
c
      write (uerror,'(//,3a)') 
     *    ' *** FATAL in INITWPPG *** ',label,' data set not found'
c
      call endadb
c
      stop
      end
      subroutine readmem(a,work,name,n1,n2,n3,ndim,ib,igrid,ic,type,
     *                    umemcom,uerror,istat)
c
c     This routine is developed to read an array a from MEMCOM.
c
c     Creation date: 04 - sep - 1992
c
c     Author:        J.B. Vos/IMHEF
c
c     Modification history:
c
c     date           programmer      description
c
c
c     Input:
c
c     ib      = block number
c     ic      = cycle number
c     igrid   = grid number
c     work    = work array
c     a       = array containing stored results from MEMCOM.
c
      implicit none
c
      integer n1,n2,n3,i,j,k,l,ib,ic,iex,istat,np,umemcom,ndim,uerror,
     *        iset,nw,igrid
#ifdef MC7
      integer(kind=8) :: iadr
#else
      integer(kind=4) :: iadr
#endif
c
      real*8 a   (0:n1+1,0:n2+1,0:n3+1,1:ndim),
     *       work(1:ndim,0:n1+1,0:n2+1,0:n3+1)
c
      character typedsn*4,type*4,label*30,name*(*)
c
      external makelabl,getdb,inqsdb,prpdb,prpdb64,endadb
c
c     read results from MEMCOM
c
      np = (n1+2)*(n2+2)*(n3+2)
c
      call makelabl(name,ib,igrid,ic,label,istat)
c
c     check if dataset exists
c
      call inqsdb(umemcom,label,np,iex,istat)
c
c     print error message if data set doesnot exist, and return to
c     calling program
c
      if (iex .eq. 0) then
         write (uerror,'(3a)')
     *    ' *** WARNING in READMEM *** ',label,' data set not found'
         istat = -1
         return
      end if
c
c     read attributes of the data set
c
#ifdef MC7
      call prpdb64(umemcom,label,0,iset,typedsn,nw,iadr)
#else
      call prpdb(umemcom,label,0,iset,typedsn,nw,iadr)
#endif
c
      if (type(1:1) .ne. typedsn(1:1)) then
         write (uerror,'(3a,/,6a)')
     *       ' *** FATAL ERROR in READMEM for dataset ',label,
     *       ' *** mismatch in data set types',
     *       ' type for ',label,' equals ',typedsn,
     *       ' type in input equals ',type
              call endadb
         stop
      end if
c
      if (np*ndim .ne. nw) then
         write (uerror,'(3a,//,2(a,i4,/))')
     *    ' *** FATAL ERROR in READMEM for dataset ',label,' ***',
     *    ' array size declared    = ',np*ndim,
     *    ' size found on data set = ',nw
         call endadb
         stop
      end if
c
c     read results from MEMCOM
c
      call getdb(umemcom,label,0,work,type,nw,istat)
c
c     copy results from array work into array a
c
      do 103 l=1,ndim
         do 203 k=0,n3+1
            do 303 j=0,n2+1
               do 403 i=0,n1+1
                  a(i,j,k,l) = work(l,i,j,k)
  403          continue
  303       continue
  203    continue
  103 continue
c
      return
      end
