      program tauw_x
      implicit none
      integer uerror,umemcom,umemcom2,uout,nset,ii,j_fix
      integer con,dbset,np_cf
      integer nblock,n1,n2,n3,iprec,istat,nb,igrid,ic,np,istat2
      integer i,j,k,l,iex,ll1,ll2,n1def,n2def,igrid2,np_coor
      character *30 label,cbuf
      character *60 dbname,dbname2
      character type*4
      character(len=100):: header
      character(len=14) :: form
      character(len=80) :: filename,command
      character(len=10) :: i2char
      logical :: exists
      real(kind=8), dimension(:,:,:,:), allocatable :: tauw
      real(kind=8), dimension(:,:,:)  , allocatable :: pscalar
      real(kind=8), dimension(:,:,:,:), allocatable :: coor,xy,coorc
      real(kind=8), dimension(:,:), allocatable :: tauw_post
      real(kind=8), dimension(:), allocatable :: x_post
      real(kind=8) u0
c
      integer, parameter :: intset=11,ii_max=500,bset=1
      integer blockset(intset)
      character*12 baseset(bset)
c
c
      u0=69.2059
c
       data blockset /120,156,160,194,198,220,224,238,242,248,246/
c       data baseset /'convmesh.memcom.db_all'/
         header = '#    X            CFX            CFY'//
     &   '            CFZ'
         ll1 = len_trim(header)
c       ll1 = 70 
c       print *, ll1
c      form = '('//i2char(nres)
c      ll2  = len_trim(form)
c      form = form(1:ll2)//'(1x,g14.7))'
       form = '(4(1x,g20.7))'
c
c
c
      umemcom = 2
      umemcom2 = 1
      uerror = 6	
      uout = 10
c      command = 'rm '//trim(filename)
      type     = 'F'
      dbname2 = 'convmesh.memcom.db'
      igrid2 = 0
c      dbname = 'time_0.77.db'
      igrid = 0
c      ic = 11
c
      call filedb(dbname2,istat2)
      !print *, istat
      if (istat2 .lt. 0) then
        print '(a,$)','2nd Data base does not exist '
        stop
      endif
c
      call initmc(1,6,6)
c      
      call opdidb(umemcom2,dbname2,1,' ',istat2)
c
c ... read ADIR descriptor
c
      call getdes(umemcom2,'ADIR',0,istat2)
      call getbwi('NBR',nblock,istat2)
      call gprmdb('NWFLOAT',iprec,istat2)
c      CALL system(command)
c      inquire(file=filename, exist=exists)
c      print *, exists
      j_fix=1
c

c      do dbset=1,bset
       dbset=1
        allocate(tauw_post(3,ii_max))
        allocate(x_post(ii_max))
        tauw_post = 0
        x_post = 0
        ii=0
        con=0
c        dbname=baseset(dbset)
        dbname='convmesh.memcom.db_all'
        filename='myextract_tauw_x'
c
      call filedb(dbname,istat)
      !print *, istat
      if (istat .lt. 0) then
        print '(a,$)','Data base does not exist '
        stop
      endif
c
      call opdidb(umemcom+dbset,dbname,0,' ',istat)
c
c ... find the cycle number of the base
      do i=1,20
        call makelabl('CDES',0,igrid,i,cbuf,istat)
        call inqsdb(umemcom+dbset,cbuf,0,iex,istat) 
        if (iex == 1) then
          ic=i
          EXIT
        end if
      enddo
       print *,ic
c
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
c
         np_coor = (n1+1)*(n2+1)*(n3+1)
	 np = (n1+2)*(n2+2)*(n3+2)
         np_cf=(n1+2)*(n3+2)
c   
         allocate(pscalar(0:n1+1,0:n2+1,0:n3+1))
         allocate(tauw(3,0:n1+1,0:n2+1,0:n3+1))
         allocate(coor(3,n1+1,n2+1,n3+1))
         allocate(coorc(3,0:n1+1,0:n2+1,0:n3+1))
c         allocate(pscalar2(intset,0:n1+1,0:n2+1))
c         allocate(velo2(intset,3,0:n1+1,0:n2+1))
         allocate(xy(intset,2,0:n1+1,0:n2+1))
c
c         do l=1,noset
         call m2c_iomemcom(pscalar,'PRES',np,1,
     &                           nb,igrid,ic,
     &                           umemcom+dbset,6,1,istat)

         call m2c_iomemcom(tauw,'CF',np_cf,3,
     &                        nb,igrid,ic,
     &                        umemcom+dbset,6,1,istat)
         call m2c_iomemcom(coor,'COOR',3*np_coor,1,
     &                     nb,igrid2,0,
     &                     umemcom2,6,1,istat2)
         call center3d(coor,coorc,n1,n2,n3)
c ... averaging in homogenious z-direction (do not neet it here?)
c            do j=1,n2
c            do i=1,n1
c              psum=0
c               do k=1,n3
c                 psum = psum + pscalar(i,j,k)
c               enddo
c              pscalar2(nset,i,j)=psum/(n3+1)
c            enddo
c            enddo        
c            do l=1,3
c             do j=1,n2
c             do i=1,n1
c               velosum=0
c                 do k=1,n3
c                   velosum=velosum+velo(l,i,j,k)
c                 enddo
c               velo2(nset,l,i,j)=velosum/(n3+1)
c             enddo
c             enddo
c           end do
            do l=1,2
             do j=1,n2
             do i=1,n1
                   xy(nset,l,i,j)=coorc(l,i,j,1)
             enddo
             enddo
            enddo
        do l=1,3
          con=0
        do i=n1,1,-1 
          con=n1+1-i          
          tauw_post(l,con+ii)=tauw(l,i,j_fix,1)
          x_post(con+ii)=coorc(1,i,j_fix,1)          
        enddo
        enddo
        ii=ii+n1        
        deallocate(coor)
        deallocate(coorc)
        deallocate(pscalar)
        deallocate(tauw)
c        deallocate(velo2)
c        deallocate(pscalar2)
        deallocate(xy)   
c      
      end do
       print *,ii
c
              open(unit=uout+dbset,file=filename,status='unknown')
                write(uout+dbset,'(a)') header(1:ll1)
                do i=1,ii
                  write(uout+dbset,form) x_post(i),tauw_post(1,i),
     &                     tauw_post(2,i),tauw_post(3,i)
                enddo
              close(uout+dbset)
c
       deallocate(tauw_post)
       deallocate(x_post)
c
c      end do
c
      call endadb
c
      end 

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

