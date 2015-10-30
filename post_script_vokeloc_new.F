      program post_script
      implicit none
      integer uerror,umemcom,umemcom2,uout,nset,nnu
      integer nxc,tcalc,n2glob,dbset,uprnt
      integer nblock,n1,n2,n3,iprec,istat,nb,igrid,ic,np,istat2
      integer i,j,k,l,iex,ll1,ll2,n1def,n2def,igrid2,np_coor
      character *30 label,cbuf
      character *60 dbname,dbname2
      character type*4
      character(len=320):: header
      character(len=20) :: form_out
      character(len=80) :: filename,command,filevelo7
      character(len=10) :: i2char
      logical :: exists
      integer i2d, nsurf
      real(kind=8), dimension(:,:,:,:),allocatable :: velo_1a,velo_1b
      real(kind=8), dimension(:,:,:,:),allocatable :: velo_2a,velo_2b
c
      real(kind=8), dimension(:,:,:),allocatable :: velos,velos2
      real(kind=8),dimension(:,:,:),allocatable :: velo_hom,velo_hom2
      real(kind=8), dimension(:,:,:),allocatable :: velorms
      real(kind=8), dimension(:,:),allocatable :: uv,uv_hom,uvrms
c
      real(kind=8), dimension(:,:,:),allocatable :: velos_2,velos2_2
      real(kind=8),dimension(:,:,:),allocatable :: velohom_2,velohom2_2
      real(kind=8), dimension(:,:,:),allocatable :: velorms_2
      real(kind=8), dimension(:,:),allocatable :: uv_2,uvhom_2,uvrms_2
c
      real(kind=8), dimension(:,:,:,:),allocatable :: coor_1a
      real(kind=8), dimension(:,:,:,:),allocatable :: coorc_1a
      real(kind=8), dimension(:,:,:,:),allocatable :: coor_2a
      real(kind=8), dimension(:,:,:,:),allocatable :: coorc_2a
c
      real(kind=8), dimension(:), allocatable :: yc,yc_up
      real(kind=8), dimension(:,:), allocatable :: xc_1,xc_2      
c
c
      real(kind=8), parameter :: zero = 0.0,half=0.5,one=1.0
      real*8 wt,w_old,w_new
      integer nb_1a,nb_1b,n1_1a,n2_1a,n3_1a,n1_1b,n2_1b,n3_1b
      integer np_1a,np_1b,np_coor_1a,n1glob,n1max,n3tot
      integer nb_2a,nb_2b,n1_2a,n2_2a,n3_2a,n1_2b,n2_2b,n3_2b
      integer np_2a,np_2b,np_coor_2a
      integer n2_up
      integer vset,va,n3glob,n3b,cter
c
c
      real(kind=8) velosum,psum,u0,diam
      real(kind=8) rho,nu
c
      integer, parameter :: intset=5,nres=4,dball=226,voke=7
      integer, parameter :: n2up_cut = 6
      character baseset(dball)*24
      real timeset(dball)
      real xc(voke)
      integer blockset1a(intset)
      integer blockset1b(intset)
      integer blockset2a(intset)
      integer blockset2b(intset)
      integer nxcvoke(voke)
      integer nbvoke(voke)
      integer nbunit(voke)
      integer iglobe(voke)
c      integer nz(intset)
c
c
      u0=69.2059
      diam=0.124465232
      timeset = 0
      tcalc = 0
      rho=1.1847274513
      nu=0.0029579478/rho
c
       data blockset1a /120,156,160,194,198/
       data blockset1b /119,155,159,193,197/
       data blockset2a /118,122,158,162,196/
       data blockset2b /117,121,157,161,195/       
       data nbvoke /120,120,156,160,160,194,198/
       data nxcvoke /13,3,20,27,18,25,30/
       data nbunit /100,101,102,103,104,105,106/
c     &               13,4,20,27,19,25,30/
c       data baseset /120,156,160,194,198,220,224,238,242,248,246/
         header = '#    X/D            Y/D            U_av/U0'//
     &   '            V_av/U0            W_av/U0            Urms/U0'//
     &   '            Vrms/U0            Wrms/U0            UVrms/U0^2'
       ll1 = len_trim(header)
c       print *, ll1
       form_out = '(9(1x,g14.7))'
c      
      do i=1,dball
c        tcalc=44+i+(i-1)
        tcalc=2*(i-1)+360
c        timeset(i)=tcalc/100.0
c        print *,tcalc,timeset(i)
        baseset(i)='time_1.'//trim(str(tcalc))//'.db'
        print *,baseset(i)
      enddo
c      stop
c

      i2d = 0
      n1max = 0
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
c      n1max = 5*n1glob

      do nset=1,intset
         call getdim(blockset1a(nset),umemcom2,n1,n2,n3)
!         print *, 'n1', n1 
         n1max = n1max + n1         
      enddo
      call getdim(blockset1b(1),umemcom2,n1,n2,n3b)
         n2glob = n2
         n3glob = n3 + n3b
      call getdim(blockset2a(1),umemcom2,n1,n2,n3)
         n2_up = n2
c         n3glob = n3 + n3b

      print *, 'n1max, n2glob, n3glob',n1max, n2glob, n3glob 
      print *, 'n2_up',n2_up
  

c
c... allocating global arrays
      allocate(yc(n2glob))
      allocate(yc_up(n2_up))
      allocate(xc_1(voke,n2glob))
      allocate(xc_2(voke,n2_up))
c      allocate(xy(2,n1max,n2glob))
      allocate(velos(3,n1max,n2glob))
      allocate(velos2(3,n1max,n2glob))
      allocate(uv(n1max,n2glob))
c
      allocate(velos_2(3,n1max,n2_up))
      allocate(velos2_2(3,n1max,n2_up))
      allocate(uv_2(n1max,n2_up))
c
      allocate(velo_hom(3,n1max,n2glob))
      allocate(velo_hom2(3,n1max,n2glob))
      allocate(velorms(3,n1max,n2glob))
      allocate(uv_hom(n1max,n2glob))
      allocate(uvrms(n1max,n2glob))
c
      allocate(velohom_2(3,n1max,n2_up))
      allocate(velohom2_2(3,n1max,n2_up))
      allocate(velorms_2(3,n1max,n2_up))
      allocate(uvhom_2(n1max,n2_up))
      allocate(uvrms_2(n1max,n2_up))
c
c      xy = 0
      iglobe = 0
      velos = 0
      velos2 = 0
      uv = 0
      velos_2 = 0
      velos2_2 = 0
      uv_2 = 0
c      nz = 0
      velo_hom = 0
      velo_hom2 = 0
      velorms = 0
      uv_hom = 0
      uvrms = 0
      velohom_2 = 0
      velohom2_2 = 0
      velorms_2 = 0
      uvhom_2 = 0
      uvrms_2 = 0
c      print *,'n2glob= ',n2glob
c      stop
c
c

c
c... loop over the solution databases
      do dbset=1,dball       
c      call endadb
c      call initmc(1,6,6)
c      call opdidb(umemcom2,dbname2,1,' ',istat2)
c      
c
        w_new = one / real(dbset)
        w_old = one - w_new

      if (dbset.le.99) then 
        umemcom=1+dbset
      else 
        if (dbset.le.198) then       
        umemcom=1+(dbset-99)
      else
        if (dbset.le.297) then       
        umemcom=1+(dbset-198)
      endif
      endif
      endif
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
      do i=20,40
c        print *,i
        call makelabl('CDES',0,igrid,i,cbuf,istat)
        call inqsdb(umemcom,cbuf,0,iex,istat) 
        if (iex == 1) then
          ic=i
          EXIT
        end if
      enddo
c      print *,ic
c
       cter = 0     
c
       velos = 0
       velos2 = 0
       uv = 0
c
       velos_2 = 0
       velos2_2 = 0
       uv_2 = 0

c ... looping over the set of blocks
      do nset=1,intset
c
c ... read MDES to obtain grid size
         nb_1a=blockset1a(nset)
         nb_1b=blockset1b(nset)
c
         nb_2a=blockset2a(nset)
         nb_2b=blockset2b(nset)
c
c ... find n1,n2,n3 for each and extract the var's
         call getdim(nb_1a,umemcom2,n1_1a,n2_1a,n3_1a)
         call getdim(nb_1b,umemcom2,n1_1b,n2_1b,n3_1b)
c find for upper blocks too
         call getdim(nb_2a,umemcom2,n1_2a,n2_2a,n3_2a)
         call getdim(nb_2b,umemcom2,n1_2b,n2_2b,n3_2b)
c
         np_coor_1a = (n1_1a+1)*(n2_1a+1)*(n3_1a+1)
	 np_1a = (n1_1a+2)*(n2_1a+2)*(n3_1a+2) 
	 np_1b = (n1_1b+2)*(n2_1b+2)*(n3_1b+2)
c for upper blocks too
         np_coor_2a = (n1_2a+1)*(n2_2a+1)*(n3_2a+1)
	 np_2a = (n1_2a+2)*(n2_2a+2)*(n3_2a+2) 
	 np_2b = (n1_2b+2)*(n2_2b+2)*(n3_2b+2)
c         np_coor_1b = (n1_1b+1)*(n2_1b+1)*(n3_1b+1)

c         n1=n1_1a
c         n2=n2_1a
c         n3=n3_1a
c         nz(nset) = n3_1a + n3_1b
c
         allocate(velo_1a(3,0:n1_1a+1,0:n2_1a+1,0:n3_1a+1))
         allocate(coor_1a(3,n1_1a+1,n2_1a+1,n3_1a+1))
         allocate(coorc_1a(3,0:n1_1a+1,0:n2_1a+1,0:n3_1a+1))
         allocate(velo_1b(3,0:n1_1b+1,0:n2_1b+1,0:n3_1b+1))
c  and for upper blocks
         allocate(velo_2a(3,0:n1_1a+1,0:n2_1a+1,0:n3_1a+1))
         allocate(coor_2a(3,n1_1a+1,n2_1a+1,n3_1a+1))
         allocate(coorc_2a(3,0:n1_1a+1,0:n2_1a+1,0:n3_1a+1))
         allocate(velo_2b(3,0:n1_1b+1,0:n2_1b+1,0:n3_1b+1))
c
         call m2c_iomemcom(velo_1a,'VELO',np_1a,3,
     &                        nb_1a,igrid,ic,
     &                        umemcom,6,1,istat)
         call m2c_iomemcom(coor_1a,'COOR',3*np_coor_1a,1,
     &                     nb_1a,igrid2,0,
     &                     umemcom2,6,1,istat2)
c     calculate cell center coordinates
         call center3d(coor_1a,coorc_1a,n1_1a,n2_1a,n3_1a)
c
         call m2c_iomemcom(velo_1b,'VELO',np_1b,3,
     &                        nb_1b,igrid,ic,
     &                        umemcom,6,1,istat)
c
c
c
c ...        for upper blocks
         call m2c_iomemcom(velo_2a,'VELO',np_2a,3,
     &                        nb_2a,igrid,ic,
     &                        umemcom,6,1,istat)
         call m2c_iomemcom(coor_2a,'COOR',3*np_coor_2a,1,
     &                     nb_2a,igrid2,0,
     &                     umemcom2,6,1,istat2)
c     calculate cell center coordinates
         call center3d(coor_2a,coorc_2a,n1_2a,n2_2a,n3_2a)
c
         call m2c_iomemcom(velo_2b,'VELO',np_2b,3,
     &                        nb_2b,igrid,ic,
     &                        umemcom,6,1,istat)



c calculate the sum's for averaging in homogenious z-direction (velo, uv)
      call sumoverz(n1_1a,n2_1a,n3_1a,n1max,n2glob,cter,velo_1a,
     &           velos,velos2,uv)
      call sumoverz(n1_1b,n2_1b,n3_1b,n1max,n2glob,cter,velo_1b,
     &           velos,velos2,uv)
c for upper blocks
      call sumoverz(n1_2a,n2_2a,n3_2a,n1max,n2_up,cter,velo_2a,
     &           velos_2,velos2_2,uv_2)
      call sumoverz(n1_2b,n2_2b,n3_2b,n1max,n2_up,cter,velo_2b,
     &           velos_2,velos2_2,uv_2)
  
        do vset=1,voke
          if (nb_1a.eq.nbvoke(vset)) then
            xc(vset) = coorc_1a(1,nxcvoke(vset),1,1)            
            iglobe(vset) = nxcvoke(vset)+cter
            do j=1,n2_1a
              xc_1(vset,j) = coorc_1a(1,nxcvoke(vset),j,1)
            enddo
            do j=1,n2_2a
              xc_2(vset,j) = coorc_2a(1,nxcvoke(vset),j,1)
            enddo
          endif          
        enddo            

        if (nset.eq.1) then
          do j=1,n2_1a
            yc(j) = coorc_1a(2,1,j,1)
          enddo
          do j=1,n2_2a
            yc_up(j) = coorc_2a(2,1,j,1)
          enddo      
        endif

c ...   fill 2D array with xy coordinates and block number
c            do l=1,2
c             do j=1,n2
c             do i=1,n1_1a
c                   xy(l,i+cter,j)=coorc_1a(l,i,j,1)
c             enddo
c             enddo
c            enddo

c          i = nxcvoke(vset)
c          xc(vset)=xy(nset,1,i,1)        


        deallocate(coor_1a,coor_2a)
        deallocate(coorc_1a,coorc_2a)
c        deallocate(pscalar)
        deallocate(velo_1a,velo_2a)
        deallocate(velo_1b,velo_2b)
c        deallocate(pscalar2)
c        deallocate(xy)
c...  closing loop over blocks     

         cter = cter + n1_1a
 
      end do 
c ...
c      n3tot = nz(1)
c
      do j=1,n2glob
        do i=1,n1max
            do l=1,3
               velo_hom(l,i,j) = w_old*velo_hom(l,i,j) + 
     &                w_new*(velos(l,i,j)/n3glob)
               velo_hom2(l,i,j) = w_old*velo_hom2(l,i,j) +
     &                w_new*(velos2(l,i,j)/n3glob)
              velorms(l,i,j) = sqrt(velo_hom2(l,i,j) - 
     &                    velo_hom(l,i,j)**2)
            enddo
               uv_hom(i,j) = w_old*uv_hom(i,j) + 
     &                w_new*(uv(i,j)/n3glob)
              uvrms(i,j) = uv_hom(i,j) - 
     &    velo_hom(1,i,j)*velo_hom(2,i,j)
        enddo
      enddo
c
c... for upper blocks
      do j=1,n2_up
        do i=1,n1max
            do l=1,3
               velohom_2(l,i,j) = w_old*velohom_2(l,i,j) + 
     &                w_new*(velos_2(l,i,j)/n3glob)
               velohom2_2(l,i,j) = w_old*velohom2_2(l,i,j) +
     &                w_new*(velos2_2(l,i,j)/n3glob)
              velorms_2(l,i,j) = sqrt(velohom2_2(l,i,j) - 
     &                    velohom_2(l,i,j)**2)
            enddo
               uvhom_2(i,j) = w_old*uvhom_2(i,j) + 
     &                w_new*(uv_2(i,j)/n3glob)
              uvrms_2(i,j) = uvhom_2(i,j) - 
     &    velohom_2(1,i,j)*velohom_2(2,i,j)
        enddo
      enddo
c
c
c
         call enddb(umemcom,istat)
c...  closing loop over databases
      end do
c      stop

      i=0

c      print *, 'n2glob', n2glob
c ... writing out 7 voke's positions to 7 files
      do vset=1,voke
c        nb=nbvoke(vset)
c        do va=1,intset
c          if (nb.eq.blockset1a(va)) then
c            nset=va
c          endif
c        enddo
         i = iglobe(vset)
c        xc=xy(nset,1,i,1)        
c
        filevelo7='velo7_pos'//trim(str(vset))
c
        open(unit=nbunit(vset),file=filevelo7,status='replace',
     &                  position='append')        
          write(nbunit(vset),'(a)') header(1:ll1)
c
c       stop
        do j=1,n2glob        
c           yc(j)=xy(nset,2,i,j)
         write(nbunit(vset),form_out) xc_1(vset,j)/diam,yc(j)/diam-0.5,
     &        velo_hom(1,i,j)/u0,velo_hom(2,i,j)/u0,
     &                  velo_hom(3,i,j)/u0,
     &        velorms(1,i,j)/u0,velorms(2,i,j)/u0,
     &                  velorms(3,i,j)/u0,
     &        uvrms(i,j)/(u0**2)
c     &                  yplus(nset,j),urmsplus(nset,j),
c     &                  vrmsplus(nset,j),wrmsplus(nset,j)
        enddo
c
        do j=1,n2up_cut
         write(nbunit(vset),form_out) xc_2(vset,j)/diam,
     &     yc_up(j)/diam-0.5,velohom_2(1,i,j)/u0,velohom_2(2,i,j)/u0,
     &                  velohom_2(3,i,j)/u0,
     &        velorms_2(1,i,j)/u0,velorms_2(2,i,j)/u0,
     &                  velorms_2(3,i,j)/u0,
     &        uvrms_2(i,j)/(u0**2)
        enddo  
c      
       close(nbunit(vset))

      enddo
c
      deallocate(yc,velos,velos2,uv,yc_up)
      deallocate(velos_2,velos2_2,uv_2)
      deallocate(velo_hom,velo_hom2,uv_hom)
      deallocate(velohom_2,velohom2_2,uvhom_2)
      deallocate(velorms,uvrms)
      deallocate(velorms_2,uvrms_2,xc_1,xc_2)
c
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


      subroutine sumoverz(n1,n2,n3,n1max,n2glob,cter,velo,
     &           velos,velos2,uv)
      implicit none
       integer, intent(in) :: n1,n2,n3,cter,n2glob,n1max
       real*8, intent(in) :: velo(3,0:n1+1,0:n2+1,0:n3+1)
       real*8, intent(inout) :: velos(3,n1max,n2glob)
       real*8, intent(inout) :: velos2(3,n1max,n2glob)
       real*8, intent(inout) :: uv(n1max,n2glob)
c
       integer i,j,k,l
c
       do j=1,n2
        do i=1,n1
          do k=1,n3
           do l=1,3
              velos(l,i+cter,j)=velos(l,i+cter,j)+velo(l,i,j,k)
              velos2(l,i+cter,j)=velos2(l,i+cter,j)+velo(l,i,j,k)**2
            enddo
            uv(i+cter,j)=uv(i+cter,j)+velo(1,i,j,k)*velo(2,i,j,k)
          enddo
        enddo
        end do
c       
      end

      subroutine getdim(nblock,umemcom,n1,n2,n3)
      implicit none
        integer, intent(in) :: umemcom,nblock
        integer, intent(out) :: n1,n2,n3
        integer igrid,istat
        character *30 cbuf
        igrid = 0
        istat = 0      
        call makelabl('MDES',nblock,igrid,0,cbuf,istat)
        call gettab(umemcom,cbuf,0,istat)
        call getbwi('N1',n1,istat)
        call getbwi('N2',n2,istat)
        call getbwi('N3',n3,istat)
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

      subroutine finddbcycle(umemcom,ic)
c
c
        integer, intent(in) :: umemcom
        integer, intent(out) :: ic
c    local
        integer i,igrid,istat,iex
        character *30 cbuf
        igrid = 0
        istat = 0
        do i=1,40
          call makelabl('CDES',0,igrid,i,cbuf,istat)
          call inqsdb(umemcom,cbuf,0,iex,istat) 
          if (iex == 1) then
            ic=i
            EXIT
          end if
        enddo
      end
