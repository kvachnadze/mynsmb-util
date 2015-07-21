      program ensemble_channel
        implicit none
        character *60 dbname,dbname2,list
        character *30 label,cbuf,form,form2
        integer uerror,umemcom,umemcom2,uout
        integer n1,n2,n3,uprnt,n1_all,n3_all,n1_loc
        integer nblock,iprec,istat,nb,igrid,ic,istat2
        integer igrid2,np,np_coor,l,un,ll1
        integer nxzdo, nxzup,npcf,n1_glob
        integer i,j,k,iex,n2_glob,n2_down,n2_up,d
        real*8 wt,w_old,w_new,utau,tauw1,x1,dx
        real dt,a
c        real(kind=8), dimension(:,:,:)  , allocatable :: pscalar
        real(kind=8), dimension(:,:,:,:), allocatable :: velo
        real(kind=8), dimension(:,:,:), allocatable :: cfdo,cfup
        real(kind=8), dimension(:,:,:), allocatable :: tauw
        real(kind=8), dimension(:,:,:,:), allocatable :: coor,coorc
        real(kind=8), dimension(:,:)  , allocatable :: velodo,veloup
        real(kind=8), dimension(:,:)  , allocatable :: velodo2,veloup2
        real(kind=8), dimension(:)  , allocatable :: uvdo,uvup
        real(kind=8), dimension(:)  , allocatable :: uvrms
        real(kind=8), dimension(:,:)  , allocatable :: velorms,velo_hom
        real(kind=8), dimension(:,:)  , allocatable :: velo_hom2,tauw_av
        real(kind=8), dimension(:)  , allocatable :: uv_hom
        real(kind=8), dimension(:)  , allocatable :: y_c,x_c
        real(kind=8), parameter :: zero = 0.0,half=0.5,one=1.0
        integer, parameter :: dball=90
c        character baseset(dball)*14
        character(len=6) :: textnum
        character(len=180):: header

        form = '(6(1x,g14.7))'
        form2 = '(2(1x,g14.7))'
         header = '#    Y            Umean            Urms'//
     &   '            Vrms            Wrms            UVrms'
        ll1 = len_trim(header)
        utau = 3.8928316332716
        dbname2 = 'convmesh.memcom.db'
c        dbname = 'time_1.5500.db'

c        nb = 1

        umemcom = 2
        umemcom2 = 1
        uerror = 6
        uprnt=6
        tauw1=0

        igrid2=0
        igrid=0

        call initmc(1,6,6)
        call opdidb(umemcom2,dbname2,1,' ',istat2)

        call getdes(umemcom2,'ADIR',0,istat2)
        call getbwi('NBR',nblock,istat2)
        call gprmdb('NWFLOAT',iprec,istat2)

c ... calculate the n2_glob - number of cells in y direction
c ... pair of block num 1 and 5 will be considered
        call makelabl('MDES',1,igrid2,0,cbuf,istat2)
        call gettab(umemcom2,cbuf,0,istat2)
        call getbwi('N2',n2_down,istat2)
        call getbwi('N1',n1_loc,istat2)
        call makelabl('MDES',5,igrid2,0,cbuf,istat2)
        call gettab(umemcom2,cbuf,0,istat2)
        call getbwi('N2',n2_up,istat2)
        n2_glob = n2_down + n2_up
        n1_glob = 4*n1_loc
c        print *,'n2_do, n2_up, n2_glob',n2_down,n2_up,n2_glob

c .. allocate sum arrays
c        allocate(psum(n2_glob))
c        allocate(pav(n2_glob))
         allocate(uvrms(n2_glob))
         allocate(velorms(3,n2_glob))
         allocate(velo_hom(3,n2_glob))
         allocate(velo_hom2(3,n2_glob))
         allocate(uv_hom(n2_glob))
         allocate(y_c(n2_glob))
         allocate(x_c(n1_glob))
         allocate(velodo(3,n2_down))
         allocate(veloup(3,n2_up))
         allocate(velodo2(3,n2_down))
         allocate(veloup2(3,n2_up))
         allocate(uvdo(n2_down))
         allocate(uvup(n2_up))
         allocate(tauw(dball,nblock,n1_loc))
         allocate(tauw_av(nblock,n1_loc))
c         allocate(tauwup(dball,nblock,n1_loc))

c        psum = 0
c        pav = 0
         uvrms = 0
         velorms = 0
         velodo = 0
         veloup = 0
         velodo2 = 0
         veloup2 = 0
         uvdo = 0
         uvup = 0

         velo_hom = 0
         velo_hom2 = 0
         uv_hom = 0
        dt=0.0054
        a = 0.783

        do d=1,dball
          umemcom = d + 2

          w_new = one / real(d)
          w_old = one - w_new
c          print *,'w_new,w_old',w_new,w_old

          n1_all = 0
          n3_all = 0

          nxzdo = 0
          nxzup = 0
          velodo = 0
          veloup = 0          
          velodo2 = 0
          veloup2 = 0
          uvdo = 0
          uvup = 0


          textnum = trim(str_f((d-1)*dt+a))
          list = 'time_'//textnum//'.db'
c          baseset(d) = trim(list)
          print *, trim(list)
          dbname = trim(list)


c ... initialize the database with solution     
        call opdidb(umemcom,dbname,0,' ',istat)      
        call filedb(dbname,istat)
c ... find the cycle number of the base
        call finddbcycle(umemcom,ic)
c        print *,'dbcycle number= ',ic
       

        do nb=1,nblock
        
        call makelabl('MDES',nb,igrid2,0,cbuf,istat2)
        call gettab(umemcom2,cbuf,0,istat2)
        call getbwi('N1',n1,istat2)
        call getbwi('N2',n2,istat2)
        call getbwi('N3',n3,istat2)
        
c        print *,'nb=, n1=, n2=, n3= ',nb,n1,n2,n3

c        print *,'nb',nb


         np_coor = (n1+1)*(n2+1)*(n3+1)
         np = (n1+2)*(n2+2)*(n3+2)
         npcf = 3*(n1+2)*(n3+2)

c         allocate(pscalar(0:n1+1,0:n2+1,0:n3+1))
         allocate(velo(3,0:n1+1,0:n2+1,0:n3+1))
         allocate(cfdo(3,0:n1+1,0:n3+1))
         allocate(cfup(3,0:n1+1,0:n3+1))

c
c
         if ((d.eq.1) .and. ((nb.eq.1) .or. (nb.eq.5))) then
           allocate(coor(3,n1+1,n2+1,n3+1))
           allocate(coorc(3,0:n1+1,0:n2+1,0:n3+1))
           call m2c_iomemcom(coor,'COOR',3*np_coor,1,
     &                     nb,igrid2,0,
     &                     umemcom2,6,1,istat2)
c      calculate cell center coordinates
           call center3d(coor,coorc,n1,n2,n3)
           do j=1,n2
             if (nb.eq.1) then
               y_c(j) = coorc(2,1,j,1)
               x1 = coorc(1,1,1,1)
               dx = coorc(1,2,1,1) - x1
             else
               y_c(j+n2_down) = coorc(2,1,j,1)
             endif      
           enddo
           deallocate(coor,coorc)
         endif

c         pscalar = 0
         velo = 0
c         coor = 0
c         coorc = 0


c         print *,'nb',nb

c         obtain data from memcom datasets
         call m2c_iomemcom(cfdo,'CF_3',npcf,1,
     &                           nb,igrid,ic,
     &                           umemcom,6,1,istat)
         call m2c_iomemcom(cfup,'CF_4',npcf,1,
     &                           nb,igrid,ic,
     &                           umemcom,6,1,istat)
c
         call m2c_iomemcom(velo,'VELO',np,3,
     &                        nb,igrid,ic,
     &                        umemcom,6,1,istat)

        
c        if ((nb.eq.1) .and. (d.eq.1)) then
c           print *,'(n1+2)*(n3+2)*3',(n1+2)*(n3+2)*3
c           npcf = (n1+2)*(n3+2)*3
c            allocate(cf(3,0:n1+1,0:n3+1))        
c                     call m2c_iomemcom(cf,'CF_3',npcf,1,
c     &                        nb,igrid,ic,
c     &                        umemcom,6,1,istat)

c            print *,'cf(1,1,1)',cf(1,1,1),cf(1,2,1),cf(1,3,1)
c        endif

        if ((nb.le.4) .or. ((nb.ge.9) .and. (nb.le.12))) then
          nxzdo = nxzdo + n1*n3        
        do j=1,n2
          do i=1,n1
            do k=1,n3
            do l=1,3
              velodo(l,j) = velodo(l,j) + velo(l,i,j,k)
              velodo2(l,j) = velodo2(l,j) + velo(l,i,j,k)**2
              enddo
              uvdo(j) = uvdo(j) + velo(1,i,j,k) * velo(2,i,j,k)
            enddo
          enddo
        enddo

        do i=1,n1
          tauw(d,nb,i) = cfdo(1,i,1)          
        enddo        

        else

          nxzup = nxzup + n1*n3
        do j=1,n2
          do i=1,n1
            do k=1,n3
            do l=1,3
              veloup(l,j) = veloup(l,j) + velo(l,i,j,k)
              veloup2(l,j) = veloup2(l,j) + velo(l,i,j,k)**2

              enddo
              uvup(j) = uvup(j) + velo(1,i,j,k) * velo(2,i,j,k)
            enddo
          enddo
        enddo

        do i=1,n1
          tauw(d,nb,i) = cfup(1,i,1)
        enddo


        endif


      deallocate(velo,cfdo,cfup)
c ... close loop for the blocks
      enddo

c      print *,'nxzdo,nxzup',nxzdo,nxzup

      do j=1,n2_down
        do l=1,3

        velo_hom(l,j) = velo_hom(l,j)*w_old +
     &                  w_new*(velodo(l,j)/real(nxzdo))
        velo_hom2(l,j) = velo_hom2(l,j)*w_old +
     &                  w_new*(velodo2(l,j)/real(nxzdo))

        enddo

        uv_hom(j) = uv_hom(j)*w_old +
     &                  w_new*(uvdo(j)/real(nxzdo))       

c        print *,'d,j,velo',d,j,velo_hom(1,j) 
      enddo

      do j=1,n2_up
        do l=1,3

        velo_hom(l,j+n2_down) = velo_hom(l,j+n2_down)*w_old + 
     &                          w_new*veloup(l,j)/real(nxzup)
        velo_hom2(l,j+n2_down) = velo_hom2(l,j+n2_down)*w_old +
     &                          w_new*veloup2(l,j)/real(nxzup)
        enddo

        uv_hom(j+n2_down) = uv_hom(j+n2_down)*w_old +
     &                  w_new*(uvup(j)/real(nxzdo))


c        print *,'d,j,velo',d,j+n2_down,velo_hom(1,j+n2_down)
      enddo
      


c ... averaged in homogenous for each j global


      do j=1,n2_glob
        do l=1,3
         velorms(l,j) = sqrt(velo_hom2(l,j) - velo_hom(l,j)**2)
        enddo
         uvrms(j) = uv_hom(j) - velo_hom(1,j)*velo_hom(2,j)
      enddo


c        deallocate(velodo,veloup)
c ...     closing the loop over databases
      enddo


c ... average in time tauw for both walls
c      tauw1=0
c      tauw2=0
      
      do nb=1,nblock
        do i=1,n1_loc
          tauw1 = 0
          do d=1,dball
            tauw1 = tauw1 + tauw(d,nb,i)            
          enddo
          tauw_av(nb,i) = tauw1/real(dball)
        enddo
      enddo
      
      do i=1,n1_glob
        x_c(i) = x1+(i-1)*dx
      enddo
        print *,'x_c(n1_glob)',x_c(n1_glob)


      un = 10
c      open(un+1,file = 'tauw_do') 
c      open(un+2,file = 'tauw_up')
c        do nb=1,nblock
c          do i=1,n1_loc            
c            if ((nb.le.4) .or. ((nb.ge.9) .and. (nb.le.12))) then
c              write(un+1,form2) x_c((nb-1)*n1_loc+i),tauw_av(nb,i)
c            else
c              write(un+2,form2) x_c((nb-1)*n1_loc+i),tauw_av(nb,i)
c            endif 
c          enddo
c        enddo 

c      close(un+1)
c      close(un+2)

      open(un,file = 'velorms_extr')
      write(un,'(a)') header(1:ll1)
      do j=1,n2_glob

          print *,'j=, ux= ', 
     &            j,velo_hom(1,j)
          write(un,form) y_c(j), velo_hom(1,j)/utau,
     &                   velorms(1,j)/utau,velorms(2,j)/utau,
     &                   velorms(3,j)/utau,-uvrms(j)/(utau**2)
c        
      enddo


      close(un)

      deallocate(velodo,veloup,y_c,velodo2,veloup2)
      deallocate(uvdo,uvup)
      deallocate(velorms,velo_hom,uvrms)
      deallocate(velo_hom2,uv_hom)
      deallocate(tauw,tauw_av)
      call endadb

      contains

      function str_f(k)
        !   "Convert a float to string."
        character(len=20)   :: str_f,format
        real, intent(in) :: k
        data format  /'(F10.4)'/
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
