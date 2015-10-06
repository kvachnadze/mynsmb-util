      program rename_csv
        implicit none
        integer doflag
        integer rm1,rm2,incr,newset
        integer i, j, k
        character*60 command1,command2
        character*30 oldname,newname

!        nstep = 2660


      !input parameters
!      print *,'***** INPUT'
      print *,'***** DO YOU WANT TO (1)REMOVE, (2)COPY or to (3)RENAME the CSV files to the SpySi format? *****'
      read *,doflag

      if (doflag.eq.1) then

        print *,'***** give the start value of csv file to remove from:'
        read *,rm1
        print *,'***** give the last value of csv file to remove to:'
        read *,rm2
        print *,'***** give the csv files increment number:'
        read *,incr

!   removing csv files of beginning of 2nd cycle (if we do not need them)
        do i=rm1,rm2,incr
          oldname = trim(str(i))//'.csv'
          command2 = 'rm '//trim(oldname)
!          print *, command2
          call system(command2)
        enddo
!     
      else

      if (doflag.eq.2) then

        print *,'**************************************************************'    
        print *,'***** COPYING the set of csv files *****' 
        print *,'**************************************************************'    
        print *,'Before startting REMOVE CSV files that you do not want to copy'
        print *,'**************************************************************'    
        print *,'***** give the start value of the set:'
        read *,rm1
        print *,'***** give the last value of the set:'
        read *,rm2
        print *,'***** give the csv files increment number:'
        read *,incr
        print *,'***** how many additional sets do you want? give the number of additional sets:'
        read *,newset


! copying 1st cycle to have 10 cycles and renaming
        do k=1,newset

         do i=rm1,rm2,incr
          oldname = trim(str(i))//'.csv'
          j=i+rm2*k
          newname = trim(str(j))//'.csv'
          command2 ='cp '//trim(oldname)//' '//trim(newname)
!          print *, command2
          call system(command2)
         enddo
        enddo
!
      endif

      if (doflag.eq.3) then     

        print *,'**************************************************************'    
        print *,'***** RENAMING the csv files to the SpySi format *****' 
        print *,'**************************************************************'    
        print *,'***** give the start value of csv file to rename from:'
        read *,rm1
        print *,'***** give the last value of csv file to rename to:'
        read *,rm2
        print *,'***** give the csv files increment number:'
        read *,incr


! renaming to spisy appropriate name with 0's
        do i=rm1,rm2,incr
          oldname = trim(str(i))//'.csv'
          call getcsvname(rm2,i,newname)
          if (int(log10(1.0*rm2)).ne.int(log10(1.0*i))) then
            command1 = 'mv '//trim(oldname)//' '//trim(newname)
            call system(command1)
          endif
!          print *, command1
        enddo      


      endif
     endif    

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

