      program rename 
         implicit none
         character(len=180) :: command,dbpattern,list
         character(len=6) :: textnum
         character(len=5) :: textnum2
         integer i,l
         real dt,a
         dbpattern = 'convmesh.memcom.db.77.time'
         dt=0.0054
         a = 0.783
         do i=1,143
           textnum = trim(str_f((i-1)*dt+a))
           l = len(textnum)

         if (textnum(1:1).eq.'0') then

           if (textnum(l:l).eq.'0') then 
             l=l-1
             textnum2 = trim(textnum(1:l))
             command = 'mv '//trim(dbpattern)//textnum2(2:l)//' '// &
                     'time_'//textnum//'.db'
             command = trim(command)
!             list = 'time_'//textnum//'.db'
!             list = trim(list)
           else
             command = 'mv '//trim(dbpattern)//textnum(2:l)//' '// &
                     'time_'//textnum//'.db'
             command = trim(command)
           end if

         else

           if (textnum(l:l).eq.'0') then
             l=l-1
             textnum2 = trim(textnum(1:l))
             command = 'mv '//trim(dbpattern)//textnum2//' '// &
                     'time_'//textnum//'.db'
             command = trim(command)
           else
             command = 'mv '//trim(dbpattern)//textnum//' '// &
                     'time_'//textnum//'.db'
             command = trim(command)
           end if

        endif
            
             list = 'time_'//textnum//'.db'
             list = trim(list)



!           CALL system(command)
            print *,list
         enddo
      contains
!
      function str(k)
        !   "Convert an integer to string."
        character(len=20)   :: str
        integer, intent(in) :: k        
        write (str, *) k
        str = adjustl(str)
      end function str
!
      function str_f(k)
        !   "Convert a float to string."
        character(len=20)   :: str_f,format
        real, intent(in) :: k
        data format  /'(F10.4)'/
        write (str_f, format) k
        str_f = adjustl(str_f)
      end function str_f
!
      end program
