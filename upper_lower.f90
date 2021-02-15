subroutine To_upper(str)
     character(*), intent(in out) :: str
     integer :: i
 
     do i = 1, len(str)
       select case(str(i:i))
         case("a":"z")
           str(i:i) = achar(iachar(str(i:i))-32)
       end select
     end do 
   end subroutine To_upper
 
   subroutine To_lower(str)
     character(*), intent(in out) :: str
     integer :: i
 
     do i = 1, len(str)
       select case(str(i:i))
         case("A":"Z")
           str(i:i) = achar(iachar(str(i:i))+32)
       end select
     end do  
   end subroutine To_Lower