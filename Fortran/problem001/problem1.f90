program euler1
   implicit none
   integer i, i_max, sum
   ! i_max = 9
   i_max = 999
   do i = 1, i_max
      if (mod(i, 3) == 0 .OR. mod(i, 5) == 0) then
         sum = sum + i
      end if
   end do

   print *, sum

end program euler1
