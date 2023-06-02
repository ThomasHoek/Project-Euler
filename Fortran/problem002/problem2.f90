program euler2
   implicit none
   integer :: a, b, temp, sum_num, max_num

   sum_num = 0
   max_num = 4*10**6
   a = 1
   b = 0

   do while (a .LT. max_num)
      temp = a   
      a = a + b
      b = temp
      
      ! if equal or close to 0. 
      if (mod(b, 2) .LT. 0.0001) then
         sum_num = sum_num + b
      endif

   end do
   print *, sum_num

end program euler2
