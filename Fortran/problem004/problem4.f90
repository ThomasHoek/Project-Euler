program euler4
   implicit none
   logical palindrone
   integer :: i, j,k, comb, highest = 0
   real :: start, finish
   
   do k = 1, 10
      call cpu_time(start)
      do i = 9999, 1000, -1
         do j = 9999, 1000, -1
            comb = i*j
            if (comb .LT. highest) cycle  ! go to next iter of j      
            if (palindrone(comb)) highest = comb
         end do
      end do

      ! print *, highest
      call cpu_time(finish)
      print '("Time = ",f18.15," seconds.")', finish - start
   end do
end program euler4

function palindrone(input) result(bool_out)
   implicit none
   integer:: input, input_copy, reverse, temp
   logical :: bool_out

   input_copy = input
   reverse = 0
   do while (input_copy > 0)
      temp = mod(input_copy, 10)
      reverse = reverse*10 + temp
      input_copy = input_copy/10
   end do
   bool_out = input .EQ. reverse
end function palindrone
