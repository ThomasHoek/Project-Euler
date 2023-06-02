program euler5
   implicit none
   integer  :: num = 0, highest = 20
   integer  :: i
   real     :: start, end

   call cpu_time(start)
   outer: do while (.true.)
      num = num + (highest * (highest - 1))
      do i = highest - 2, 1, -1
         if (i == 1) exit outer

         if (mod(num, i) /= 0) cycle outer

      end do
   end do outer
   print *, num
   call CPU_TIME(end)
   print '("Time = ",f15.14," seconds.")',end-start

end program euler5
