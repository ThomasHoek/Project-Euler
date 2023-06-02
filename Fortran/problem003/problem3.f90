program euler3
   implicit none
   integer, parameter   :: k32 = selected_real_kind(32)
   real (kind=k32)      :: prime_counter, start, finish
   real(kind=k32)       :: n = 600851475143_k32

   prime_counter = 2
   call cpu_time(start)

   do while (prime_counter .LT. n)
      if (mod(n, prime_counter) .LT. 0.0001) then
         n = floor(n / prime_counter, k32)
      else
         prime_counter = prime_counter + 1
      end if
   end do

   print *, prime_counter
   call cpu_time(finish)
   print '("Time = ",f15.14," seconds.")',finish-start

end program euler3

