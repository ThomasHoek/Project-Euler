! https://mathworld.wolfram.com/FibonacciQ-Matrix.html
program euler2
   implicit none
   
   integer, parameter :: double_kind = selected_real_kind(p=18)
   integer:: i
   real (kind=double_kind) :: sum_num, max_num
   real (kind=double_kind), dimension(2, 2):: m1, m2
   real (kind=double_kind), dimension(2, 1):: m3, mfinal

   ! Initialise matrixes
   m1 = reshape([ & 
   1.0, 1.0, &
   1.0, 0.0], shape(m1))

   m2 = reshape([&
   1.0, 1.0, &
   1.0, 0.0], shape(m2))

   m3 = reshape([1.0, 0.0], shape(m3)) 

   mfinal = reshape((/1.0, 0.0/), shape(m3))

   ! Loop until fib +2 is bigger then maxnum
   max_num = 4*10**6

   do while (mfinal(1, 1) .LT. max_num)

      ! generate next fib sequence
      m1 = matmul(m1, m2)

      ! decode m1 matrix, to fib sequence
      mfinal = matmul(m1, m3)
      
      ! if we go for some reason above float limit.
      if (isnan(mfinal(1,1))) then
         print *, i
         exit
      endif

      ! if close or equal to 0
      if (mod(mfinal(2, 1), 2.0) .LT. 0.0001) then
         sum_num = sum_num + mfinal(2, 1)
      endif
   end do

   print *, sum_num
end program euler2
