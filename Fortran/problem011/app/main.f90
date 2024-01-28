!  for i in {1..10}; do fpm run; done

program euler11
   use problem011_lib, only: diagonal_product
   implicit none
   integer, parameter :: n = 20         ! size of matrix
   integer, parameter :: chunksize = 4  ! size of chunks
   integer :: matrix(n, n)                   ! input matrix
   integer :: max_val, i
   real(kind=16):: start, end
   max_val = 0

   call cpu_time(start)

   open(unit = 1 , file = "input.txt")
   read(1,*) matrix

   ! up and down
   do i = 1, n-chunksize + 1
      max_val = max(maxval(product(matrix(i:i+chunksize - 1, :), dim=1)), max_val)
      max_val = max(maxval(product(matrix(:, i:i+chunksize - 1), dim=2)), max_val)
      exit
   end do

   ! diagonal one
   call diagonal_product(matrix, n, chunksize, max_val, .False.)

   ! diagonal two
   call diagonal_product(matrix, n, chunksize, max_val, .True.)

   print *, max_val

   call cpu_time(end)
   print '("Time = ",f18.15," seconds.")', end - start

end program euler11
