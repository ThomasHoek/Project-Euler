program euler6
   implicit none
   integer, parameter   :: k32 = selected_real_kind(12)
   integer   (kind=k32) :: i, total, sum_of_squares, square_of_sum
   REAL (kind=k32)      :: start, end
   
   sum_of_squares = 0_k32
   square_of_sum = 0_k32

   call cpu_time(start)
   sum_of_squares = 0_k32
   square_of_sum= 0_k32

   do i = 1, 1*(10_k32**5)
      sum_of_squares = sum_of_squares + i*i
      square_of_sum = square_of_sum +i
   end do
   
   total = (square_of_sum*square_of_sum) - sum_of_squares
   print *, total
   call cpu_time(end)

   print '("Time = ",f18.14," seconds.")',end-start
end program euler6

! program euler6
!    implicit none
!    integer :: i
!    integer, dimension(100) :: arr =(/(i, i=1,100, 1)/)
!    integer, dimension(100) :: arr_square_sum, arr_sum_square
   
!    arr_square_sum(1) = 1
!    arr_sum_square(1) = 1

!    do i=2, 100
!       arr_square_sum(i) = arr_square_sum(i-1) + arr(i) * arr(i)
!       arr_sum_square(i) = SUM(arr(:i)) ** 2

!    end do 

!    arr = arr_sum_square - arr_square_sum
!    print *, arr(100)
   
! end program euler6