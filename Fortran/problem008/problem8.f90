program euler8
   Use, intrinsic :: iso_fortran_env, Only: iostat_end
   implicit none

   integer, parameter:: int_p = selected_int_kind(10)
   character(len=50):: sequence
   integer(kind=int_p), allocatable :: current(:)
   integer :: index, counter, amount
   integer(kind=int_p) :: highest
   integer :: error

   ! variables

   highest = 0
   counter = 0
   amount = 13 ! test case amount = 4

   allocate (current(amount))

   ! main

   ! open file
   Open (10, file='input8.txt', status='old', action='read')

   ! while loop
   Do
      ! read file
      Read (10, *, iostat=error) sequence

      ! depending on error
      Select Case (error)
       Case (0)  ! reading went correct

         ! loop over every element
         do index = 1, len(sequence)

            ! read char as int and place it in the current array
            read (sequence(index:index),'(I10)') current(mod(counter, amount) + 1)

            ! we are above 13; check if the product is higher
            if (counter >= amount) highest = max(PRODUCT(current), highest)
            counter = counter + 1

         end do

       Case (iostat_end) ! end of file
         Exit
      End Select
   End Do

   print *, highest

end program euler8
