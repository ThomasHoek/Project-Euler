program euler10
   implicit none

   integer, parameter   :: k32 = selected_int_kind(12)

   integer (kind=k32), parameter :: limit = 2000000 
   print *, sum(sieve_of_eratosthenes(limit))

contains

   function sieve_of_eratosthenes(N) result(prime_array)
      implicit none

      !--------example_diag5scription:
      !   Language: Fortran 90.
      !   This code is written to JULES coding standards v1.
      !   http://jules-lsm.github.io/coding_standards/standard_code_templates/index.html
      !-----------------------------------------------------------------------------

      ! Subroutine arguments
      ! integer, parameter   :: k32 = selected_int_kind(12)
      integer (kind=k32), intent(in) :: N         ! limit

      ! Local variables
      integer (kind=k32), dimension(N) :: is_prime
      integer (kind=k32) :: number                ! number which is currently being checked
      integer (kind=k32) :: prime_amount          ! amount of primes found
      integer (kind=k32) :: prime_array_index     ! value to remember the index
      integer (kind=k32), allocatable :: prime_array(:)

      !-----------------------------------------------------------------------------

      is_prime = 1

      do number = 2, nint(sqrt(real(N)))
         if (is_prime(number) == 1) then

            is_prime(number*number:N:number) = 0
         end if
      end do

      ! Now we have a logical list of primes, make a new list of amount of TRUE called prime_array
      prime_amount = sum(is_prime) - 1
      allocate (prime_array(prime_amount))

      prime_array_index = 1
      ! For every value in prime_array; if index is 1; add index to prime_array
      do number = 2, N
         if (is_prime(number) == 1) then
            prime_array(prime_array_index) = number
            prime_array_index = prime_array_index + 1
         end if
      end do
   end function sieve_of_eratosthenes

end program euler10
