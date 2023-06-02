program euler7
   implicit none

   real, parameter :: limit = 10001
   integer, allocatable, dimension(:) :: prime_lst

   ! upper limit
   prime_lst = sieve_of_eratosthenes(int(limit * log(limit) + limit * log(log(limit))))

   print *, prime_lst(int(limit))

contains

   function sieve_of_eratosthenes(N) result(prime_array)
      implicit none

      !-----------------------------------------------------------------------------
      ! Description:
      !   Finds primes until N
      !
      ! Method:
      !   This code makes use of the sieve of Atkin.
      !   The pseudocode from wikipedia.org is used as reference.
      !   https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
      !
      !   This is partially optimised myself
      !
      ! Code Description:
      !   Language: Fortran 90.
      !   This code is written to JULES coding standards v1.
      !   http://jules-lsm.github.io/coding_standards/standard_code_templates/index.html
      !-----------------------------------------------------------------------------

      ! Subroutine arguments
      integer, intent(in) :: N         ! limit
      integer, allocatable :: prime_array(:)

      ! Local variables
      integer, dimension(N) :: is_prime
      integer :: number                ! number which is currently being checked
      integer :: prime_amount          ! amount of primes found
      integer :: prime_array_index     ! value to remember the index

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
end program euler7
