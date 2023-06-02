! https://en.wikipedia.org/wiki/Fibonacci_number#Binet's_formula
! Get Nth fibonacci number
program euler2
   implicit none
   integer :: n
   integer, parameter :: ikind = selected_real_kind(p=18)
   real :: a, b, u1(2, 1), u2(2, 1), sol_1(2, 1), sol_2(2, 1)

   n = 98
   a = (1.0 + sqrt(5.0))/2.0
   b = (1.0 - sqrt(5.0))/2.0

   u1 = reshape((/a, 1.0/), shape(u1))
   u2 = reshape((/b, 1.0/), shape(u1))

   sol_1 = u1*(a**n)*((5.0 + sqrt(5.0))/10)
   sol_2 = u2*(b**n)*((5.0 - sqrt(5.0))/10)

   print *, sol_1 + sol_2

end program euler2
