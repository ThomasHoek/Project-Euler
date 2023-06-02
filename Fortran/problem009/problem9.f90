! https://mathworld.wolfram.com/PythagoreanTriple.html
! https://en.wikipedia.org/wiki/Tree_of_primitive_Pythagorean_triples
! https://en.wikipedia.org/wiki/Formulas_for_generating_Pythagorean_triples#A_Ternary_Tree:_Generating_All_Primitive_Pythagorean_Triples

program euler9
   implicit none

   integer :: limit
   ! integer, parameter   :: k32 = selected_int_kind(12)
   ! integer (kind=k32), allocatable :: M(:)   ! parents
   ! integer (kind=k32), dimension(3, 3) :: U, A, D  ! tree's

   integer , allocatable :: M(:)   ! start of the tree
   integer , dimension(3, 3) :: U, A, D  ! branches
   logical :: ans
   real :: start, finish
   
   
   call cpu_time(start)
   limit = 2000

   M = [3, 4, 5]

   U =  reshape([ &
      1,  2,  2, &
      -2, -1, -2, &
      2,  2,  3 ], shape(U))

   A =  reshape([ &
      1, 2, 2, &
      2, 1, 2, &
      2, 2, 3 ], shape(A))

   D =  reshape([ &
      -1, -2, -2, &
      2,  1,  2, &
      2,  2,  3 ], shape(D))

   ans = primitive_triple(M)
   call cpu_time(finish)
   print '("Time = ",f18.15," seconds.")', finish - start

contains
   ! uses arguments from program, not independent.
   recursive function primitive_triple(triple) result(solution)
      ! variables
      ! integer (kind=k32), intent(in), dimension(3):: triple
      ! integer (kind=k32) :: mult, s_mult_trip

      integer, intent(in), dimension(3):: triple

      ! local variables
      integer :: mult, s_mult_trip
      logical :: solution
      solution = .false.

      ! check self
      if (sum(triple) == limit) then
         solution = .true.
         print *, product(triple * mult)
         return
      end if

      ! check if above limit
      if (sum(triple) > limit) then
         return
      end if
      
      ! check multiplications of primitive
      s_mult_trip = 1
      mult = 2
      do while(s_mult_trip < limit)
         s_mult_trip = sum(triple * mult)

         if (s_mult_trip == limit) then
            solution = .true.
            print *, product(triple * mult)
            return
         end if
         mult = mult + 1
      end do

      ! Ternary_Tree
      if (primitive_triple(matmul(U, triple))) return
      if (primitive_triple(matmul(A, triple))) return
      if (primitive_triple(matmul(D, triple))) return
   end function primitive_triple

end program euler9
