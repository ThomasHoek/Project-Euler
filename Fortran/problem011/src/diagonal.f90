module problem011_lib
   implicit none
   private

   public :: diagonal_product
contains
   subroutine diagonal_product(matrix, matrix_size, chunk_size, max_product, mirror)
      implicit none

      integer, dimension(:,:) :: matrix
      integer :: matrix_size, chunk_size, max_product
      integer, allocatable, dimension(:) :: v
      logical :: mirror
      integer :: i , j

      ! for every diagonal with size above chunk_size | assuming square
      do i = -(matrix_size-chunk_size), matrix_size-chunk_size
         ! get diagonal
         v = diag_modidied(matrix, i, mirror)

         ! loop over and get chunks of chunksize
         do j=1, size(v) - chunk_size + 1

            ! if bigger than max, keep it.
            max_product = max(product(v(j:j+chunk_size - 1)), max_product)
         end do
      end do

      return
   end subroutine diagonal_product

   module function diag_modidied(A, k, m) result(res)
      ! modified code from the stdlib-linalg dependancy
      integer, intent(in) :: A(:,:)
      integer, intent(in) :: k
      logical, intent(in) :: m
      integer :: res(minval(shape(A))-abs(k))
      integer :: i, sz
      integer :: matrix_size  ! for reverse
      sz = minval(shape(A))-abs(k)

      ! upper left to down right
      if (.not. m) then
         if (k > 0) then

            do i = 1, sz
               res(i) = A(i,k+i)
            end do

         else if (k < 0) then
            do i = 1, sz
               res(i) = A(i+abs(k),i)
            end do

         else
            do i = 1, sz
               res(i) = A(i,i)
            end do
         end if

      else ! upper right to down left
         matrix_size = minval(shape(A))
         if (k > 0) then
            do i = 1, sz
               res(i) = A(1 + matrix_size - i, k+i)
            end do

         else if (k < 0) then
            do i = 1, sz
               res(i) = A(1 + matrix_size - i - abs(k),  i )
            end do

         else
            do i = 1, sz
               res(i) = A(1 + matrix_size - i, i )
            end do
         end if
      end if
   end function diag_modidied
end module problem011_lib
