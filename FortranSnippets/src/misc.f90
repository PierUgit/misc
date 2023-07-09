!***********************************************************************************************
! Author: Pierre Hugonnet
! https://github.com/PierUgit
! License: GPL v3 
!***********************************************************************************************
! various stuff
!***********************************************************************************************
module misc
implicit none

private

public :: inplace_transpose

contains

!***********************************************************************************************
subroutine inplace_transpose(a,n,m)
!***********************************************************************************************
! In place transposition of a real (n,m) matrix to a (m,n) matrix, based on cycles
! The matrix is stored in a 1D n*m array
! Additional required memory: n*m bits
!***********************************************************************************************
use bitfield

integer, intent(in) :: n, m
real, intent(inout), target :: a(n*m)

integer :: cycle_start, cycle_to, cycle_from, i, j
real :: t
type(bitfield_t) :: b
real, pointer :: a1(:,:), a2(:,:)
!***********************************************************************************************

! trivial square matrix case
if (n == m) then
   a1(1:n,1:m) => a
   a2(1:m,1:n) => a
   do j = 1, n
      do i = 1, j-1
         t = a2(i,j)
         a2(i,j) = a1(j,i)
         a1(j,i) = t
      end do
   end do
   return
end if

call b%allocate(n*m)
call b%set(.false.)

do cycle_start = 1, n*m
   if (b%fget(cycle_start)) cycle
   t = a(cycle_start)              ! keep for the end of the cycle
   cycle_to = cycle_start
   do
      call b%set(cycle_to,.true.)
      cycle_from = transpose_1dindex(cycle_to,m,n)
      if (cycle_from == cycle_start) then
         a(cycle_to) = t
         exit   ! end of the cycle
      end if
      a(cycle_to) = a(cycle_from)
      cycle_to = cycle_from
   end do
end do

call b%deallocate()

CONTAINS

   pure integer function transpose_1dindex(knm,n,m) result(kmn)
   integer, intent(in) :: knm, n, m
   integer :: i, j
   j = (knm-1)/n + 1
   i = knm - (j-1)*n
   kmn = (i-1)*m + j
   end function transpose_1dindex

end subroutine

end module misc