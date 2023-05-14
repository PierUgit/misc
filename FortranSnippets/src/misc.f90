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

integer :: k, i1, j1, k1, i2, j2, k2
real :: t
type(bitfield_t) :: b
real, pointer :: a1(:,:), a2(:,:)
!***********************************************************************************************

! trivial square matrix case
if (n == m) then
   a1(1:n,1:m) => a
   a2(1:m,1:n) => a
   do j2 = 1, n
      do i2 = 1, j2-1
         t = a2(i2,j2)
         a2(i2,j2) = a1(j2,i2)
         a1(j2,i2) = t
      end do
   end do
   return
end if

call b%allocate(n*m)
call b%setall(.false.)

do k = 1, n*m
   if (b%get(k)) cycle   ! element already updated
   ! start of a new cycle at element k
   t = a(k)              ! keep for later
   call b%set(k,.true.)  ! mark as "being updated"
   k2 = k
   do
      j2 = (k2-1)/m + 1    ! indexes in the destination matrix
      i2 = k2 - (j2-1)*m   !
      i1 = j2              ! indexes in the source matrix
      j1 = i2              !
      k1 = (j1-1)*n + i1   ! position of the source element in the 1D array
      if (b%get(k1)) then  
         ! if the source element is marked, then it was the first one of the cycle
         a(k2) = t   ! upadte the destination with the value that had been stored 
         exit   ! end of the cycle
      end if
      a(k2) = a(k1)           ! update destination 
      call b%set(k1,.true.)   ! mark source as used
      k2 = k1
   end do
end do

call b%deallocate()

end subroutine

end module misc