program misc_test
use misc
implicit none

real, allocatable, target :: a(:), d(:)
real, allocatable :: c(:,:)
real, pointer :: b(:,:)
integer, parameter :: NMMAX = 500
integer :: n, m

allocate( a(NMMAX**2) )
call random_number(a)

do n = 1, NMMAX
   do m = 1, NMMAX
      b(1:n,1:m) => a(1:n*m)
      c = transpose(b)
      d = a(1:n*m)
      call inplace_transpose(d,n,m)
      b(1:m,1:n) => d(:)
      if ( any(c /= b) ) then
         write(*,*) "WRONG inplace_transpose()",n,m
         error stop
      end if
   end do
   write(*,*) "n =",n,"PASSED"
end do

end program