program misc_test
use misc
!$ use omp_lib
implicit none


inplace_tranpose: BLOCK

real, allocatable, target :: a(:), d(:)
real, allocatable :: c(:,:)
real, pointer :: b(:,:)
integer, parameter :: NMMAX = 500
integer :: n, m
real :: time, x
allocate( a(NMMAX**2) )
call random_number(a)

write(*,"(A)",advance="no") "In place transpose test..."

call tictoc()
!$OMP PARALLEL DO SCHEDULE(nonmonotonic:dynamic) PRIVATE(b,c,d,x)
do n = 1, NMMAX
   do m = 1, NMMAX
      call random_number(x)
      if (x >= 0.1) cycle
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
end do
!$OMP END PARALLEL DO
call tictoc(time)

write(*,*) "PASSED (", time, "sec.)" 

END BLOCK inplace_tranpose


contains

   subroutine tictoc(time)
   use iso_fortran_env, only: int64
   real, intent(out), optional :: time
   integer(int64) :: tic, toc, rate
   save :: tic
   if (present(time)) then
      call system_clock(toc,rate)
      time = real(toc-tic)/rate
   else
      call system_clock(tic)
   end if
   end subroutine

end program