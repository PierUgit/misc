program misc_test
use bitfield
use misc
implicit none


bitfield: BLOCK

real :: time
type(bitfield_t) :: bi, ci, di
logical, allocatable :: li(:)

print*, "bitfield tests 0"

call bi%allocate(3)
call bi%set([.true., .false., .true.])
if (bi%getsize() /= 3) error stop "assignment"
call bi%deallocate()

print*, "bitfield tests 10"

call bi%allocate(-10,60)
call bi%set(-10,10,.true.)
call bi%set(11,60,.false.)
if (.not.bi%fget(0)) error stop "get 1"
if (bi%fget(20)) error stop "get 2"
call bi%extract(0,20,ci)
if (ci%getlb() /= 0) error stop "getlb"

print*, "bitfield tests 20"

allocate(li(ci%getsize()))
call ci%get(li)
if (.not.all(li(1:11)) .or. any(li(12:21))) error stop "any"
if (bi%count(0,60) /= 11) error stop "count"
call bi%deallocate()
call ci%extract(5,15,di)
deallocate(li)
allocate(li(di%getsize()))
call di%get(li)
print*, li

print*, "bitfield tests 30"

call bi%allocate(10**9)
call bi%set(1,123456789,.true.)
call bi%set(123456790,10**9,.false.)
call tictoc()
print*, bi%count(1,10**9)
call tictoc(time)
print*, time

END BLOCK bitfield


inplace_tranpose: BLOCK

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