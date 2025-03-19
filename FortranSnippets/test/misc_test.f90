program misc_test
use bitfield
use misc
!$ use omp_lib
implicit none


bitfield: BLOCK

real :: time
integer :: i, n
type(bitfield_t) :: bi, ci, di
logical, allocatable :: li(:)
double precision :: tic, toc

if (.not.bitfield_check()) error stop "bitfield is not usable"

write(*,"(A)",advance="no") "bitfield tests 1..."

li = [.true., .false., .true.]
bi = li
if (bi%getsize() /= 3) error stop
if (any(bi%fget() .neqv. li)) error stop
call bi%deallocate()

write(*,*) "PASSED"

write(*,"(A)",advance="no") "bitfield tests 2..."

call bi%allocate(-10,60)
call bi%set(-10,10,1,.true.)
call bi%set(11,60,1,.false.)
if (.not.bi%fget(0)) error stop
if (bi%fget(20)) error stop
call bi%extract(0,20,1,ci)
call ci%setlb(0)
if (ci%getlb() /= 0) error stop
if (any(ci%fget() .neqv. bi%fget(0,20,1))) error stop 

write(*,*) "PASSED"

write(*,"(A)",advance="no") "bitfield tests 3..."

li = ci%fget()
if (.not.all(li(1:11)) .or. any(li(12:21))) error stop "a"
if (bi%count(0,60,1) /= 11) error stop "b"
call bi%deallocate()
call ci%extract(5,15,1,di)
if (any(di%fget() .neqv. li(6:16))) error stop "c"
call ci%deallocate()
call di%deallocate()
deallocate( li )

write(*,*) "PASSED"

write(*,"(A30)",advance="no") "bitfield tests (setrange0 10**9 inc=1)..."

call tictoc()
call bi%allocate(10**9)
call bi%set(1,10**7,1,.true.)
call bi%set(10**7+1,10**9,1,.false.)
call tictoc(time)
if (bi%count() /= 10**7) error stop
call bi%deallocate()

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A30)",advance="no") "bitfield tests (setrange0 10**9 inc=11)..."

call bi%allocate(10**9) ; bi = .false.
call tictoc()
call bi%set(1,10**9,11,.true.)
call tictoc(time)
if (bi%count() /= (10**9-1)/11+1) error stop
call bi%deallocate()

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A30)",advance="no") "bitfield tests (setrange0 10**9 inc=101)..."

call bi%allocate(10**9) ; bi = .false.
call tictoc()
call bi%set(1,10**9,101,.true.)
call tictoc(time)
if (bi%count() /= (10**9-1)/101+1) error stop
call bi%deallocate()

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A30)",advance="no") "bitfield tests (setrange1 10**9 inc=1)..."

call bi%allocate(10**9)
allocate( li(10**8), source=.true. )
call tictoc()
do i = 0, 9
   call bi%set(i*10**8+1,(i+1)*10**8,1,li)
end do
call tictoc(time)
if (bi%count() /= 10**9) error stop
call bi%deallocate()
deallocate(li)

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A30)",advance="no") "bitfield tests (setrange1 10**9 inc=11)..."

call bi%allocate(10**9); bi = .false.
allocate( li((10**9-1)/11+1), source=.true. )
call tictoc()
call bi%set(1,10**9,11,li)
call tictoc(time)
if (bi%count() /= size(li)) error stop
call bi%deallocate()
deallocate( li )

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A30)",advance="no") "bitfield tests (setrange1 10**9 inc=101)..."

call bi%allocate(10**9); bi = .false.
allocate( li((10**9-1)/101+1), source=.true. )
call tictoc()
call bi%set(1,10**9,101,li)
call tictoc(time)
if (bi%count() /= size(li)) error stop
call bi%deallocate()

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A30)",advance="no") "bitfield tests (many sets 10**7 inc=1)..."

call bi%allocate(10**9) ; bi = .true.
call tictoc()
do i = 1, 10**7
   call bi%set(i,.false.)
end do
call tictoc(time)
if (bi%count(1,10**7,1) /= 0) error stop

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A30)",advance="no") "bitfield tests (many sets 10**8 inc=11)..."

call tictoc()
!call bi%allocate(10**9)
do i = 10**7+1, 10**8, 11
   call bi%set(i,.false.)
end do
call tictoc(time)
if (bi%count(10**7+1,10**8,11) /= 0) error stop

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A30)",advance="no") "bitfield tests (many sets 10**9 inc=101)..."

call tictoc()
!call bi%allocate(10**9)
do i = 10**8+1, 10**9, 101
   call bi%set(i,.false.)
end do
call tictoc(time)
if (bi%count(10**8+1, 10**9, 101) /= 0) error stop

write(*,*) "PASSED (", time, "sec.)"

END BLOCK bitfield


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