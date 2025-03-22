program misc_test
use bitfield
use misc
!$ use omp_lib
implicit none


bitfield: BLOCK

real :: time
integer :: i, n, ipass, j
type(bitfield_t) :: bi, ci, di
logical, allocatable :: li(:)
double precision :: tic, toc
integer, parameter :: INC1 = 1, INC2 = 5, INC3 = 49
logical :: foo

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
call bi%set(60,11,-1,.false.)
if (.not.bi%fget(0)) error stop "a"
if (bi%fget(20)) error stop
call bi%extract(0,20,1,ci)
call ci%setlb(0)
if (ci%getlb() /= 0) error stop "b"
if (any(ci%fget() .neqv. bi%fget(0,20,1))) error stop 

write(*,*) "PASSED"

write(*,"(A)",advance="no") "bitfield tests 3..."

li = ci%fget()
if (.not.all(li(1:11)) .or. any(li(12:21))) error stop "a"
if (bi%count(0,60,1) /= 11) error stop "b1"
if (bi%count(60,0,-1) /= 11) error stop "b2"
call bi%deallocate()
call ci%extract(5,15,1,di)
if (any(di%fget() .neqv. li(6:16))) error stop "c1"
call di%deallocate()
call ci%extract(15,5,-1,di)
if (any(di%fget() .neqv. li(16:6:-1))) error stop "c2"
call ci%deallocate()
call di%deallocate()
deallocate( li )

write(*,*) "PASSED"

write(*,"(A40)",advance="no") "bitfield tests (setrange0 10**9 inc=1)..."

call tictoc()
call bi%allocate(10**9)
call bi%set(10**7,1,-1,.true.)
call bi%set(10**7+1,10**9,1,.false.)
call tictoc(time)
if (bi%count() /= 10**7) error stop

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A40)",advance="no") "bitfield tests (setrange0 10**9 inc=INC2)..."

bi = .false.
call tictoc()
call bi%set(1,10**9,INC2,.true.)
call tictoc(time)
if (bi%count() /= (10**9-1)/INC2+1) error stop

bi = .false.
call bi%set(10**9,1,-INC2,.true.)
if (bi%count() /= (10**9-1)/INC2+1) error stop

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A40)",advance="no") "bitfield tests (setrange0 10**9 inc=INC3)..."

bi = .false.
call tictoc()
call bi%set(1,10**9,INC3,.true.)
call tictoc(time)
if (bi%count() /= (10**9-1)/INC3+1) error stop

bi = .false.
call bi%set(10**9,1,-INC3,.true.)
if (bi%count() /= (10**9-1)/INC3+1) error stop

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A40)",advance="no") "bitfield tests (setrange1 10**9 inc=1)..."

allocate( li(10**8), source=.false. ) ; li(::3) = .true. ; n = count(li)
bi = .false.
call tictoc()
do i = 0, 9
   call bi%set(i*10**8+1,(i+1)*10**8,1,li)
end do
call tictoc(time)
if (bi%count() /= 10*n) error stop

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A40)",advance="no") "bitfield tests (getrange1 10**9 inc=1)..."

li = .false.
call tictoc()
do i = 0, 9
   call bi%get(i*10**8+1,(i+1)*10**8,1,li)
end do
call tictoc(time)
if (count(li) /= n) error stop
deallocate(li)

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A40)",advance="no") "bitfield tests (setrange1 10**9 inc=INC2)..."

bi = .false.
allocate( li((10**9-1)/INC2+1), source=.false. ) ; li(::3) = .true. ; n = count(li)
call tictoc()
call bi%set(1,10**9,INC2,li)
call tictoc(time)
if (bi%count(1,10**9,INC2) /= n) error stop

bi = .false.
call bi%set(10**9,1,-INC2,li)
call tictoc(time)
if (bi%count(INC2,10**9,INC2) /= n) error stop

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A40)",advance="no") "bitfield tests (getrange1 10**9 inc=INC2)..."

li = .false.
call tictoc()
call bi%get(INC2,10**9,INC2,li)
call tictoc(time)
if (count(li) /= n) error stop "a"

li = .false.
call bi%get(10**9,1,-INC2,li)
if (count(li) /= n) error stop "b"
deallocate(li)

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A40)",advance="no") "bitfield tests (setrange1 10**9 inc=INC3)..."

bi = .false.
allocate( li((10**9-1)/INC3+1), source=.false. ) ; li(::3) = .true. ; n = count(li)
call tictoc()
call bi%set(1,10**9,INC3,li)
call tictoc(time)
if (bi%count(1,10**9,INC3) /= n) error stop

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A40)",advance="no") "bitfield tests (getrange1 10**9 inc=INC3)..."

li = .false.
call tictoc()
call bi%get(1,10**9,INC3,li)
call tictoc(time)
if (count(li) /= n) error stop
deallocate(li)

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A40)",advance="no") "bitfield tests (sets0     10**9 inc=1)..."

allocate( li(10**9), source=.false. ) ; li(::3) = .true. ; n = count(li)
call tictoc()
do i = 1, 10**9
   call bi%set(i,li(i))
end do
call tictoc(time)
if (bi%count(1,10**9,1) /= n) error stop

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A40)",advance="no") "bitfield tests (gets0     10**9 inc=1)..."

li = .false.
call tictoc()
do i = 1, 10**9
   call bi%get(i,li(i))
end do
call tictoc(time)
if (count(li) /= n) error stop
deallocate( li )

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A40)",advance="no") "bitfield tests (sets0     10**9 inc=INC2)..."

allocate( li((10**9-1)/INC2+1), source=.false. ) ; li(::3) = .true. ; n = count(li)
call tictoc()
do i = 0, (10**9-1)/INC2
   call bi%set(i*INC2+1,li(i+1))
end do
call tictoc(time)
if (bi%count(1,10**9,INC2) /= n) error stop

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A40)",advance="no") "bitfield tests (gets0     10**9 inc=INC2)..."

li = .false.
call tictoc()
do i = 0, (10**9-1)/INC2
   call bi%get(i*INC2+1,li(i+1))
end do
call tictoc(time)
if (count(li) /= n) error stop
deallocate( li )

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A40)",advance="no") "bitfield tests (sets0     10**9 inc=INC3)..."

allocate( li((10**9-1)/INC3+1), source=.false. ) ; li(::3) = .true. ; n = count(li)
call tictoc()
do i = 0, (10**9-1)/INC3
   call bi%set(i*INC3+1,li(i+1))
end do
call tictoc(time)
if (bi%count(1,10**9,INC3) /= n) error stop

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A40)",advance="no") "bitfield tests (gets0     10**9 inc=INC3)..."

li = .false.
call tictoc()
do i = 0, (10**9-1)/INC3
   call bi%get(i*INC3+1,li(i+1))
end do
call tictoc(time)
if (count(li) /= n) error stop
deallocate( li )

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A40)",advance="no") "bitfield tests (extract   10**9 inc=1)..."

bi = .false. ; call bi%set( 10**8+1, 10**9, 1, .true. )
n = bi%count();
call tictoc()
call bi%extract( 10**8+1, 10**9, 1, ci )
call bi%replace( 1, 9*10**8, 1, ci )
call tictoc(time)
if (bi%count() /= bi%getsize()) error stop
call ci%deallocate()

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A40)",advance="no") "bitfield tests (extract   10**9 inc=INC2)..."

bi = .false.; call bi%set( 10**8+1, 10**9, INC2, .true. )
n = bi%count();
call tictoc()
call bi%extract( 10**8+1, 10**9  , INC2, ci )
call bi%replace( 10**8  , 10**9-1, INC2, ci )
call tictoc(time)
if (bi%count() /= 2*n) error stop "a"
call ci%deallocate()

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A40)",advance="no") "bitfield tests (extract   10**9 inc=INC3)..."

bi = .false.; call bi%set( 10**8+1, 10**9, INC3, .true. )
n = bi%count();
call tictoc()
call bi%extract( 10**8+1, 10**9  , INC3, ci )
call bi%replace( 10**8  , 10**9-1, INC3, ci )
call tictoc(time)
if (bi%count() /= 2*n) error stop
call ci%deallocate()

bi = .false.; call bi%set( 10**9, 10**8+1, -INC3, .true. )
n = bi%count();
call bi%extract( 10**9  , 10**8+1,-INC3, ci )
call bi%replace( 10**9-1, 10**8  ,-INC3, ci )
call tictoc(time)
if (bi%count() /= 2*n) error stop "b"
call ci%deallocate()

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A40)",advance="no") "bitfield tests (count     10**9 inc=1)..."

bi = .false.
call bi%set(1,10**9,INC2,.true.)
call tictoc()
if (bi%count() /= (10**9-1)/INC2+1) error stop
call tictoc(time)

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A40)",advance="no") "bitfield tests (count     10**9 inc=INC2)..."

call tictoc()
if (bi%count(1,10**9,INC2) /= (10**9-1)/INC2+1) error stop
call tictoc(time)

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A40)",advance="no") "bitfield tests (count     10**9 inc=INC3)..."

li = bi%fget(1,10**9,INC3)
call tictoc()
if (bi%count(1,10**9,INC3) /= count(li)) error stop
call tictoc(time)

write(*,*) "PASSED (", time, "sec.)"

write(*,"(A40)",advance="no") "bitfield tests (anyall    10**9 inc=1)..."

bi = .false.
if (bi%any() .or. bi%all()) error stop
call bi%set(1,10**9,INC2,.true.)
if (.not.bi%any() .or. bi%all()) error stop
bi = .true.
if (.not.bi%any() .or. .not.bi%all()) error stop

write(*,*) "PASSED"

write(*,"(A40)",advance="no") "bitfield tests (anyall    10**9 inc=INC2)..."

bi = .false.
if (bi%any(1,10**9,INC2) .or. bi%all(10**9,1,-INC2)) error stop "a"
call bi%set(1,10**9,2*INC2,.true.)
if (.not.bi%any(1,10**9,INC2) .or. bi%all(1,10**9,INC2)) error stop "b"
bi = .true.
if (.not.bi%any(10**9,1,-INC2) .or. .not.bi%all(1,10**9,INC2)) error stop "c"

write(*,*) "PASSED"

write(*,"(A40)",advance="no") "bitfield tests (anyall    10**9 inc=INC3)..."

bi = .false.
if (bi%any(1,10**9,INC3) .or. bi%all(10**9,1,-INC3)) error stop
call bi%set(1,10**9,2*INC3,.true.)
if (.not.bi%any(1,10**9,INC3) .or. bi%all(1,10**9,INC3)) error stop
bi = .true.
if (.not.bi%any(10**9,1,-INC3) .or. .not.bi%all(1,10**9,INC3)) error stop

write(*,*) "PASSED"

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