!***********************************************************************************************
program sums_bench
!***********************************************************************************************
! Actual benchmark. 
! * First part is about times (although accuracies are computed too)
!   - uses elapse times
!   - The reference result is the (very slow) one with quadruple precision accumulator
!     (this is why it is run first, although printed later)
!   - Beware that with ITER > 1 some compilers may skip some iterations
! * Second part is about accuracy
!   - The reference result is the one with the dp accumulator, as the qp accumular loop is not
!     tested because too slow, and it has been checked that the dp accumulator was giving the 
!     right result with the number of elements used in these tests
!
!    gfortran -O3 -fopenmp sums.f90 sums_bench.F90
! or
!    gfortran -O3 -ffast-math -DFAST -fopenmp sums.f90 sums_bench.F90
!***********************************************************************************************
use iso_fortran_env, only : int64
use sums
implicit none

real(sp), allocatable :: a(:)
integer, parameter :: P2MAX=30
integer :: i, k, n, metatest, lu, lseed
real :: ref
character(len=96) :: string, stringsep, suffix, filename

suffix = ""
#ifdef FAST
suffix = "_fast"
#endif

call random_seed(size=lseed)
call random_seed(put=[(0,i=1,lseed)])

TIMES: BLOCK 

integer, parameter :: ITER=1
real(sp) :: s, ss, tt, time

n = 2**P2MAX

100 format(A10,F16.4,X,F8.1,F10.3)
101 format(10X,A16  ,X,A8  ,A10  )
allocate(a(N))
filename = "../test/sums_bench_files/bench" // trim(suffix) // "_1.txt"
open(newunit=lu,file=filename)

write(stringsep,*) "==========================================================="
write(*,*) stringsep
write(*,*) "Sum of", n, " random numbers"
write(*,*) "With different methods:"
write(*,*) "* intrinsic sum()                   * do loop with a real32 accumulator"
write(*,*) "* do loop with a real64 accumulator * do loop with a real128 accumulator"
write(*,*) "* pairwise                          * pairwise with 10/100/1000 chunk length"
write(*,*) "* Kahan                             * Kahan with 10/100/1000 chunk length"

call random_number(a) 
!---------------------------
METATESTS:do metatest = 1, 2
!---------------------------

if (metatest == 1) then
   write(*,*) stringsep ; write(lu,*) stringsep
   write(string,*) "Random numbers in the [-0.5 ; 0.5[ interval"
   write(*,*) string    ; write(lu,*) string
   write(*,*) stringsep ; write(lu,*) stringsep
   a = a - 0.5   ! all values between -0.5 and 0.50
   ref = sqrt(n/12.0)   ! expectation of the std deviation
else if (metatest == 2) then
   write(*,*) stringsep; write(lu,*) stringsep
   write(string,*) "Random numbers in the [1.0 ; 2.0[ interval"
   write(*,*) string    ; write(lu,*) string
   write(*,*) stringsep; write(lu,*) stringsep
   a = a + 1.5   ! all values between 1.0 and 2.0
   ref = 1.5*n   ! expectation of the sum
end if

write(string,101) "Sum", "error2", "time" 
write(*,*) string    ; write(lu,*) string
     
call tictoc()
ss = 0.0
do i = 1, ITER
   ss = ss + sum_qp(a)
end do
call tictoc(tt)

call tictoc()
s = 0.0
do i = 1, ITER
   s = s + sum(a)
end do
call tictoc(time)
write(string,100) "sum       =", s, (s-ss)/spacing(ref), time
write(*,*) string    ; write(lu,*) string

call tictoc()
s = 0.0
do i = 1, ITER
   s = s + sum_sp(a)
end do
call tictoc(time)
write(string,100) "sum_sp    =", s, (s-ss)/spacing(ref), time
write(*,*) string    ; write(lu,*) string

call tictoc()
s = 0.0
do i = 1, ITER
   s = s + sum_dp(a)
end do
call tictoc(time)
write(string,100) "sum_dp    =", s, (s-ss)/spacing(ref), time
write(*,*) string    ; write(lu,*) string

write(string,100) "sum_qp    =", ss, 0.0, tt
write(*,*) string    ; write(lu,*) string

call tictoc()
s = 0.0
do i = 1, ITER
   s = s + psum(a)
end do
call tictoc(time)
write(string,100) "psum      =", s, (s-ss)/spacing(ref), time
write(*,*) string    ; write(lu,*) string

call tictoc()
s = 0.0
do i = 1, ITER
   s = s + psum(a,10)
end do
call tictoc(time)
write(string,100) "psum_10   =", s, (s-ss)/spacing(ref), time
write(*,*) string    ; write(lu,*) string

call tictoc()
s = 0.0
do i = 1, ITER
   s = s + psum(a,100)
end do
call tictoc(time)
write(string,100) "psum_100  =", s, (s-ss)/spacing(ref),time
write(*,*) string    ; write(lu,*) string

call tictoc()
s = 0.0
do i = 1, ITER
   s = s + psum(a,1000)
end do
call tictoc(time)
write(string,100) "psum_1000 =", s, (s-ss)/spacing(ref), time
write(*,*) string    ; write(lu,*) string

call tictoc()
s = 0.0
do i = 1, ITER
   s = s + ksum(a)
end do
call tictoc(time)
write(string,100) "ksum      =", s, (s-ss)/spacing(ref), time
write(*,*) string    ; write(lu,*) string

call tictoc()
s = 0.0
do i = 1, ITER
   s = s + ksumc(a,10)
end do
call tictoc(time)
write(string,100) "ksum_10   =", s, (s-ss)/spacing(ref), time
write(*,*) string    ; write(lu,*) string

call tictoc()
s = 0.0
do i = 1, ITER
   s = s + ksumc(a,100)
end do
call tictoc(time)
write(string,100) "ksum_100  =", s, (s-ss)/spacing(ref), time
write(*,*) string    ; write(lu,*) string

call tictoc()
s = 0.0
do i = 1, ITER
   s = s + ksumc(a,1000)
end do
call tictoc(time)
write(string,100) "ksum_1000 =", s, (s-ss)/spacing(ref), time
write(*,*) string    ; write(lu,*) string

!---------------
end do METATESTS
!---------------

deallocate(a)
close(lu)

END BLOCK TIMES




ACCURRACIES: BLOCK 

real(sp) :: allsums(0:11)
integer, parameter :: P2INC = 16
real, allocatable :: allerrs(:,:,:)

allocate( allerrs(11,0:P2MAX*P2INC,2) )

write(*,*) stringsep

!---------------------------
!---------------------------

do k = 0, P2MAX*P2INC
   n = nint(2**(k/P2INC) * 2d0**(mod(k,P2INC)/real(P2INC,kind=dp)))
   write(*,*) n
   allocate(a(n))
   call random_number(a)
   
   METATESTS:do metatest = 1, 2

      if (metatest == 1) then
         a = a - 0.5    ! all values between -0.5 and 0.5
         ref = sqrt(n/12.0)
      else if (metatest == 2) then
         a = a + 1.5    ! all values between 1.0 and 2.0
         ref = 1.5*n
      end if
      !$OMP PARALLEL SECTIONS IF(n > 100000)
      !$OMP SECTION
         allsums(0) = sum_ep(a)
      !$OMP SECTION
         allsums(8) = ksum(a)
      !$OMP SECTION
         allsums(4) = psum(a)
      !$OMP SECTION
         allsums(9) = ksumc(a,10)
      !$OMP SECTION
         allsums(5) = psum(a,10)
      !$OMP SECTION
         allsums(10) = ksumc(a,100)
      !$OMP SECTION
         allsums(6) = psum(a,100)
      !$OMP SECTION
         allsums(11) = ksumc(a,1000)
      !$OMP SECTION
         allsums(7) = psum(a,1000)
      !$OMP SECTION
         allsums(1) = sum(a)
      !$OMP SECTION
         allsums(2) = sum_sp(a)
      !$OMP SECTION
         allsums(3) = sum_dp(a)
      !$OMP END PARALLEL SECTIONS
      allerrs(:,k,metatest) = (allsums(1:)-allsums(0))/spacing(ref)
      
   end do METATESTS
      
   deallocate(a)
   
end do

filename = "../test/sums_bench_files/bench" // trim(suffix) // "_2.txt"
open(newunit=lu,file=filename)
do metatest = 1, 2
   do k = 0, P2MAX*P2INC
      n = nint(2**(k/P2INC) * 2d0**(mod(k,P2INC)/real(P2INC,kind=dp)))
      write(lu,*) metatest,n,allerrs(:,k,metatest)
   end do
end do
close(lu)

END BLOCK ACCURRACIES



contains

   subroutine tictoc(time)
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

