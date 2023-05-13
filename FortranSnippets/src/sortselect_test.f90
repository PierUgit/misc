Program sortselect_test

   use iso_fortran_env
   use sortselect
   implicit none

   integer, parameter :: dp = kind(1d0)
   logical, parameter :: FILOUT = .true.
   integer, parameter :: P2INC = 16
    
   real, allocatable :: a(:), b(:)
   double precision :: x
   integer :: p2min, p2max, prand, n, nth, ntest, n_old, metatest
   integer :: i, j, k, kref, idist, lu
   character(len=20), allocatable :: testlist(:)
   character(len=128) :: filename
   real, allocatable :: v(:), time(:)
   
   
   
   do metatest = 4, 4
   
   
   
   write(*,*) "========================================================================="
   write(*,*) "========================================================================="
   write(*,*) "========================================================================="
   
   
   if (METATEST == 1) then
      testlist = [character(20) :: "insertselect",  &
                                   "heapselect",    &
                                   "quickselectR",  &
                                   "bisection00",   &
                                   "bisection10",   &
                                   "bisection01",   &
                                   "bisection11"    ]
      p2min = 1
      p2max = 20
      QUICKSELECT_REVERT2HEAP = 0
      BISECSELECT_STRICT_REVERT2HEAP = 0
      BISECSELECT_NONSTRICT_REVERT2HEAP = 0
      BISECSELECT_ALLOC_THRESHOLD = 0
   else if (METATEST == 2) then
      testlist = [character(20) :: "quickselectR",  &
                                   "bisection00",   &
                                   "bisection10",   &
                                   "bisection01",   &
                                   "bisection11"    ]
      p2min = 1
      p2max = 29
      BISECSELECT_ALLOC_THRESHOLD = 0
   else if (METATEST == 3) then
      testlist = [character(20) :: "insertsort",  &
                                   "heapsort",    &
                                   "quicksortR",  &
                                   "quicksortM",  &
                                   "quicksortT"   ]
      p2min = 1
      p2max = 20
      QUICKSORT_REVERT2HEAP = 0
      HEAPSORT_REVERT2INSERT = 0
   else if (METATEST == 4) then
      testlist = [character(20) :: "insertsort",  &
                                   "heapsort",    &
                                   "quicksortR",  &
                                   "quicksortM",  &
                                   "quicksortT"   ]
      p2min = 1
      p2max = 26
   end if 
   
   ntest = size(testlist)
   allocate( v(ntest), time(ntest), source=0.0 )
   write(filename,"(I1)") METATEST
   filename = "../test/sortselect_test_files/test"//filename(1:1)//".txt"
   if (FILOUT) open(newunit=lu,file=filename)
   
   do idist = 1, 5
      allocate(b(2**p2max))
      if (idist == 1) call random_number2(b,1,1e4)
      if (idist == 2) call random_number2(b,3,1e4)
      if (idist == 3) call random_number2(b,9,1e4)
      if (idist == 4) call random_number_gaussian(b,1e4)
      if (idist == 5) then
         call random_number2(b,1,1e-4)
         b = b + 1.0
      end if
      do i = p2min, p2max
      do j = 0, P2INC-1
         if (i == p2max .and. j > 0) exit
         n = 2**i * 2**(real(j)/P2INC)
         if (n == n_old) cycle
         n_old = n
               
         call random_number(x)
         nth = floor(x*n) + 1
            
         do k = 1, ntest
            if (idist > 1 .and. (testlist(k)(1:9) /= "bisection" .and. testlist(k) /= "quickselectR")) then
               time(k) = huge(time)
               cycle
            end if
            a = b(1:max(n,2**20))
            call testit(a,testlist(k),n,nth,v(k),time(k))
         end do

         write(*,"(I2,I12,20E12.3)") idist, n, time(:)
         if (FILOUT) write(lu,*) idist, n, time(:)

         do k = 1, ntest
            if (time(k) /= huge(time) .and. testlist(k)(1:10) /= "bisection1") then
               kref = k
               exit
            end if
         end do
         if (any(time(:) /= huge(time) .and. v(:) /= v(kref) .and. testlist(:)(1:10) /= "bisection1")) then
            write(*,*) v(:)
            error stop "v problem"
         end if

      end do
      end do
      deallocate(b)
   end do
   
   deallocate(v, time)
   if (FILOUT) close(lu)

   end do
   
   
   
contains

   !===========================================
   subroutine testit(a,method,n,nth,v,time)
   !===========================================
   real, intent(inout) :: a(:)
   integer, intent(in) :: n, nth
   character(len=*) :: method
   real, intent(out) :: v, time
   
   integer :: nth2, ifirst, ilast
   integer(int64) :: tic, toc, rate
   logical :: computed
   !===========================================
   computed = .true.
   call system_clock(tic,rate)
   nth2 = nth      
   select case (method)
      case("quickselectR"); 
         do ilast = n, size(a), n
            ifirst = ilast - n + 1
            v = quickselect(a(ifirst:ilast),nth2,pivot="R")
            nth2 = mod(nth2,n) + 1
         end do
      case("quickselectM")
         if (real(n) * size(a) <= 1e16) then
            do ilast = n, size(a), n
               ifirst = ilast - n + 1
               v = quickselect(a(ifirst:ilast),nth2,pivot="M")
               nth2 = mod(nth2,n) + 1
            end do
         else
            computed = .false.
         end if
      case("quickselectT")
         if (real(n) * size(a) <= 1e16) then
            do ilast = n, size(a), n
               ifirst = ilast - n + 1
               v = quickselect(a(ifirst:ilast),nth2,pivot="T")
               nth2 = mod(nth2,n) + 1
            end do
         else
            computed = .false.
         end if
      case("heapselect")
         if (real(n) * size(a) <= 1e14) then
            do ilast = n, size(a), n
               ifirst = ilast - n + 1
               v = heapselect(a(ifirst:ilast),nth2)
               nth2 = mod(nth2,n) + 1
            end do
         else
            computed = .false.
         end if
      case("insertselect")
         if (real(n) * size(a) <= 1e10) then
            do ilast = n, size(a), n
               ifirst = ilast - n + 1
               v = insertionselect(a(ifirst:ilast),nth2)
               nth2 = mod(nth2,n) + 1
            end do
         else
            computed = .false.
         end if
      case("bisection00");   
         do ilast = n, size(a), n
            ifirst = ilast - n + 1
            v = bisectionselect(a(ifirst:ilast),nth2)
            nth2 = mod(nth2,n) + 1
         end do
      case("bisection10")
         do ilast = n, size(a), n
            ifirst = ilast - n + 1
            v = bisectionselect(a(ifirst:ilast),nth2,strict=.false.)
            nth2 = mod(nth2,n) + 1
         end do
      case("bisection01")
         do ilast = n, size(a), n
            ifirst = ilast - n + 1
            v = bisectionselect(a(ifirst:ilast),nth2,alloc=.true.)
            nth2 = mod(nth2,n) + 1
         end do
      case("bisection11")
         do ilast = n, size(a), n
            ifirst = ilast - n + 1
            v = bisectionselect(a(ifirst:ilast),nth2,strict=.false.,alloc=.true.)
            nth2 = mod(nth2,n) + 1
         end do
      case("quicksortR"); 
         do ilast = n, size(a), n
            ifirst = ilast - n + 1
            call quicksort(a(ifirst:ilast),pivot="R")
         end do
         v = a(ifirst+n/2)
      case("quicksortM")
         do ilast = n, size(a), n
            ifirst = ilast - n + 1
            call quicksort(a(ifirst:ilast),pivot="M")
         end do
         v = a(ifirst+n/2)
      case("quicksortT")
         do ilast = n, size(a), n
            ifirst = ilast - n + 1
            call quicksort(a(ifirst:ilast),pivot="T")
         end do
         v = a(ifirst+n/2)
      case("heapsort")
         do ilast = n, size(a), n
            ifirst = ilast - n + 1
            call heapsort(a(ifirst:ilast))
         end do
         v = a(ifirst+n/2)
         case("insertsort")
         if (real(n) * size(a) <= 1e10) then
            do ilast = n, size(a), n
               ifirst = ilast - n + 1
               call insertionsort(a(ifirst:ilast))
            end do
            v = a(ifirst+n/2)
         else
            computed = .false.
         end if
      case default; error stop "wrong test"
   end select
   call system_clock(toc,rate)
   time = huge(time)
   if (computed) time = ((toc-tic)/real(rate,kind=dp)) / real(ifirst+n-1,kind=dp)
    
   end subroutine testit

   !===========================================
   subroutine random_number2(x,p,vmax)
   !===========================================
   real, intent(out) :: x(:)
   integer, intent(in) :: p
   real, optional :: vmax
   
   integer, parameter :: chunk=4096
   integer :: i, l
   double precision :: a(chunk), c
   !===========================================
   c = 1d0; if (present(vmax)) c = vmax**(1.0/p)
   do i = 1, size(x), chunk
      l = min(chunk,size(x) - i + 1)
      call random_number(a)
      x(i:i+l-1) = (a(1:l) * c)**p
   end do
   end subroutine random_number2
   
   !===========================================
   subroutine random_number_gaussian(x,vmax)
   !===========================================
   real, intent(out) :: x(:)
   real, optional :: vmax
   
   integer, parameter :: chunk=4096
   integer :: i, j, l
   double precision :: a(chunk), b(chunk), c
   !===========================================
   c = 1 / 6d0; if (present(vmax)) c = c * vmax
   do i = 1, size(x), chunk
      l = min(chunk,size(x) - i + 1)
      call random_number(a)
      a(:) = 0d0
      do j = 1, 12
         call random_number(b)
         a(:) = a(:) + b(:)
      end do
      x(i:i+l-1) = (a(1:l) - 6d0) * c
   end do
   end subroutine random_number_gaussian

End Program