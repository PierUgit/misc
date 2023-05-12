!***********************************************************************************************
! Author: Pierre Hugonnet
! https://github.com/PierUgit
! License: GPL v3 
!***********************************************************************************************
! Various sorting and n-th element selection routines of/on a real array, in increasing order
! Sorting routines have partial sorting options.
! Selection routines modify the input array, except bisection_select()
!***********************************************************************************************

MODULE sortselect
implicit none

PRIVATE

integer, public :: &
   QUICKSORT_REVERT2HEAP             = 1000,  & ! revert to heapsort
   HEAPSORT_REVERT2INSERT            = 100,   & ! revert to insertionsort
   QUICKSELECT_REVERT2HEAP           = 100,   & ! revert to heapselect
   BISECSELECT_STRICT_REVERT2HEAP    = 100,   & ! revert to heapselect
   BISECSELECT_NONSTRICT_REVERT2HEAP = 20,    & ! revert to heapselect
   BISECSELECT_ALLOC_THRESHOLD       = 1000000
   
PUBLIC :: insertionsort, insertionselect, &
          heapsort, heapselect,           &
          quicksort, quickselect,         &
          bisectionselect,                &
          median5

CONTAINS   

   !*************************************************************************************
   subroutine insertionsort(a,partial)
   !*************************************************************************************
   ! classical algorithm...
   !*************************************************************************************
    real, intent(inout) :: a(:)
   integer, intent(in), optional :: partial
   
   integer :: n, i, j, k, l
   real :: t
   !*************************************************************************************
   n = size(a)
   k = n; if (present(partial)) k = partial
   
   ! Sort only the first k elements (=n if not partial sort)
   do l = 2, k 
       ! the first (l-1) elements are sorted, we are now looking at element l
      do j = 1, l-1
         if (a(l) < a(j)) then
            ! insertion of element l
            t = a(l)
            do i = l, j+1, -1
               a(i) = a(i-1)
            end do
            a(j) = t
            exit
         end if
      end do
   end do
   
   ! now inserting the remaining ones into the list 1:k
   do l = k+1, n
      do j = 1, k
         if (a(l) < a(j)) then
            ! insertion of element l
            t = a(l)
                a(l) = a(k)
            do i = k, j+1, -1
               a(i) = a(i-1)
            end do
            a(j) = t
            exit
         end if
      end do
   end do
   
   end subroutine insertionsort
   
         
   !*************************************************************************************
   real function insertionselect(a,k)
   !*************************************************************************************
   ! The k-th element is ensured to be in a(k). 
   ! The rest of the output list has no particular structure
   !*************************************************************************************
   real, intent(inout) :: a(:)
   integer, intent(in) :: k
   
   integer :: n, kk
   real :: t
   !*************************************************************************************
   n = size(a)
   
   kk = k
   if (kk > (n+1)/2) then
      kk = n - kk + 1
      a = -a
   end if
   call insertionsort(a,partial=kk)
   if (kk /= k) then
      a = -a
      t = a(k); a(k) = a(kk); a(kk) = t
   end if
   insertionselect = a(k)
   end function insertionselect


   !*************************************************************************************
   subroutine heapsort(a,partial)
   !*************************************************************************************
   ! classical algorithm...
   !*************************************************************************************
   real, intent(inout) :: a(:) 
   integer, intent(in), optional :: partial
   !*************************************************************************************
   if (size(a) <= HEAPSORT_REVERT2INSERT) then
      call insertionsort(a,partial)
      return
   end if
   
   if (present(partial)) then
      call heapsort_partial___(a,partial)
   else
      call heapsort___(a)
   end if
   
   end subroutine heapsort
   
   
   !*************************************************************************************
   subroutine heapsort___(a)
   !*************************************************************************************
   real, intent(inout) :: a(:) 
   integer :: n, i
   real :: t
   !*************************************************************************************
   n = size(a)

   ! create an initial max-heap
   do i = n/2, 1, -1
      call heapify(a,i)
   end do
   ! swapping the head with the last leave, detach the leave, and rebuild a max-heap
   ! until the heap vanishes
   do i = n, 1, -1
      t = a(1); a(1) = a(i); a(i) = t
      call heapify(a(1:i-1),1)
   end do
    
   end subroutine heapsort___
   

   !*************************************************************************************
   subroutine heapsort_partial___(a,k)
   !*************************************************************************************
   real, intent(inout) :: a(:) 
   integer, intent(in) :: k
   integer :: n, i
   real :: t
   !*************************************************************************************
   n = size(a)

   ! create an initial max-heap with the first k elements
   do i = k/2, 1, -1
      call heapify(a(1:k),i)
   end do
   ! possibly insert the remaining elements in the heap
    do i = k+1, n
        if (a(i) < a(1)) then
            t = a(1); a(1) = a(i); a(i) = t
          call heapify(a(1:k),1)
        end if
    end do    
    ! swapping the head with the last leave, detach the leave, and rebuild a max-heap
   ! until the heap vanishes
   do i = k, 1, -1
      t = a(1); a(1) = a(i); a(i) = t
      call heapify(a(1:i-1),1)
   end do
    
   end subroutine heapsort_partial___
    
        
   !*************************************************************************************
   real function heapselect(a,k)
   !*************************************************************************************
   ! The k-th element is ensured to be in a(k). 
   ! The rest of the output list has no particular structure
   !*************************************************************************************
   real, intent(inout) :: a(:) 
   integer, intent(in) :: k
   integer :: n, i, kk
   real :: t
   !*************************************************************************************
   n = size(a)

   kk = k
   if (kk > (n+1)/2) then   ! if k is large, it's better to select from the end
      kk = n - kk + 1
      a = -a
   endif

   ! create an initial max-heap with the first kk elements
    do i = kk/2, 1, -1
      call heapify(a(1:kk),i)
   end do
   ! possibly insert the remaining elements in the heap
   do i = kk+1, n
        if (a(i) < a(1)) then
            t = a(1); a(1) = a(i); a(i) = t
          call heapify(a(1:kk),1)
        end if
    end do

   if (kk /= k)  a = -a

   ! the k-th element is the head of the heap at the end
   t = a(k); a(k) = a(1); a(1) = t
   heapselect = a(k)

   end function heapselect


    !*************************************************************************************
    recursive subroutine heapify(a,i)
    !*************************************************************************************
    ! input: a max-heap where the i-th element has been lowered 
    !        (hence possibly no longer a max-heap)
    ! output: a max-heap
    !*************************************************************************************
    real, intent(inout) :: a(:)
    integer, intent(in) :: i
    integer :: n, largest, left, right
    real :: t
    !*************************************************************************************
    n = size(a)
    largest = i
    left = 2*i ; right = left + 1
    if (left <= n) then
       if (a(left) > a(largest)) largest = left;
       if (right <= n) then
          if (a(right) > a(largest)) largest = right
       end if
    end if
    if (largest /= i) then
       t = a(i); a(i) = a(largest); a(largest) = t 
       call heapify(a,largest)
    end if
    end subroutine heapify


   !*************************************************************************************
   subroutine quicksort(a,partial,pivot)
   !*************************************************************************************
   ! classical algorithm
   !*************************************************************************************
   real, intent(inout) :: a(:)
   integer, intent(in), optional :: partial
   character(len=1), intent(in), optional :: pivot
   character(len=1) :: pivot___
   !*************************************************************************************
   pivot___ = 'R'; 
   if (size(a) <= 5) pivot___ = 'C'
   if (present(pivot)) pivot___ = pivot
   if (pivot___ == 'c') pivot___ = 'C'
   if (pivot___ == 'r') pivot___ = 'R'
   if (pivot___ == 'm') pivot___ = 'M'
   if (pivot___ == 't') pivot___ = 'T'
   if (index('CRMT',pivot___) == 0) error stop "quicksort: wrong pivot method"      
   call quicksort___(a,pivot___,partial)
   end subroutine quicksort
   

   !*************************************************************************************
   real function quickselect(a,k,pivot)
   !*************************************************************************************
   ! The k-th element is ensured to be in a(k). 
   ! The rest of the output list is partioned wrt the k-th element
   !*************************************************************************************
   real, intent(inout) :: a(:)
   integer, intent(in) :: k
   character(len=1), intent(in), optional :: pivot
   character(len=1) :: pivot___
   !*************************************************************************************
   pivot___ = 'R'; 
   if (size(a) <= 5) pivot___ = 'C'
   if (present(pivot)) pivot___ = pivot
   if (pivot___ == 'c') pivot___ = 'C'
   if (pivot___ == 'r') pivot___ = 'R'
   if (pivot___ == 'm') pivot___ = 'M'
   if (pivot___ == 't') pivot___ = 'T'
   if (index('CRMT',pivot___) == 0) error stop "quicksort: wrong pivot method"      
   call quicksort___(a,pivot=pivot,select=k)
   quickselect = a(k)
   end function quickselect


   !*************************************************************************************
   recursive subroutine quicksort___(a,pivot,partial,select)
   !*************************************************************************************
   ! (significantly) Modified from:
   ! Author: t-nissie
   ! License: GPLv3
   ! Gist: https://gist.github.com/t-nissie/479f0f16966925fa29ea 
   !*************************************************************************************
   real, intent(inout) :: a(:)
   character(len=1), intent(in) :: pivot
   integer, intent(in), optional :: partial
   integer, intent(in), optional :: select
   real :: x, t
   integer n, i, j
   !*************************************************************************************
   n = size(a)
   
   if (present(select) .and. n <= QUICKSELECT_REVERT2HEAP) then
      t = heapselect(a,select)   ! t is not used per se
      return
   else if (.not.present(select) .and. n <= QUICKSORT_REVERT2HEAP) then
      call heapsort(a,partial)
      return
   end if
   
   if (pivot == 'M' .and. n > 5) then
      x = median5(a,.false.)
   else if (pivot == 'T' .and. n > 5) then
      x = median5(a,.true.)
   else if (index('MT',pivot) /= 0 .or. pivot == 'C') then
      x = a((n+1)/2)
   else
      call random_number(x)
      x = a( 1 + nint(x*(n-1)) )
   end if
             
   i = 1; j = n
   do
      do while (a(i) < x)
         i=i+1
      end do
      do while (x < a(j))
         j=j-1
      end do
      if (i >= j) exit
      t = a(i);  a(i) = a(j);  a(j) = t
      i=i+1; j=j-1
   end do
   if (present(select)) then
      if (1 < i-1 .and. select <= i-1) call quicksort___(a(1:i-1),pivot,select=select)
      if (j+1 < n .and. select >= j+1) call quicksort___(a(j+1:n),pivot,select=select-j)
   else if (present(partial)) then
      if (1 < i-1)                      call quicksort___(a(1:i-1),pivot,partial=partial)
      if (j+1 < n .and. partial >= j+1) call quicksort___(a(j+1:n),pivot,partial=partial-j)
   else
      if (1 < i-1) call quicksort___(a(1:i-1),pivot)
      if (j+1 < n) call quicksort___(a(j+1:n),pivot)
   end if
   end subroutine quicksort___
   
    
   !*************************************************************************************
   recursive function median5(a,transp) result(m5)
   !*************************************************************************************
   ! Approximate median
   !*************************************************************************************
   use misc
   real, intent(inout) :: a(:)
   logical, intent(in) :: transp
   real :: m5
   integer :: n, ifirst, ilast, n5

   !*************************************************************************************
   n = size(a)
   if (n <= 5) then
      m5 = insertionselect(a,(n+1)/2)
   else
      n5 = 0
      do ilast = 5, n, 5   ! the last chunk is ignored if not complete
         ifirst = ilast - 4
         n5 = n5 + 1
         call insertionsort(a(ifirst:ilast))
      end do
      if (transp) then
         call inplace_transpose(a,5,n5)
         m5 = median5(a(2*n5+1:3*n5),transp)
      else
         m5 = median5(a(3:3+5*(n5-1):5),transp)
      end if
   end if
     
   end function median5
     
     
   !*************************************************************************************
   real function bisectionselect(a,k,strict,alloc) result(val)
   !*************************************************************************************
   ! Finding the k-th value of a(:) in increasing order by bisection of a [v1;v2] interval,
   ! without modifying the input array, and mostly without sorting or copying the data 
   ! (see below)
   !
   ! strict (default=.true.):
   !   If .true., the returned value is necessarily in the input array, otherwise
   !   it is an arbitrary value such that count(a<=val) == k, potentially saving 
   !   some final recursion steps
   ! alloc (default=.false.)
   !   I true, when the number of elements in the current interval falls below 1/10th the input number
   !   input number of elements, these elements are copied to a new array.
   !   - Drawback: a bit more memory occupation and (limited) data duplication
   !   - Advantage: much less elements to count, and much more cache friendly
   !
   ! If the number of elements in the interval falls below a hard-coded threshold, 
   ! we stop the bisection and explicitly sort the remaining elements.
   ! 
   ! At each step we have count(a<=v1) < k <= count(a<=v2)
   !
   ! The complexity is O(n) because the recursion depth is bounded (there are 2**32 
   ! representable 32 bits reals, so as a rule of thumb 32 recursions needed at most)
   !*************************************************************************************
   real, intent(in)    :: a(:)
   integer, intent(in) :: k
   logical, intent(in),  optional :: strict
   logical, intent(in),  optional :: alloc

   real :: v1, v2
   integer :: n, k1, k2, i
   logical :: strict___, alloc___
   !*************************************************************************************
   n = size(a)
   
   ! Search for the min value v1 and max value v2 (faster than using minval/maxval)
   ! Generally in the code, k = count(a <= v)
   v1 = a(1)
   v2 = a(1)
   do i = 2, n
      if (a(i) >  v2) then
         v2 = a(i)
      else if (a(i) <  v1) then
         v1 = a(i)
      end if
   end do
   if (v1 /= -huge(a)) then
      v1 = nearest(v1, -1.0)
      k1 = 0
   else
      k1 = count(a <= v1)
   end if
   k2 = n
    
   ! trivial cases
   if (k <= k1) then
      val = v1
   else if (k == n) then
       val = v2
   else
      strict___ = .true.; if (present(strict)) strict___ = strict
      alloc___ = .false.; if (present(alloc)) alloc___ = alloc
      val = bisection___(a,k,strict___,alloc___,v1,v2,k1,k2)
   end if
    
   end function bisectionselect


   !*************************************************************************************
   real recursive function bisection___(a,k,strict,alloc,v1,v2,k1,k2) result(val)
   !*************************************************************************************
   real,    intent(in) :: a(:)
   integer, intent(in) :: k
   logical, intent(in) :: strict, alloc
   real,    value      :: v1, v2
   integer, value      :: k1, k2

   integer :: n, k0, i, j
   real :: v0, c
   real, allocatable :: b(:)
   !*************************************************************************************
   n = size(a)
   
   if ((strict .and. n <= BISECSELECT_STRICT_REVERT2HEAP) .or.          &
      (.not.strict .and. n <= BISECSELECT_NONSTRICT_REVERT2HEAP)) then
      b = a
      val = insertionselect(b,k)
      return
   end if
   
   ! Reduce the [v1,v2] interval by bisection
   v0 = safe_mean(v1,v2)
   if (v0 == v2 .or. v0 == v1) then 
      ! no real value exists between v1 and v2, end of the bisection
      val = v2
   else
      k0 = count(a <= v0)
      if (.not.strict .and. k0 == k) then
         ! v0 is not necessarily a value present in a(:)
         val = v0
      else
         if (k0 >= k) then
            v2 = v0; k2 = k0
         else
            v1 = v0; k1 = k0
         end if
         if (alloc .and. n >= BISECSELECT_ALLOC_THRESHOLD .and. k2-k1 <= n/10) then
            call extract(a,v1,v2,b,k2-k1)
            val = bisection___(b,k-k1,strict,alloc,v1,v2,0,k2-k1)
         else
            val = bisection___(a,k,strict,alloc,v1,v2,k1,k2)
         end if
      end if
   end if
    
   end function bisection___
          
   
   !*************************************************************************************   
   pure subroutine extract(a,v1,v2,b,m)
   !*************************************************************************************   
   real, intent(in) :: a(:), v1, v2
   real, allocatable, intent(out) :: b(:)
   integer, intent(in) :: m
   integer :: i, j
   !*************************************************************************************   
   allocate( b(m) )
   j = 0
   do i = 1, size(a)
      if (a(i) > v1 .and. a(i) <= v2) then
         j = j + 1
         b(j) = a(i)
      end if
   end do
   if (j /= m) error stop "size issue in extract()"
   end subroutine extract


   !*************************************************************************************   
   pure elemental function safe_mean(x,y) result(m)
   !*************************************************************************************   
   ! returns an approximation of m=(x+y)/2 without overflow/underflow and such that 
   ! * m = max(x,y) if y == x or if y == nearest(x,y-x)
   ! * min(x,y) < m < max(x,y) otherwise
   !*************************************************************************************
   integer, parameter :: wp = kind(1.0)   
   real(wp), intent(in) :: x, y
   real(wp) :: m
   real(wp) :: v1, v2, a, b
   !*************************************************************************************
   if (y == x) then
      m = y
   else if (y > x) then
      v1 = min(x,y)
      v2 = max(x,y)
      a = nearest(v1,1.0_wp)
      if (v2 == a) then
         m = v2
      else 
         if (v1 >= 0.0_wp) then
            m = v1 + (v2-v1)/2
         else if (v2 <= 0.0_wp) then
            m = v2 + (v1-v2)/2
         else
            m = (v1+v2)/2
         end if
         b = nearest(v2,-1.0_wp)
         m = min( max( m, a ), b)
      end if
   end if
   end function safe_mean
   
END MODULE sortselect
