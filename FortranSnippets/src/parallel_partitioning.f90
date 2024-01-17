!***********************************************************************************************
! Module containing routines for PARallel PARtionning of a real array
! ang program to test it
!***********************************************************************************************
module parpar_m
!***********************************************************************************************
use omp_lib
implicit none

private
public :: seqpar, parpar

integer, parameter :: BLOCK_SIZE = 1024
type block
   integer, pointer :: p(:) => null()
end type

contains

   !********************************************************************************************
   function seqpar(a,v) result(pivot)
   !********************************************************************************************
   ! Sequential partionning of the array a(:) into 3 partitions
   ! Partition 1: a(1:pivot(1))              all values <= v
   ! Partition 2: a(pivot(2): ) -            all values >= v
   ! The pivot value v can be input, or taken from the middle element of the array
   !********************************************************************************************
   integer, intent(inout) :: a(:)
   integer, optional, intent(in) :: v
   integer :: pivot(2)
   
   integer :: i, j, n, v1, t, k
   double precision :: tic, toc
   !********************************************************************************************
   n = size(a)
   if (.not.present(v)) then
      v1 = a((1+n)/2)
   else
      v1 = v
   end if
   
   ! Initial partionning in 2, there can be some overlap
   tic = omp_get_wtime()
   i = 1; j = n
   !print "(3I5,*(I3))", v1, i, j, a
   do
      do while (a(i) < v1)
         i=i+1
         if (i > n) exit
      end do
      do while (v1 < a(j))
         j=j-1
         if (j < 1) exit
      end do
      if (i >= j) exit
      t = a(i);  a(i) = a(j);  a(j) = t
      !print "(3I5,*(I3))", v1, i, j, a
      i=i+1; j=j-1
   end do
   pivot = [i-1,j+1]
   toc = omp_get_wtime()
   !print*, "          2-partionning:", toc-tic
      
   !print "(3I5,*(I3))", v0, pivot, a
   end function
   
   
   !********************************************************************************************
   function blockpar(b,v) result(pivot)
   !********************************************************************************************
   ! Same as seqpar(), but with the array a(:) reorganised in segments of size BLOCK_SIZE
   !********************************************************************************************
   type(block), intent(inout) :: b(:)
   integer, intent(in) :: v
   integer :: pivot(2)
   
   integer :: i, j, ib, jb, ii, jj, nb, n, t, v1
   integer, pointer :: pi(:), pj(:), pk(:)
   double precision :: tic, toc
   !********************************************************************************************
   nb = size(b)
   n = 0
   do ib = 1, size(b)
      n = n + size( b(ib)%p )
   end do

   v1 = v

   tic = omp_get_wtime()
   i = 0;   ib = 1;  pi => b(ib)%p; ii = 0
   j = n+1; jb = nb; pj => b(jb)%p; jj = size(pj)+1
   do
      do
         i = i+1; ii = ii+1
         if (ii > size(pi)) then
            if (i > n) exit
            ii = 1; ib=ib+1; pi => b(ib)%p
         end if
         if (pi(ii) >= v1) exit
      end do
      do
         j = j-1; jj = jj-1
         if (jj < 1) then
            if (j < 1) exit
            jj = BLOCK_SIZE; jb=jb-1; pj => b(jb)%p
         end if
         if (pj(jj) <= v1) exit
      end do
      if (i >= j) exit
      t = pi(ii);  pi(ii) = pj(jj);  pj(jj) = t
      !print "(3I5,*(I3))", v1, i, j, a
   end do
   pivot = [i-1,j+1]
   toc = omp_get_wtime()
   !print*, "          2-partionning:", toc-tic

   end function


   !********************************************************************************************
   function parpar(a,v,num_threads) result(pivot)
   !********************************************************************************************
   ! Parallel version of seqpar()
   !********************************************************************************************
   integer, intent(inout), target :: a(:)
   integer, optional, intent(in) :: v, num_threads
   integer :: pivot(2)

   type(block), allocatable, target :: b(:)
   type(block), pointer :: bb(:)
   integer :: i, j, n, v___, nb, nt, ntt, ib, it, imin, imax, ii, nn
   integer, allocatable :: ipivot(:,:), bpivot(:,:)
   double precision :: tic, toc
   !********************************************************************************************
   tic = omp_get_wtime()
   
   n = size(a)
   v___ = a((1+n)/2); if (present(v)) v___ = v
   
   nt = ognt()
   if (present(num_threads)) nt = min(nt,num_threads)
      
   nb = (n-1) / BLOCK_SIZE + 1
   allocate( b(nb) )
   i = 1
   do ib = 1, nb
      j = min(i+BLOCK_SIZE-1,n)
      b(ib)%p => a(i:j)
      i = j+1
      if (i > n) exit 
   end do
   
   ntt = min(nt, 1+(nb-1)/8)
            
   allocate( ipivot(2,ntt), bpivot(2,ntt) )
      
   toc = omp_get_wtime()
   !print*, "preparation", toc-tic
      
   !$OMP PARALLEL DO PRIVATE(bb,tic,toc)
   do it = 1, ntt
      !print*, it, "entering blockpar", size(bb)
      tic = omp_get_wtime()
      bb => b(it:nb:ntt)
      ipivot(:,it) = blockpar(bb,v___)
      toc = omp_get_wtime()
      print*, it, "leaving blockpar", toc-tic
   end do
   !$OMP END PARALLEL DO
                  
   if (ntt == 1) then
      pivot = ipivot(:,1)
      return
   end if
      
   do it = 1, ntt
      bb => b(it:nb:ntt)
      nn = 0
      do i = 1, size(bb)
         nn = nn + size(bb(i)%p)
      end do
      if (ipivot(1,it) == 0) then
         ! we are just before thread block 1
         call b2i(it,1,i)
         ipivot(1,it) = i-1
      else
         call i2b(ipivot(1,it),ib,ii)
         ib = (ib-1)*ntt + it
         call b2i(ib,ii,ipivot(1,it))
      end if
      if (ipivot(2,it) == nn+1) then
         ! we are just after last thread block
         ib = it
         do while (ib+ntt <= nb)
            ib = ib + ntt
         end do
         if (ib == nb) then
            ipivot(2,it) = n+1
         else
            call b2i(ib+1,1,i)
            ipivot(2,it) = i
         end if
      else
         call i2b(ipivot(2,it),ib,ii)
         ib = (ib-1)*ntt + it
         call b2i(ib,ii,ipivot(2,it))
      end if
   end do

   imin = minval(ipivot(1,:)) + 1
   imax = maxval(ipivot(2,:)) - 1
      
   print*, imax-imin+1, " remaining elements to partition sequentially"
            
   pivot = imin + seqpar(a(imin:imax),v___) - 1
         
   end function
   
   !********************************************************************************************
   subroutine i2b(i,ib,ii)
   !********************************************************************************************
   ! converts linear index into blocked indeces
   !********************************************************************************************
   integer, intent(in) :: i
   integer, intent(out) :: ib, ii
   !********************************************************************************************
   if (i == 0) then
      ib = 0
      ii = BLOCK_SIZE
      return
   end if
   ib = (i-1) / BLOCK_SIZE + 1
   ii = i - (ib-1)*BLOCK_SIZE
   end subroutine
   
   !********************************************************************************************
   subroutine b2i(ib,ii,i)
   !********************************************************************************************
   ! converts blocked indeces into linear index 
   !********************************************************************************************
   integer, intent(in) :: ib, ii
   integer, intent(out) :: i
   !********************************************************************************************
   i = (ib-1) * BLOCK_SIZE + ii
   end subroutine
   
   !********************************************************************************************
   integer function ognt()
   !********************************************************************************************
   ognt = 1
   !$OMP PARALLEL
   !$OMP SINGLE
   !$ ognt = omp_get_num_threads()
   !$OMP END SINGLE
   !$OMP END PARALLEL
   end function
   
end module


!***********************************************************************************************
program parpar_p
!***********************************************************************************************
use parpar_m
implicit none

integer, parameter :: n = 750000000
integer :: ipiv(2), v, nt, s, i
integer, allocatable :: a(:), a0(:), seed(:)
real, allocatable :: r(:)
double precision :: tic, toc
!***********************************************************************************************

allocate( r(n) )
call random_seed(size=s)
seed = [(i,i=1,s)]
call random_seed(put=seed)
call random_number(r)

a0 = n*r/10
deallocate(r)
!where (a0 == 50)
!   a0 = 0
!end where

print*, "Sequential routine"
a = a0
tic = omp_get_wtime()
ipiv = seqpar(a)
toc = omp_get_wtime()
print*, toc-tic
print*, ipiv
call checka()

do nt = 1, ognt()
   print*, "Parallel routine with", nt," threads"
   a = a0
   !print "(*(I3))", a
   tic = omp_get_wtime()
   ipiv = parpar(a,num_threads=nt)
   toc = omp_get_wtime()
   print*, toc-tic
   print*, ipiv
   call checka()
end do

contains

   subroutine checka()
   if (maxval(a(1:ipiv(1))) > minval(a(ipiv(2): ))) error stop "fail 1"
   !do i = 1, n
   !   if (count(a == a(i)) /= count(a0 == a(i))) error stop "fail 3"
   !end do
   end subroutine

end program

