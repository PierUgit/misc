!***********************************************************************************************
! Author: Pierre Hugonnet
! https://github.com/PierUgit
! License: GPL v3 
!***********************************************************************************************
! Implementation of a 1-bit logical array, stored in a default integer array
! Not cpu-efficient at all, but memory efficient
! Limitations:
! - 1D only
! - size limited to a default integer
!
! Example: 
! 
! type(bitfield_t) :: b
! call b%allocate(1000)    ! allocates 1000 1-bit logicals (lower bound is 1)
! call b%allocate(0,999)   ! allocates 1000 1-bit logicals with a lower bound = 0
! call b%set(.false.)      ! sets all the bits to .false.
! call b%set(100,.true.)   ! sets the bit index 100 to .true.
! call b%set(10,19,.true)  ! sets the bits indexes 10 to 19 to .true.
! v = b%get(200)           ! gets the value of bit index 200
! call b%get(200,v)        ! gets the value of bit index 200 and put in v
! call b%get(10,19,v)      ! gets the value of bit indexes 10 to 19 and put them in v(:)
! call b%get(v)            ! gets the value of all the bits and put them in v(:)
! call b%deallocate()      ! deallocates the bitfield
!***********************************************************************************************
module bitfield
!use iso_fortran_env
implicit none

private

integer, parameter :: l=bit_size(0)
integer, parameter :: zeros = 0
integer, parameter :: ones = not(zeros)

type, public :: bitfield_t
   integer, allocatable, private :: a(:)
   integer, private :: n = -1
   integer, private :: lb
contains
   procedure :: allocate1 => b_allocate1
   procedure :: allocate2 => b_allocate2
   generic :: allocate => allocate1, allocate2
   procedure :: deallocate => b_deallocate
   procedure :: getsize => b_getsize
   procedure :: getlb => b_getlb
   procedure :: getub => b_getub
   procedure :: setlb => b_setlb
   procedure :: setub => b_setub
   procedure, private :: set1 => b_set1
   procedure, private :: setall => b_setall
   procedure, private :: setrange => b_setrange
   generic   :: set => set1, setall, setrange
   procedure :: get => b_fget
   procedure, private :: get1 => b_get1
   procedure, private :: getall => b_getall
   procedure, private :: getrange => b_getrange
   generic :: gets => get1, getall, getrange
   procedure, private :: allall => b_allall
   procedure, private :: allrange => b_allrange
   generic :: all => allall, allrange
   procedure, private :: anyall => b_anyall
   procedure, private :: anyrange => b_anyrange
   generic :: any => anyall, anyrange
   procedure :: pull => b_pull
end type

contains

   subroutine b_allocate1(this,n)
   class(bitfield_t), intent(inout) :: this
   integer, intent(in) :: n
   this%n = n
   this%lb = 1
   allocate( this%a(0:(n-1)/l) )
   end subroutine 

   subroutine b_allocate2(this,lb,ub)
   class(bitfield_t), intent(inout) :: this
   integer, intent(in) :: lb, ub
   this%n = ub - lb + 1 
   this%lb = lb
   allocate( this%a(0:(this%n-1)/l) )
   end subroutine 

   subroutine b_deallocate(this)
   class(bitfield_t), intent(inout) :: this
   deallocate( this%a )
   this%n = -1
   end subroutine 

   integer function b_getsize(this)
   class(bitfield_t), intent(in) :: this
   b_getsize = this%n
   end function 
   
   integer function b_getlb(this)
   class(bitfield_t), intent(in) :: this
   b_getlb = this%lb
   end function 

   integer function b_getub(this)
   class(bitfield_t), intent(in) :: this
   b_getub = this%lb + this%n - 1
   end function 

   subroutine b_setlb(this,lb)
   class(bitfield_t), intent(inout) :: this
   integer, intent(in) :: lb
   this%lb = lb
   end subroutine 

   subroutine b_setub(this,ub)
   class(bitfield_t), intent(inout) :: this
   integer, intent(in) :: ub
   this%lb = ub - this%n + 1
   end subroutine 

   subroutine b_set1(this,i,v)
   class(bitfield_t), intent(inout) :: this
   integer, intent(in) :: i
   logical, intent(in) :: v
   integer :: ii, j
   call indeces(this%lb,i,j,ii)
   if (v) then
      this%a(j) = ibset(this%a(j),ii)
   else
      this%a(j) = ibclr(this%a(j),ii)
   end if
   end subroutine 

   subroutine b_setall(this,v)
   class(bitfield_t), intent(inout) :: this
   logical, intent(in) :: v
   integer :: a
   this%a(:) = merge(ones,zeros,v)
   end subroutine 

   subroutine b_setrange(this,istart,istop,v)
   class(bitfield_t), intent(inout) :: this
   integer, intent(in) :: istart, istop
   logical, intent(in) :: v
   integer :: a
   integer :: iistart, iistop, jstart, jstop
   if (istart > istop) return
   a = merge(ones,zeros,v)
   call indeces(this%lb,istart,jstart,iistart)
   call indeces(this%lb,istop ,jstop ,iistop)
   if (jstart == jstop) then
      call mvbits(a,0,istop-istart+1,this%a(jstart),iistart)
   else
      call mvbits(a,0,l-iistart,this%a(jstart),iistart)
      this%a(jstart+1:jstop-1) = a
      call mvbits(a,0,iistop,this%a(jstop),0)
   endif
   end subroutine 

   logical function b_fget(this,i)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: i
   integer :: ii, j
   call indeces(this%lb,i,j,ii)
   b_fget = btest(this%a(j),ii)
   end function 

   subroutine b_get1(this,i,v)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: i
   logical, intent(out) :: v
   v = this%get(i)
   end subroutine 

   subroutine b_getall(this,v)
   class(bitfield_t), intent(in) :: this
   logical, intent(out) :: v(:)
   call b_getrange(this,1,this%n,v)
   end subroutine 
   
   subroutine b_getrange(this,istart,istop,v)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: istart, istop
   logical, intent(out) :: v(:)
   integer :: i, j, ii, iistart, iistop, jstart, jstop
   if (istart > istop) return
   call indeces(this%lb,istart,jstart,iistart)
   call indeces(this%lb,istop ,jstop ,iistop)
   i = 0
   if (jstart == jstop) then
      do ii = iistart, iistop
         i = i+1
         v(i) = btest(this%a(jstart),ii)
      end do
   else
      do ii = iistart, l-1
         i = i+1
         v(i) = btest(this%a(jstart),ii)
      end do
      do j = jstart+1, jstop-1
         do ii = 0, l-1
            i = i+1
            v(i+1) = btest(this%a(j),ii)
         end do
      end do
      do ii = 0, iistop
         i = i+1
         v(i) = btest(this%a(jstop),ii)
      end do
   end if
   end subroutine 
   
   logical function b_allall(this)
   class(bitfield_t), intent(in) :: this
   b_allall = b_allrange(this,1,this%n)
   end function 

   logical function b_allrange(this,istart,istop) result(v)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: istart, istop
   integer :: i, j, ii, iistart, iistop, jstart, jstop
   v = .true.
   if (istart > istop) return
   call indeces(this%lb,istart,jstart,iistart)
   call indeces(this%lb,istop ,jstop ,iistop)
   if (jstart == jstop) then
      do ii = iistart, iistop
         v = v .and. btest(this%a(jstart),ii)
         if (.not.v) return
      end do
   else
      do ii = iistart, l-1
         v = v .and. btest(this%a(jstart),ii)
         if (.not.v) return
      end do
      do j = jstart+1, jstop-1
         v = v .and. this%a(j) == ones
         if (.not.v) return
      end do
      do ii = 0, iistop
         v = v .and. btest(this%a(jstop),ii)
         if (.not.v) return
      end do
   end if
   end function 

   logical function b_anyall(this)
   class(bitfield_t), intent(in) :: this
   b_anyall = b_anyrange(this,1,this%n)
   end function 

   logical function b_anyrange(this,istart,istop) result(v)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: istart, istop
   integer :: i, j, ii, iistart, iistop, jstart, jstop
   v = .false.
   if (istart > istop) return
   call indeces(this%lb,istart,jstart,iistart)
   call indeces(this%lb,istop ,jstop ,iistop)
   if (jstart == jstop) then
      do ii = iistart, iistop
         v = v .or. btest(this%a(jstart),ii)
         if (v) return
      end do
   else
      do ii = iistart, l-1
         v = v .or. btest(this%a(jstart),ii)
         if (v) return
      end do
      do j = jstart+1, jstop-1
         v = v .or. this%a(j) /= zeros
         if (v) return
      end do
      do ii = 0, iistop
         v = v .or. btest(this%a(jstop),ii)
         if (v) return
      end do
   end if
   end function 
   
   function b_pull(this,istart,istop,lb) result(that)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: istart, istop
   integer, intent(in), optional :: lb
   type(bitfield_t) :: that
   integer :: iistart, iistop, jstart, jstop, j, jdest
   if (istart > istop) return
   call that%allocate(istart,istop)
   if (present(lb)) call that%setlb(lb)
   call indeces(this%lb,istart,jstart,iistart)
   call indeces(this%lb,istop ,jstop ,iistop)
   if (jstart == jstop) then
      call mvbits(this%a(jstart),iistart,iistop-iistart+1,that%a(0),0)
   else
      call mvbits(this%a(jstart),iistart,l-iistart,that%a(0),0)
      jdest = 0
      do j = jstart+1, jstop-1
         call mvbits(this%a(j),0,iistart,that%a(jdest),l-iistart)
         jdest = jdest + 1
         call mvbits(this%a(j),iistart,l-iistart,that%a(jdest),0)
      end do
      call mvbits(this%a(jstop),0,iistart,that%a(jdest),l-iistart)
   end if
   end function 

   
   subroutine indeces(lb,i,j,ii)
   integer, intent(in) :: lb, i
   integer, intent(out) :: j, ii
   ii = i-lb ; j = ii/l ; ii = ii - j*l
   end subroutine

end module