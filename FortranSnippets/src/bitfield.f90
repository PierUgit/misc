!***********************************************************************************************
! Author: Pierre Hugonnet
! https://github.com/PierUgit
! License: GPL v3 
!***********************************************************************************************
! Implementation of a 1-bit logical array, stored in a default integer array
! Not cpu-efficient at all, but memory efficient
! No assumption is made on how the integers are internally represented
! Limitations:
! - 1D only
! - size limited to a default integer
!
! Example: 
! 
! type(bitfield_t) :: b
!
! call b%allocate(size)
! call b%allocate(lb,ub)
!     integer :: size, lb, ub
!
! call b%deallocate()
!
! n  = b%getsize()
! lb = b%getlb()
! ub = b%getub() 
! call b%setlb(lb)
! call b%setub(ub)
!     integer :: n, lb, ub
!
! call b%set(bool)
! call b%set(pos,bool) 
! call b%set(frompos,topos,bool) 
!     logical :: bool[(:)]
!     integer :: pos, frompos, topos
!     Note: b must always be allocated beforehand
!     Note: setting from a logical array is highly inefficient
!
! b = bool
!     type(bitfield_t) :: b
!     logical :: bool[(:)]
!     Note: allocation on assignement can occur if bool is rank 1
!
! call b%get(pos,bool)
!     logical :: bool
! call b%get(bool)
! call b%get(frompos,topos,bool)
!     logical :: bool(:)
!     integer :: pos, frompos, topos
!     Note: bool(:) must be allocated beforehand
!
! bool = b%fget(pos)
!     logical :: bool
! bool = b%fget()
! bool = b%fget(frompos,topos)
!     logical :: bool(:)
!     integer :: pos, frompos, topos
!     Note: bool(:) must be allocated beforehand
!
! bool = b
!     type(bitfield_t) :: b
!     logical, allocatable :: bool(:)
!     Note: works only for an allocatable LHS; allocation on assignement can occur
!
! call b%replace(c)
!     type(bitfield_t) :: c
!
! call b%extract(frompos,topos,c)
! c = b%fextract(frompos,topos)
!     integer :: frompos, topos
!     type(bitfield_t) :: c
!     Note: in the subroutine form, c must not be allocated beforehand
!
! n = b%count() 
! n = b%count(frompos,topos) 
!     integer :: frompos, topos
!
! bool = b%all()            
! bool = b%all(frompos,topos) 
!     integer :: frompos, topos
!
! bool = b%any()           
! bool = b%any(frompos,topos) 
!     integer :: frompos, topos
!***********************************************************************************************
module bitfield
!use iso_fortran_env
implicit none

private

public :: bitfield_t
public :: assignment(=)

integer, parameter :: l = bit_size(0)
integer :: zeros, ones
logical :: initialized = .false.

type :: bitfield_t
   private
   integer, allocatable :: a(:)
   integer :: n = -1
   integer :: lb = 1
   integer :: ub = 0
contains
   private
   procedure :: allocate1 => b_allocate1
   procedure :: allocate2 => b_allocate2
   generic, public :: allocate => allocate1, allocate2
   procedure, public :: deallocate => b_deallocate
   
   procedure, public :: getsize => b_getsize
   procedure, public :: getlb => b_getlb
   procedure, public :: getub => b_getub
   procedure, public :: setlb => b_setlb
   procedure, public :: setub => b_setub
   
   procedure :: set0 => b_set0
   procedure :: setall0 => b_setall0
   procedure :: setrange0 => b_setrange0
   procedure :: setall1 => b_setall1
   procedure :: setrange1 => b_setrange1
   generic, public:: set => set0, setall0, setall1, setrange0, setrange1
      
   procedure :: get1 => b_get1
   procedure :: getall => b_getall
   procedure :: getrange => b_getrange
   generic, public :: get => get1, getall, getrange
   
   procedure :: fget1 => b_fget1
   procedure :: fgetall => b_fgetall
   procedure :: fgetrange => b_fgetrange
   generic, public :: fget => fget1, fgetall, fgetrange
   
   procedure :: countall => b_countall
   procedure :: countrange => b_countrange
   generic, public :: count => countall, countrange
   
   procedure :: allall => b_allall
   procedure :: allrange => b_allrange
   generic, public  :: all => allall, allrange
   procedure :: anyall => b_anyall
   procedure :: anyrange => b_anyrange
   generic, public :: any => anyall, anyrange
   
   procedure, public :: extract => b_extract
   procedure, public :: fextract => b_fextract
   procedure, public :: replace => b_replace
   
   procedure, private :: indeces => b_indeces
end type

interface assignment(=)
   module procedure assign_l2b_0, assign_l2b_1, assign_b2l
end interface

contains

   subroutine init()
   integer :: ii
      zeros = 0
      do ii = 0, l
         zeros = ibclr(zeros,ii)
      end do
      ones = not(zeros)
   end subroutine

   subroutine b_allocate1(this,n)
   class(bitfield_t), intent(inout) :: this
   integer, intent(in) :: n
      call b_allocate2(this,1,n)
   end subroutine 

   subroutine b_allocate2(this,lb,ub)
   class(bitfield_t), intent(inout) :: this
   integer, intent(in) :: lb, ub
   integer :: ii
      if (.not.initialized) call init()
      if (allocated(this%a)) error stop "bitfield is already allocated"
      if (ub >= lb) then
         this%n = ub - lb + 1 
         this%lb = lb
         this%ub = ub
         allocate( this%a(0:(this%n-1)/l) )
      else
         this%n = 0 
         allocate( this%a(0) )
      end if
   end subroutine 

   subroutine b_deallocate(this)
   class(bitfield_t), intent(inout) :: this
      if (.not.allocated(this%a)) error stop "bitfield is not allocated"
      deallocate( this%a )
      this%n = -1
      this%lb = 1
      this%ub = 0
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
      b_getub = this%ub
   end function 

   subroutine b_setlb(this,lb)
   class(bitfield_t), intent(inout) :: this
   integer, intent(in) :: lb
      if (this%n > 0) then
         this%lb = lb
         this%ub = lb + this%n -1
      end if
   end subroutine 

   subroutine b_setub(this,ub)
   class(bitfield_t), intent(inout) :: this
   integer, intent(in) :: ub
      if (this%n > 0) then
         this%lb = ub - this%n + 1
         this%ub = ub
      end if
   end subroutine 
   
   

   subroutine b_set0(this,i,v)
   class(bitfield_t), intent(inout) :: this
   integer, intent(in) :: i
   logical, intent(in) :: v
   integer :: ii, j
      ! no runtime check, as it would hurt the performances for a single bit set
      call this%indeces(i,j,ii)
      if (v) then
         this%a(j) = ibset(this%a(j),ii)
      else
         this%a(j) = ibclr(this%a(j),ii)
      end if
   end subroutine 

   subroutine b_setall0(this,v)
   class(bitfield_t), intent(inout) :: this
   logical, intent(in) :: v
      if (.not.allocated(this%a)) error stop "b_setall0: bitfield is not allocated"
      this%a(:) = merge(ones,zeros,v)
   end subroutine 

   subroutine b_setrange0(this,istart,istop,v)
   class(bitfield_t), intent(inout) :: this
   integer, intent(in) :: istart, istop
   logical, intent(in) :: v
   integer :: a
   integer :: iistart, iistop, jstart, jstop
      if (istart > istop) return
      if (.not.allocated(this%a)) error stop "b_setrange0: bitfield is not allocated"
      a = merge(ones,zeros,v)
      call this%indeces(istart,jstart,iistart)
      call this%indeces(istop ,jstop ,iistop)
      if (jstart == jstop) then
         call mvbits(a,0,istop-istart+1,this%a(jstart),iistart)
      else
         call mvbits(a,0,l-iistart,this%a(jstart),iistart)
         this%a(jstart+1:jstop-1) = a
         call mvbits(a,0,iistop+1,this%a(jstop),0)
      endif
   end subroutine 
   
   subroutine b_setall1(this,v)
   class(bitfield_t), intent(inout) :: this
   logical, intent(in) :: v(:)
      call b_setrange1(this,this%lb,this%ub,v)
   end subroutine 

   subroutine b_setrange1(this,istart,istop,v)
   class(bitfield_t), intent(inout) :: this
   integer, intent(in) :: istart, istop
   logical, intent(in) :: v(:)
   integer :: ii, j, i, iistart, iistop, jstart, jstop
      if (istart > istop) return
      if (.not.allocated(this%a)) error stop "b_setrang1: bitfield is not allocated"
      if (istart < this%lb .or. istart > this%ub .or. istop < this%lb .or. istop > this%ub) &
         error stop "b_setrange1(): out of bound indeces" 
      call this%indeces(istart,jstart,iistart)
      call this%indeces(istop ,jstop ,iistop)
      i = 0
      if (jstart == jstop) then
         do ii = iistart, iistop
            i = i+1
            if (v(i)) then ; this%a(jstart) = ibset(this%a(jstart),ii)
                      else ; this%a(jstart) = ibclr(this%a(jstart),ii)
            end if
         end do
      else
         do ii = iistart, l-1
            i = i+1
            if (v(i)) then ; this%a(jstart) = ibset(this%a(jstart),ii)
                      else ; this%a(jstart) = ibclr(this%a(jstart),ii)
            end if
         end do
         do j = jstart+1, jstop-1
            do ii = 0, l-1
               i = i+1
               if (v(i)) then ; this%a(j) = ibset(this%a(j),ii)
                         else ; this%a(j) = ibclr(this%a(j),ii)
               end if
            end do
         end do
         do ii = 0, iistop
            i = i+1
            if (v(i)) then ; this%a(jstop) = ibset(this%a(jstop),ii)
                      else ; this%a(jstop) = ibclr(this%a(jstop),ii)
            end if
         end do
      endif
   end subroutine 

   subroutine assign_l2b_0(this,v)
   class(bitfield_t), intent(inout) :: this
   logical, intent(in) :: v
      call b_setall0(this,v)
   end subroutine 
   
   subroutine assign_l2b_1(this,v)
   class(bitfield_t), intent(inout) :: this
   logical, intent(in) :: v(:)
      if (allocated(this%a) .and. this%getsize() /= size(v)) call this%deallocate()
      if (.not.allocated(this%a)) call b_allocate1(this,size(v))
      call b_setall1(this,v)
   end subroutine 

   

   subroutine b_get1(this,i,v)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: i
   logical, intent(out) :: v
   integer :: j, ii
      call this%indeces(i,j,ii)
      v = btest(this%a(j),ii)
   end subroutine 
   
   subroutine b_getall(this,v)
   class(bitfield_t), intent(in) :: this
   logical, intent(out) :: v(:)
      if (this%getsize() /= size(v)) error stop "b_getall(): the sizes differ" 
      call b_getrange(this,this%lb,this%ub,v)
   end subroutine 
   
   subroutine b_getrange(this,istart,istop,v)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: istart, istop
   logical, intent(out) :: v(:)
   integer :: i1, i2, j, ii, iistart, iistop, jstart, jstop
   integer, allocatable :: iir(:)
      if (istart > istop) return
      if (istart < this%lb .or. istart > this%ub .or. istop < this%lb .or. istop > this%ub) &
         error stop "b_setrange1(): out of bound indeces" 
      call this%indeces(istart,jstart,iistart)
      call this%indeces(istop ,jstop ,iistop)
      i1 = 1
      if (jstart == jstop) then
         iir = [(ii,ii=iistart,iistop)]
         i2 = i1+iistop-iistart
         v(i1:i2) = btest(this%a(jstart),iir)
         i1 = i2+1
      else
         iir = [(ii,ii=iistart,l-1)]
         i2 = i1+l-1-iistart
         v(i1:i2) = btest(this%a(jstart),iir)
         i1 = i2+1
         
         iir = [(ii,ii=0,l-1)]
         do j = jstart+1, jstop-1
            i2 = i1+l-1
            v(i1:i2) = btest(this%a(j),iir)
            i1 = i2+1
         end do
         
         iir = [(ii,ii=0,iistop)]
         i2 = i1+iistop
         v(i1:i2) = btest(this%a(jstop),iir)
      end if
   end subroutine 
         
   function b_fget1(this,i) result(v)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: i
   logical :: v
      call b_get1(this,i,v)
   end function 

   function b_fgetall(this) result(v)
   class(bitfield_t), intent(in) :: this
   logical, allocatable:: v(:)
      allocate( v(this%getlb():this%getub()) )
      call b_getall(this,v)
   end function 

   function b_fgetrange(this,istart,istop) result(v)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: istart, istop
   logical, allocatable :: v(:)
      allocate( v(istart:istop) )
      call b_getrange(this,istart,istop,v)   
   end function

   subroutine assign_b2l(v,this)
   logical, allocatable, intent(out) :: v(:)
   class(bitfield_t), intent(in) :: this
      if (allocated(v) .and. this%getsize() /= size(v)) deallocate(v)
      if (.not.allocated(v)) allocate( v(this%getsize()) )
      call b_getall(this,v)
   end subroutine 


   
   subroutine b_replace(this,istart,inc,that)
   class(bitfield_t), intent(inout) :: this
   integer, intent(in) :: istart, inc
   type(bitfield_t), intent(in) :: that
   integer :: istop, iistart, iistop, jstart, jstop, j, jsource, iisource
      if (that%getsize() <= 0) return
      istop = istart + inc*(that%getsize()-1)
      if (istart < this%lb .or. istart > this%ub .or. istop < this%lb .or. istop > this%ub)      &
         error stop "b_replace(): out of bound bounds" 
      call this%indeces(istart,jstart,iistart)
      call this%indeces(istop,jstop ,iistop)
      if (inc == 1) then
         if (jstart == jstop) then
            call mvbits(that%a(0),0,iistop-iistart+1,this%a(jstart),iistart)
         else
            call mvbits(that%a(0),0,l-iistart,this%a(jstart),iistart)
            jsource = 0
            do j = jstart+1, jstop-1
               call mvbits(that%a(jsource),l-iistart,iistart,this%a(j),0)
               jsource = jsource + 1
               call mvbits(that%a(jsource),0,l-iistart,this%a(j),iistart)
            end do
            call mvbits(that%a(jsource),l-iistart,iistart,this%a(jstop),0)
         end if
      else
         j = jstart
         ii = iistart
         jsource = 0
         iisource = 0
         do i = istart, istop, inc
            if (that%a(jsource),iisource) then ; this%a(j) = ibset(this%a(j),ii)
                                          else ; this%a(j) = ibclr(this%a(j),ii)
            end if
            ii = ii + inc
            do 
               if (ii < l) exit
               ii = ii - l
               j = j + 1
            end do 
            do
               if (ii >= 0) exit
               ii = ii + l
               j = j - 1
            end do
            iisource = iisource + 1
            if (iisource == l) then
               iidest = 0
               jsource = jsource + 1
            end if
         end do
      end if
   end subroutine 



   subroutine b_extract(this,istart,istop,inc,that)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: istart, istop, inc
   type(bitfield_t), intent(inout) :: that
   integer :: iistart, iistop, jstart, jstop, j, jdest, iidest, n
      if ((istop-istart)*inc < 0) return
      if (istart < this%lb .or. istart > this%ub .or. istop  < this%lb .or. istop  > this%ub) &
         error stop "b_extract(): out of bound indeces" 
      if (allocated(that%a)) error stop "b_pull: destination is already allocated"
      n = (istop-istart)/inc + 1
      call that%allocate(n)
      call this%indeces(istart,jstart,iistart)
      call this%indeces(istop ,jstop ,iistop)
      if (inc == 1) then
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
      else
         j = jstart
         ii = iistart
         jdest = 0
         iidest = 0
         do i = istart, istop, inc
            if (btest(j,ii)) then ; that%a(jdest) = ibset(that%a(jdest),iidest)
                             else ; that%a(jdest) = ibclr(that%a(jdest),iidest)
            end if
            ii = ii + inc
            do 
               if (ii < l) exit
               ii = ii - l
               j = j + 1
            end do 
            do
               if (ii >= 0) exit
               ii = ii + l
               j = j - 1
            end do
            iidest = iidest + 1
            if (iidest == l) then
               iidest = 0
               jdest = jdest + 1
            end if
         end do
      end if
   end subroutine 
   
   function b_fextract(this,istart,istop,inc) result(that)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: istart, istop
   type(bitfield_t) :: that
      call b_extract(this,istart,istop,inc,that)
   end function
   
      
      


   logical function b_allall(this)
   class(bitfield_t), intent(in) :: this
      b_allall = b_allrange(this,this%lb,this%ub)
   end function 

   logical function b_allrange(this,istart,istop) result(v)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: istart, istop
   integer :: i, j, ii, iistart, iistop, jstart, jstop
   integer, allocatable :: iir(:)
      v = .true.
      if (istart > istop) return
      call this%indeces(istart,jstart,iistart)
      call this%indeces(istop ,jstop ,iistop)
      if (jstart == jstop) then
         iir = [(ii,ii=iistart,iistop)]
         v = v .and. all(btest(this%a(jstart),iir))
         if (.not.v) return
      else
         iir = [(ii,ii=iistart,l-1)]
         v = v .and. all(btest(this%a(jstart),iir))
         if (.not.v) return

         do j = jstart+1, jstop-1
            v = v .and. this%a(j) == ones
            if (.not.v) return
         end do
         
         iir = [(ii,ii=0,iistop)]
         v = v .and. all(btest(this%a(jstop),iir))
         if (.not.v) return
      end if
   end function 

   logical function b_anyall(this)
   class(bitfield_t), intent(in) :: this
      b_anyall = b_anyrange(this,this%lb,this%ub)
   end function 

   logical function b_anyrange(this,istart,istop) result(v)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: istart, istop
   integer :: i, j, ii, iistart, iistop, jstart, jstop
   integer, allocatable :: iir(:)
      v = .false.
      if (istart > istop) return
      call this%indeces(istart,jstart,iistart)
      call this%indeces(istop ,jstop ,iistop)
      if (jstart == jstop) then
         iir = [(ii,ii=iistart,iistop)]
         v = v .or. any(btest(this%a(jstart),iir))
         if (v) return
      else
         iir = [(ii,ii=iistart,l-1)]
         v = v .or. any(btest(this%a(jstart),iir))
         if (v) return

         do j = jstart+1, jstop-1
            v = v .or. this%a(j) /= zeros
            if (v) return
         end do
         
         iir = [(ii,ii=0,iistop)]
         v = v .or. any(btest(this%a(jstop),iir))
         if (v) return
      end if
   end function 
   
   

   integer function b_countall(this) result(v)
   class(bitfield_t), intent(in) :: this
      v = b_countrange(this,this%lb,this%ub)
   end function 

   integer function b_countrange(this,istart,istop) result(v)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: istart, istop
   integer :: i, j, ii, jj, iistart, iistop, jstart, jstop
   integer, allocatable :: iir(:)
      v = 0
      if (istart > istop) return
      call this%indeces(istart,jstart,iistart)
      call this%indeces(istop ,jstop ,iistop)
      if (jstart == jstop) then
         iir = [(ii,ii=iistart,iistop)]
         v = v + count(btest(this%a(jstart),iir))
      else
         iir = [(ii,ii=iistart,l-1)]
         v = v + count(btest(this%a(jstart),iir))

         do j = jstart+1, jstop-1
            v = v + popcnt(this%a(j))
         end do
         
         iir = [(ii,ii=0,iistop)]
         v = v + count(btest(this%a(jstop),iir))
      end if
   end function 
   
   
   
   subroutine b_indeces(this,i,j,ii)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: i
   integer, intent(out) :: j, ii
      ii = i-this%lb ; j = ii/l ; ii = ii - j*l
   end subroutine

end module