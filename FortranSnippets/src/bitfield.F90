#ifndef DEBUG
#define _PURE_ pure
#else
#define _PURE_
#endif

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
! call b%set(bool)              ! efficient if bool is a scalar
! call b%set(pos,bool)          ! not efficient
! call b%set(from,to,inc,bool)  ! efficient if bool is a scalar and |inc|==1
!     logical :: bool[(:)]
!     integer :: pos, from, top, inc
!     Note: b must always be allocated beforehand
!     Note: setting from a logical array is highly inefficient
!
! b = bool                      ! efficient if bool is a scalar
!     type(bitfield_t) :: b
!     logical :: bool[(:)]
!     Note: allocation on assignement can occur if bool is rank 1
!
! call b%get(pos,bool)          ! not efficient
!     logical :: bool           
! call b%get(bool)              ! not efficient
! call b%get(from,to,inc,bool)  ! not efficient
!     logical :: bool(:)
!     integer :: pos, frompos, topos
!     Note: bool(:) must be allocated beforehand
!
! bool = b%fget(pos)            ! not efficient
!     logical :: bool
! bool = b%fget()               ! not efficient
! bool = b%fget(from,to,inc)    ! not efficient
!     logical :: bool(:)
!     integer :: pos, from, top, inc
!     Note: bool(:) must be allocated beforehand
!
! bool = b                      ! not efficient
!     type(bitfield_t) :: b
!     logical, allocatable :: bool(:)
!     Note: works only for an allocatable LHS; allocation on assignement can occur
!
! call b%replace(from,to,inc,c) ! efficient if inc==1
!     integer :: from, to, inc
!     type(bitfield_t) :: c
!
! call b%extract(from,to,inc,c) ! efficient if inc==1
! c = b%fextract(from,to,inc)   ! efficient if inc==1
!     integer :: from, to, inc
!     type(bitfield_t) :: c
!     Note: in the subroutine form, c must not be allocated beforehand
!
! n = b%count()                 ! efficient
! n = b%count(from,top,inc)     ! efficient if |inc|==1
!     integer :: from, to, inc
!
! bool = b%all()                ! efficient
! bool = b%all(from,to,inc)     ! efficient if |inc|==1
!     integer :: from, to, inc
!
! bool = b%any()                ! efficient
! bool = b%any(from,to,inc)     ! efficient if |inc|==1
!     integer :: from, to, inc
!***********************************************************************************************
module bitfield
!use iso_fortran_env
implicit none

   private

   public :: bitfield_t, bitfield_check
   public :: assignment(=), operator(==), operator(/=)
   public :: operator(.not.), operator(.and.), operator(.or.)
   public :: operator(.eqv.), operator(.neqv.)

   integer, parameter :: ik = selected_int_kind(r=18)
   integer, parameter :: l = bit_size(0_ik)
   integer, parameter :: l2l = nint(log(real(l))/log(2.0))
   integer, parameter :: minbatch = 10
   integer(ik), parameter :: zeros = 0
   integer(ik), parameter :: ones = not(zeros)

   type :: bitfield_t
      private
      integer(ik), allocatable :: a(:)
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
      
      procedure :: get0 => b_get0
      procedure :: getall => b_getall
      procedure :: getrange => b_getrange
      generic, public :: get => get0, getall, getrange
   
      procedure :: fget0 => b_fget0
      procedure :: fgetall => b_fgetall
      procedure :: fgetrange => b_fgetrange
      generic, public :: fget => fget0, fgetall, fgetrange
   
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
   end type

   interface assignment(=)
      module procedure assign_b2b, assign_l2b_0, assign_l2b_1, assign_b2l
   end interface

   interface operator(.not.)
      module procedure b_negate
   end interface
   interface operator(.and.)
      module procedure b_and
   end interface
   interface operator(.or.)
      module procedure b_or
   end interface
   interface operator(.eqv.)
      module procedure b_eqv
   end interface
   interface operator(.neqv.)
      module procedure b_neqv
   end interface
   interface operator(==)
      module procedure b_equal
   end interface
   interface operator(/=)
      module procedure b_notequal
   end interface

contains

   logical function bitfield_check() result(stat)
      integer :: ii
      
      stat = .true.
      do ii = 0, l-1
         stat = stat .and. btest( ones, ii )
      end do
      stat = stat .and. shiftr(101,1) == 50 .and. shiftl(101,1) == 202
   end function
   
   _PURE_ subroutine b_allocate1(this,n)
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: n
      
      call b_allocate2(this,1,n)
   end subroutine 

   _PURE_ subroutine b_allocate2(this,lb,ub)
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: lb, ub
            
      if (allocated(this%a)) error stop "bitfield is already allocated"
      if (ub >= lb) then
         this%n = ub - lb + 1 
         this%lb = lb
         this%ub = ub
         allocate( this%a(0:(this%n-1)/l) )
         call clear_end(this)
      else
         this%n = 0 
         allocate( this%a(0) )
      end if
   end subroutine 

   _PURE_ subroutine b_deallocate(this)
      class(bitfield_t), intent(inout) :: this
      
      if (.not.allocated(this%a)) error stop "bitfield is not allocated"
      deallocate( this%a )
      this%n = -1
      this%lb = 1
      this%ub = 0
   end subroutine 
   


   integer _PURE_ function b_getsize(this)
      class(bitfield_t), intent(in) :: this
      
      b_getsize = this%n
   end function 
   
   integer _PURE_ function b_getlb(this)
      class(bitfield_t), intent(in) :: this
      
      b_getlb = this%lb
   end function 

   integer _PURE_ function b_getub(this)
      class(bitfield_t), intent(in) :: this
      
      b_getub = this%ub
   end function 

   _PURE_ subroutine b_setlb(this,lb)
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: lb
      
      if (this%n > 0) then
         this%lb = lb
         this%ub = lb + this%n -1
      end if
   end subroutine 

   _PURE_ subroutine b_setub(this,ub)
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: ub
      
      if (this%n > 0) then
         this%lb = ub - this%n + 1
         this%ub = ub
      end if
   end subroutine 
   
   
   
   _PURE_ subroutine assign_b2b(this,that)
      class(bitfield_t), intent(inout) :: this
      type(bitfield_t), intent(inout) :: that
      
      if (allocated(this%a) .and. this%getsize() /= that%getsize()) call b_deallocate(this)
      if (.not.allocated(this%a)) call b_allocate1(this,that%getsize())
      this%a(:) = that%a(:)
   end subroutine 
   
   

   _PURE_ subroutine b_set0(this,i,v)
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: i
      logical, intent(in) :: v
      
      integer :: ii, j
      
      ! no runtime check, as it would hurt the performances for a single bit set
      call indeces(this,i,j,ii)
      if (v) then
         this%a(j) = ibset(this%a(j),ii)
      else
         this%a(j) = ibclr(this%a(j),ii)
      end if
   end subroutine 

   _PURE_ subroutine b_setall0(this,v)
      class(bitfield_t), intent(inout) :: this
      logical, intent(in) :: v
      
      if (.not.allocated(this%a)) error stop "b_setall0: bitfield is not allocated"
      this%a(:) = merge(ones,zeros,v)
   end subroutine 

   _PURE_ recursive subroutine b_setrange0(this,istart,istop,inc,v)
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: istart, istop, inc
      logical, intent(in) :: v
      
      integer(ik) :: a
      integer :: iistart, iistop, jstart, jstop, i, j, k
      integer :: iir(l), iirs
      
      if (inc < 0) then
         call b_setrange0(this,istop+mod(istart-istop,-inc),istart,-inc,v)
         return
      end if
      
      if (.not.allocated(this%a)) error stop "b_setrange0: bitfield is not allocated"
      if (istart < this%lb .or. istart > this%ub .or. istop < this%lb .or. istop > this%ub) &
         error stop "b_setrange0(): out of bound indeces" 
      if (istop < istart) return
      
      if (inc == 1) then
         a = merge(ones,zeros,v)
         call indeces(this,istart,jstart,iistart)
         call indeces(this,istop ,jstop ,iistop)
         if (jstart == jstop) then
            call mvbits( a, 0, istop-istart+1, this%a(jstart), iistart )
         else
            call mvbits( a, 0, l-iistart, this%a(jstart), iistart )
            this%a(jstart+1:jstop-1) = a
            call mvbits(a,0,iistop+1,this%a(jstop),0)
         endif
      else if (inc <= l/minbatch) then
         call indeces(this,istart,jstart,iistart)
         call indeces(this,istop ,jstop ,iistop)
         j = jstart
         iirs = 0
         do
            call getiirs(jstart,jstop,iistart,iistop,inc,j,iir,iirs)  
            a = this%a(j)
            if (v) then ; do k = 1, iirs ; a = ibset(a,iir(k)) ; end do
                   else ; do k = 1, iirs ; a = ibclr(a,iir(k)) ; end do
            end if
            this%a(j) = a
            if (j == jstop) exit
         end do
      else
         do i = istart, istop, inc
            call b_set0(this,i,v)
         end do
      end if
   end subroutine 
   
   _PURE_ subroutine b_setall1(this,v)
      class(bitfield_t), intent(inout) :: this
      logical, intent(in) :: v(:)
      
      call b_setrange1(this,this%lb,this%ub,1,v)
   end subroutine 

   _PURE_ subroutine b_setrange1(this,istart,istop,inc,v)
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: istart, istop, inc
      logical, intent(in) :: v(:)
      
      integer :: k, j, i, iistart, iistop, jstart, jstop, iv
      integer :: iir(l), iirs
      integer(ik) :: a
      
      if (.not.allocated(this%a)) error stop "b_setrange1: bitfield is not allocated"
      if (istart < this%lb .or. istart > this%ub .or. istop < this%lb .or. istop > this%ub) &
         error stop "b_setrange1(): out of bound indeces" 
         
      iv = 0
      do i = istart, istop, inc
         iv = iv+1
         call b_set0( this, i, v(iv) )
      end do
   end subroutine 

   _PURE_ subroutine assign_l2b_0(this,v)
      class(bitfield_t), intent(inout) :: this
      logical, intent(in) :: v
      
      call b_setall0(this,v)
   end subroutine 
   
   _PURE_ subroutine assign_l2b_1(this,v)
      class(bitfield_t), intent(inout) :: this
      logical, intent(in) :: v(:)
      
      if (allocated(this%a) .and. this%getsize() /= size(v)) call b_deallocate(this)
      if (.not.allocated(this%a)) call b_allocate1(this,size(v))
      call b_setall1(this,v)
   end subroutine 

   

   _PURE_ subroutine b_get0(this,i,v)
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: i
      logical, intent(out) :: v
      
      integer :: j, ii
      
      call indeces(this,i,j,ii)
      v = btest(this%a(j),ii)
   end subroutine 
   
   _PURE_ subroutine b_getall(this,v)
      class(bitfield_t), intent(in) :: this
      logical, intent(out) :: v(:)
      
      if (this%getsize() /= size(v)) error stop "b_getall(): the sizes differ" 
      call b_getrange(this,this%lb,this%ub,1,v)
   end subroutine 
   
   _PURE_ subroutine b_getrange(this,istart,istop,inc,v)
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: istart, istop, inc
      logical, intent(out) :: v(:)
      
      integer :: i1, i2, j, iistart, iistop, jstart, jstop, i, iv
      integer :: iir(l), iirs
      
      if (sign(1,istop-istart)*sign(1,inc) < 0) return
      if (istart < this%lb .or. istart > this%ub .or. istop < this%lb .or. istop > this%ub) &
         error stop "b_getrange1(): out of bound indeces" 

      if (0 < inc .and. inc <= l/minbatch) then
         call indeces( this, istart, jstart, iistart)
         call indeces( this, istop , jstop , iistop)
         j = jstart
         i1 = 1
         iirs = 0
         do
            call getiirs(jstart,jstop,iistart,iistop,inc,j,iir,iirs)
            i2 = i1+iirs-1
            v(i1:i2) = btest(this%a(j),iir(1:iirs))
            if (j == jstop) exit
            i1 = i2+1
         end do
      else if (0 < -inc .and. -inc <= l/minbatch) then
         call indeces( this, istart,                       jstart, iistart )
         call indeces( this, istop+mod(istart-istop,-inc), jstop,  iistop  )
         j = jstop
         i1 = size(v)
         iirs = 0
         do
            call getiirs(jstop,jstart,iistop,iistart,-inc,j,iir,iirs)
            i2 = i1-iirs+1
            v(i1:i2:-1) = btest(this%a(j),iir(1:iirs))
            if (j == jstart) exit
            i1 = i2-1
         end do
      else
         iv = 0
         do i = istart, istop, inc
            iv = iv+1
            call b_get0(this,i,v(iv))
         end do
      end if
   end subroutine 
         
   _PURE_ function b_fget0(this,i) result(v)
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: i
      logical :: v
      
      call b_get0(this,i,v)
   end function 

   _PURE_ function b_fgetall(this) result(v)
      class(bitfield_t), intent(in) :: this
      logical, allocatable:: v(:)
      
      allocate( v(this%getlb():this%getub()) )
      call b_getall(this,v)
   end function 

   _PURE_ function b_fgetrange(this,istart,istop,inc) result(v)
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: istart, istop, inc
      logical, allocatable :: v(:)
      
      integer :: n
      
      n = abs((istop-istart)/inc+1)
      allocate( v(n) )
      call b_getrange(this,istart,istop,inc,v)   
   end function

   _PURE_ subroutine assign_b2l(v,this)
      logical, allocatable, intent(out) :: v(:)
      class(bitfield_t), intent(in) :: this
      
      if (allocated(v) .and. this%getsize() /= size(v)) deallocate(v)
      if (.not.allocated(v)) allocate( v(this%getsize()) )
      call b_getall(this,v)
   end subroutine 


   
   _PURE_ subroutine b_replace(this,istart,istop,inc,that)
      class(bitfield_t), intent(inout) :: this
      integer, intent(in) :: istart, istop, inc
      type(bitfield_t), intent(in) :: that
      
      integer :: k, i, iistart, iistop, jstart, jstop, j, jsource, iisource, isource
      integer :: iir(l), iirs
      
      if (that%getsize() <= 0) return
      if (istart < this%lb .or. istart > this%ub .or. istop < this%lb .or. istop > this%ub) &
         error stop "b_replace(): out of bound bounds" 
      call indeces(this,istart,jstart,iistart)
      call indeces(this,istop,jstop ,iistop)
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
         isource = 0
         do i = istart, istop, inc
            isource = isource + 1
            call b_set0( this, i, b_fget0(that,isource) )
         end do      
      end if
   end subroutine 



   _PURE_ subroutine b_extract(this,istart,istop,inc,that)
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: istart, istop, inc
      type(bitfield_t), intent(inout) :: that
      
      integer :: k, i, iistart, iistop, jstart, jstop, j, jdest, iidest, n, idest
      integer :: iir(l), iirs
      logical :: v(l)
      
      if (istart < this%lb .or. istart > this%ub .or. istop  < this%lb .or. istop  > this%ub) &
         error stop "b_extract(): out of bound indeces" 
      if (allocated(that%a)) call b_deallocate( that )
         
      n = (istop-istart)/inc + 1
      if (n <= 0) then
         call b_allocate1(that,0)
         return
      end if
      
      call b_allocate1(that,n)
      call indeces(this,istart,jstart,iistart)
      call indeces(this,istop ,jstop ,iistop)
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
         idest = 0
         do i = istart, istop, inc
            idest = idest + 1
            call b_set0( that, idest, b_fget0(this,i) )
         end do
      end if
   end subroutine 
   
   _PURE_ function b_fextract(this,istart,istop,inc) result(that)
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: istart, istop, inc
      type(bitfield_t) :: that
      
      call b_extract(this,istart,istop,inc,that)
   end function
   
      
      


   _PURE_ logical function b_allall(this)
      class(bitfield_t), intent(in) :: this
      
      b_allall = b_allrange(this,this%lb,this%ub,1)
   end function 

   _PURE_ recursive logical function b_allrange(this,istart,istop,inc) result(v)
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: istart, istop, inc
      
      integer :: j, iistart, iistop, jstart, jstop, i
      integer :: iir(l), iirs
      integer(ik) :: a

      if (inc < 0) then
         v = b_allrange(this,istop+mod(istart-istop,-inc),istart,-inc)
         return
      end if

      if (istop < istart) return

      if (inc == 1) then
         call indeces(this,istart,jstart,iistart)
         call indeces(this,istop ,jstop ,iistop)
         if (jstart == jstop) then
            a = ones
            call mvbits( this%a(jstart), iistart, iistop-iistart+1, a, iistart )
            v = a == ones
         else
            a = ones
            call mvbits( this%a(jstart), iistart, l-iistart, a, iistart )
            v = a == ones
            if (.not.v) return
            do j = jstart + 1, jstop - 1
               v = v .and. this%a(j) == ones
               if (.not.v) return
            end do
            a = ones
            call mvbits( this%a(jstop), 0, iistop+1, a, 0 )
            v = v .and. a == ones
         end if
      else if (inc <= l/minbatch) then
         call indeces(this,istart,jstart,iistart)
         call indeces(this,istop ,jstop ,iistop)
         v = .true.
         j = jstart
         iirs = 0
         do
            call getiirs(jstart,jstop,iistart,iistop,inc,j,iir,iirs)
            v = v .and. all( btest( this%a(j),iir(1:iirs) ) )
            if (.not.v) return
            if (j == jstop) exit
         end do
      else
         v = .false.
         do i = istart, istop, inc
            if (.not.b_fget0( this, i )) return
         end do
         v = .true.
      end if
   end function 



   _PURE_ logical function b_anyall(this)
      class(bitfield_t), intent(in) :: this
      
      b_anyall = b_anyrange(this,this%lb,this%ub,1)
   end function 

   _PURE_ recursive logical function b_anyrange(this,istart,istop,inc) result(v)
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: istart, istop, inc
      
      integer :: j, iistart, iistop, jstart, jstop, i
      integer :: iir(l), iirs
      integer(ik) :: a

      if (inc < 0) then
         v = b_anyrange(this,istop+mod(istart-istop,-inc),istart,-inc)
         return
      end if

      if (istop < istart) return

      if (inc == 1) then
         call indeces(this,istart,jstart,iistart)
         call indeces(this,istop ,jstop ,iistop)
         if (jstart == jstop) then
            a = zeros
            call mvbits( this%a(jstart), iistart, iistop-iistart+1, a, iistart )
            v = a /= zeros
         else
            a = zeros
            call mvbits( this%a(jstart), iistart, l-iistart, a, iistart )
            v = a /= zeros
            if (v) return
            do j = jstart + 1, jstop - 1
               v = v .or. this%a(j) /= zeros
               if (v) return
            end do
            a = zeros
            call mvbits( this%a(jstop), 0, iistop+1, a, 0 )
            v = v .or. a /= zeros
         end if
      else if (inc <= l/minbatch) then
         call indeces(this,istart,jstart,iistart)
         call indeces(this,istop ,jstop ,iistop)
         v = .false.
         j = jstart
         iirs = 0
         do
            call getiirs(jstart,jstop,iistart,iistop,inc,j,iir,iirs)
            v = v .or. any( btest( this%a(j),iir(1:iirs) ) )
            if (v) return
            if (j == jstop) exit
         end do
      else
         v = .true.
         do i = istart, istop, inc
            if (b_fget0( this, i )) return
         end do
         v = .false.
      end if
   end function 



   _PURE_ integer function b_countall(this) result(v)
      class(bitfield_t), intent(in) :: this
      
      v = b_countrange(this,this%lb,this%ub,1)
   end function 

   _PURE_ recursive integer function b_countrange(this,istart,istop,inc) result(v)
      class(bitfield_t), intent(in) :: this
      integer, intent(in) :: istart, istop, inc
      
      integer :: j, iistart, iistop, jstart, jstop, i
      integer :: iir(l), iirs
      integer(ik) :: a
   
      if (inc < 0) then
         v = b_countrange(this,istop+mod(istart-istop,-inc),istart,-inc)
         return
      end if

      if (istop < istart) return

      if (inc == 1) then
         call indeces(this,istart,jstart,iistart)
         call indeces(this,istop ,jstop ,iistop)
         if (jstart == jstop) then
            a = zeros
            call mvbits( this%a(jstart), iistart, iistop-iistart+1, a, iistart )
            v = popcnt( a )
         else
            a = zeros
            call mvbits( this%a(jstart), iistart, l-iistart, a, iistart )
            v = popcnt( a )
            do j = jstart + 1, jstop - 1
               v = v + popcnt( this%a(j) )
            end do
            a = zeros
            call mvbits( this%a(jstop), 0, iistop+1, a, 0 )
            v = v + popcnt( a )
         end if
      else if (inc <= l/minbatch) then
         call indeces(this,istart,jstart,iistart)
         call indeces(this,istop ,jstop ,iistop)
         v = 0
         j = jstart
         iirs = 0
         do
            call getiirs(jstart,jstop,iistart,iistop,inc,j,iir,iirs)
            v = v + count( btest( this%a(j),iir(1:iirs) ) )
            if (j == jstop) exit
         end do
      else
         v = 0
         do i = istart, istop, inc
            if (b_fget0( this, i )) v = v+1
         end do
      end if

   end function 
   
   
   _PURE_ subroutine b_negate(this)
      type(bitfield_t), intent(inout) :: this
            
      this%a(:) = not( this%a )
   end function
   
   _PURE_ function b_fnegate(this) result(b)
      type(bitfield_t), intent(in) :: this
      type(bitfield_t) :: b
            
      call b_allocate2(b,this%lb,this%ub)
      b%a(:) = not( this%a )
   end function
   
   _PURE_ function b_and(this,that) result(b)
      type(bitfield_t), intent(in) :: this, that
      type(bitfield_t) :: b
            
      call b_allocate2(b,this%n)
      b%a(:) = iand( this%a, that%a )
   end function
   
   _PURE_ function b_or(this,that) result(b)
      type(bitfield_t), intent(in) :: this, that
      type(bitfield_t) :: b
            
      call b_allocate2(b,this%n)
      b%a(:) = ior( this%a, that%a )
   end function
   
   _PURE_ function b_eqv(this,that) result(b)
      type(bitfield_t), intent(in) :: this, that
      type(bitfield_t) :: b
            
      call b_allocate2(b,this%n)
      b%a(:) = ieor( this%a, that%a )
      b%a(:) = not(b%a)
   end function

   _PURE_ function b_neqv(this,that) result(b)
      type(bitfield_t), intent(in) :: this, that
      type(bitfield_t) :: b
            
      call b_allocate2(b,this%n)
      b%a(:) = ieor( this%a, that%a )
   end function
   
   _PURE_ logicalfunction b_equal(this,that) result(v)
      type(bitfield_t), intent(in) :: this, that
            
      integer :: j, j1, ii1, j2, ii2
      integer(ik) :: a1, a2
      
      call indeces( this, this%ub, j1, ii1 )
      call indeces( that, that%ub, j2, ii2 )
      a1 = zeros ; a2 = zeros
      call mvbits( this%a(j1), 0, ii1+1, a1, 0 )
      call mvbits( this%a(j2), 0, ii2+1, a2, 0 )
      v = a1 == a2
      if (.not.v) return
      do j = 1, size(this%a) - 1
         v = v .and. this%a(j) == that%a(j)
         if (.not.v) return
      end do
   end function

   _PURE_ logicalfunction b_notequal(this,that) result(v)
      type(bitfield_t), intent(in) :: this, that
            
      integer :: j, j1, ii1, j2, ii2
      integer(ik) :: a1, a2
      
      call indeces( this, this%ub, j1, ii1 )
      call indeces( that, that%ub, j2, ii2 )
      a1 = zeros ; a2 = zeros
      call mvbits( this%a(j1), 0, ii1+1, a1, 0 )
      call mvbits( this%a(j2), 0, ii2+1, a2, 0 )
      v = a1 /= a2
      if (v) return
      do j = 1, size(this%a) - 1
         v = v .or. this%a(j) /= that%a(j)
         if (v) return
      end do
   end function
   
   
   
   _PURE_ subroutine indeces(this,i,j,ii)
      type(bitfield_t), intent(in) :: this
      integer, intent(in) :: i
      integer, intent(out) :: j, ii
      
      ii = i-this%lb
      !j = ii/l ; ii = ii - j*l
      j = shiftr(ii,l2l); ii = ii - shiftl(j,l2l)
   end subroutine
   
   _PURE_ subroutine clear_end(this)
      type(bitfield_t), intent(inout) :: this
      
      integer :: ii, iii, j
      
      call indeces(this,this%ub,j,ii)
      do iii = ii+1, l-1
         this%a(j) = ibclr(this%a(j),iii)
      end do
   end subroutine   
   
   _PURE_ subroutine getiirs(jstart,jstop,iistart,iistop,inc,j,iir,iirs)
      integer, intent(in) :: jstart, jstop, iistart, iistop, inc
      integer, intent(inout) :: j
      integer, intent(inout) :: iir(l), iirs
      
      integer :: ii, delta
      
      if (j > jstart .and. j < jstop) then
         ! more frequent case for large bitsets, the whole chunk is updated
         delta = iirs*inc - l
         if (delta >= 0) then
            iir(1:iirs) = iir(1:iirs) + delta
            if (iir(iirs) > l-1) iirs = iirs-1
         else
            iirs = iirs+1
            iir(1:iirs) = iir(1:iirs) + delta
         end if
         j = j+1
      else if (iirs == 0 .and. jstart == jstop) then
         ! the bitfield is made of a single chunk 
         ii = iistart
         do
            iirs = iirs+1 ; iir(iirs) = ii
            ii = ii + inc ; if (ii > iistop) exit
         end do
      else if (iirs == 0 .and. j == jstart) then
         ! More than one chunk, the first chunk is set (can be incomplete)
         ii = iistart
         do
            iirs = iirs+1 ; iir(iirs) = ii
            ii = ii + inc ; if (ii > l-1) exit
         end do
      else if (j == jstart .and. j < jstop) then
         ! More than two chunks, the second chunk is set (is complete)
         ii = iir(iirs) + inc - l
         iirs = 0
         do
            iirs = iirs+1 ; iir(iirs) = ii
            ii = ii + inc ; if (ii > l-1) exit
         end do
         if (iirs < l) iir(iirs+1) = iir(iirs) + inc
         j = j+1
      else if (iirs > 0 .and. j == jstop) then
         ! More than one chunk, the last chunk is set (can be incomplete)
         ii = iir(iirs) + inc - l
         iirs = 0
         do
            iirs = iirs+1 ; iir(iirs) = ii
            ii = ii + inc ; if (ii > iistop) exit
         end do
      else if (j == jstop) then   
         ! special case, call directly on the last chunk 
         ! inc==1 in this specific case
         iirs = 0
         ii = iistop
         do
            iirs = iirs+1 ; iir(iirs) = ii
            ii = ii - 1 ; if (ii < 0) exit
         end do
      end if
   end subroutine

end module