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
! call b%set(from,to,inc,bool) 
!     logical :: bool[(:)]
!     integer :: pos, from, top, inc
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
! call b%get(from,to,inc,bool)
!     logical :: bool(:)
!     integer :: pos, frompos, topos
!     Note: bool(:) must be allocated beforehand
!
! bool = b%fget(pos)
!     logical :: bool
! bool = b%fget()
! bool = b%fget(from,to,inc)
!     logical :: bool(:)
!     integer :: pos, from, top, inc
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
! call b%extract(from,to,inc,c)
! c = b%fextract(from,to,inc)
!     integer :: from, to, inc
!     type(bitfield_t) :: c
!     Note: in the subroutine form, c must not be allocated beforehand
!
! n = b%count() 
! n = b%count(from,top,inc) 
!     integer :: from, to, inc
!
! bool = b%all()            
! bool = b%all(from,to,inc) 
!     integer :: from, to, inc
!
! bool = b%any()           
! bool = b%any(from,to,inc) 
!     integer :: from, to, inc
!***********************************************************************************************
module bitfield
!use iso_fortran_env
implicit none

private

public :: bitfield_t, bitfield_is_usable
public :: assignment(=)

integer, parameter :: l = bit_size(0)
integer, parameter :: l2l = nint(log(real(l))/log(2.0))
integer, parameter :: zeros = 0
integer, parameter :: ones = not(zeros)

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
end type

interface assignment(=)
   module procedure assign_l2b_0, assign_l2b_1, assign_b2l
end interface

contains

   logical function bitfield_is_usable()
   integer :: ii
      bitfield_is_usable = .not.any(btest(zeros,[(ii,ii=0,l-1)]))
   end function
   
   pure subroutine b_allocate1(this,n)
   class(bitfield_t), intent(inout) :: this
   integer, intent(in) :: n
      call b_allocate2(this,1,n)
   end subroutine 

   pure subroutine b_allocate2(this,lb,ub)
   class(bitfield_t), intent(inout) :: this
   integer, intent(in) :: lb, ub
   integer :: ii
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

   pure subroutine b_deallocate(this)
   class(bitfield_t), intent(inout) :: this
      if (.not.allocated(this%a)) error stop "bitfield is not allocated"
      deallocate( this%a )
      this%n = -1
      this%lb = 1
      this%ub = 0
   end subroutine 
   


   integer pure function b_getsize(this)
   class(bitfield_t), intent(in) :: this
      b_getsize = this%n
   end function 
   
   integer pure function b_getlb(this)
   class(bitfield_t), intent(in) :: this
      b_getlb = this%lb
   end function 

   integer pure function b_getub(this)
   class(bitfield_t), intent(in) :: this
      b_getub = this%ub
   end function 

   pure subroutine b_setlb(this,lb)
   class(bitfield_t), intent(inout) :: this
   integer, intent(in) :: lb
      if (this%n > 0) then
         this%lb = lb
         this%ub = lb + this%n -1
      end if
   end subroutine 

   pure subroutine b_setub(this,ub)
   class(bitfield_t), intent(inout) :: this
   integer, intent(in) :: ub
      if (this%n > 0) then
         this%lb = ub - this%n + 1
         this%ub = ub
      end if
   end subroutine 
   
   

   pure subroutine b_set0(this,i,v)
   class(bitfield_t), intent(inout) :: this
   integer, intent(in) :: i
   logical, intent(in) :: v
   integer :: ii, j
      ! no runtime check, as it would hurt the performances for a single bit set
      call b_indeces(this,i,j,ii)
      if (v) then
         this%a(j) = ibset(this%a(j),ii)
      else
         this%a(j) = ibclr(this%a(j),ii)
      end if
   end subroutine 

   pure subroutine b_setall0(this,v)
   class(bitfield_t), intent(inout) :: this
   logical, intent(in) :: v
      if (.not.allocated(this%a)) error stop "b_setall0: bitfield is not allocated"
      this%a(:) = merge(ones,zeros,v)
   end subroutine 

   pure subroutine b_setrange0(this,istart,istop,inc,v)
   class(bitfield_t), intent(inout) :: this
   integer, intent(in) :: istart, istop, inc
   logical, intent(in) :: v
   integer :: a, n
   integer :: iistart, iistop, jstart, jstop
   type(bitfield_t) :: that
      if ((istop-istart)*inc < 0) return
      if (.not.allocated(this%a)) error stop "b_setrange0: bitfield is not allocated"
      if (istart < this%lb .or. istart > this%ub .or. istop < this%lb .or. istop > this%ub) &
         error stop "b_setrange0(): out of bound indeces" 
      if (inc == 1) then
         a = merge(ones,zeros,v)
         call b_indeces(this,istart,jstart,iistart)
         call b_indeces(this,istop ,jstop ,iistop)
         if (jstart == jstop) then
            call mvbits(a,0,istop-istart+1,this%a(jstart),iistart)
         else
            call mvbits(a,0,l-iistart,this%a(jstart),iistart)
            this%a(jstart+1:jstop-1) = a
            call mvbits(a,0,iistop+1,this%a(jstop),0)
         endif
      else
         call b_extract(this,istart,istop,inc,that)
         call b_setall0(that,v)
         call b_replace(this,istart,istop,inc,that)
      end if
   end subroutine 
   
   pure subroutine b_setall1(this,v)
   class(bitfield_t), intent(inout) :: this
   logical, intent(in) :: v(:)
      call b_setrange1(this,this%lb,this%ub,1,v)
   end subroutine 

   pure subroutine b_setrange1(this,istart,istop,inc,v)
   class(bitfield_t), intent(inout) :: this
   integer, intent(in) :: istart, istop, inc
   logical, intent(in) :: v(:)
   integer :: k, j, i, iistart, iistop, jstart, jstop
   integer, allocatable :: iir(:)
      if (.not.allocated(this%a)) error stop "b_setrange1: bitfield is not allocated"
      if (istart < this%lb .or. istart > this%ub .or. istop < this%lb .or. istop > this%ub) &
         error stop "b_setrange1(): out of bound indeces" 
      call b_indeces(this,istart,jstart,iistart)
      call b_indeces(this,istop ,jstop ,iistop)

      j = jstart
      i = 0
      do
         call b_getiir(jstart,jstop,iistart,iistop,inc,j,iir)
         do k = 1, size(iir)
            i = i+1
            if (v(i)) then ; this%a(j) = ibset(this%a(j),iir(k))
                      else ; this%a(j) = ibclr(this%a(j),iir(k))
            end if
         end do
         if (j == jstop) exit
      end do
   end subroutine 

   pure subroutine assign_l2b_0(this,v)
   class(bitfield_t), intent(inout) :: this
   logical, intent(in) :: v
      call b_setall0(this,v)
   end subroutine 
   
   pure subroutine assign_l2b_1(this,v)
   class(bitfield_t), intent(inout) :: this
   logical, intent(in) :: v(:)
      if (allocated(this%a) .and. this%getsize() /= size(v)) call b_deallocate(this)
      if (.not.allocated(this%a)) call b_allocate1(this,size(v))
      call b_setall1(this,v)
   end subroutine 

   

   pure subroutine b_get1(this,i,v)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: i
   logical, intent(out) :: v
   integer :: j, ii
      call b_indeces(this,i,j,ii)
      v = btest(this%a(j),ii)
   end subroutine 
   
   pure subroutine b_getall(this,v)
   class(bitfield_t), intent(in) :: this
   logical, intent(out) :: v(:)
      if (this%getsize() /= size(v)) error stop "b_getall(): the sizes differ" 
      call b_getrange(this,this%lb,this%ub,1,v)
   end subroutine 
   
   pure subroutine b_getrange(this,istart,istop,inc,v)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: istart, istop, inc
   logical, intent(out) :: v(:)
   integer :: i1, i2, j, iistart, iistop, jstart, jstop
   integer, allocatable :: iir(:)
      if ((istop-istart)*inc < 0) return
      if (istart < this%lb .or. istart > this%ub .or. istop < this%lb .or. istop > this%ub) &
         error stop "b_setrange1(): out of bound indeces" 
      call b_indeces(this,istart,jstart,iistart)
      call b_indeces(this,istop ,jstop ,iistop)

      j = jstart
      i1 = 1
      do
         call b_getiir(jstart,jstop,iistart,iistop,inc,j,iir)
         i2 = i1+size(iir)-1
         v(i1:i2) = btest(this%a(j),iir)
         if (j == jstop) exit
         i1 = i2+1
      end do
   end subroutine 
         
   pure function b_fget1(this,i) result(v)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: i
   logical :: v
      call b_get1(this,i,v)
   end function 

   pure function b_fgetall(this) result(v)
   class(bitfield_t), intent(in) :: this
   logical, allocatable:: v(:)
      allocate( v(this%getlb():this%getub()) )
      call b_getall(this,v)
   end function 

   pure function b_fgetrange(this,istart,istop,inc) result(v)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: istart, istop, inc
   logical, allocatable :: v(:)
      allocate( v(istart:istop) )
      call b_getrange(this,istart,istop,inc,v)   
   end function

   pure subroutine assign_b2l(v,this)
   logical, allocatable, intent(out) :: v(:)
   class(bitfield_t), intent(in) :: this
      if (allocated(v) .and. this%getsize() /= size(v)) deallocate(v)
      if (.not.allocated(v)) allocate( v(this%getsize()) )
      call b_getall(this,v)
   end subroutine 


   
   pure subroutine b_replace(this,istart,istop,inc,that)
   class(bitfield_t), intent(inout) :: this
   integer, intent(in) :: istart, istop, inc
   type(bitfield_t), intent(in) :: that
   integer :: k, i, iistart, iistop, jstart, jstop, j, jsource, iisource
   integer, allocatable :: iir(:)
      if (that%getsize() <= 0) return
      if (istart < this%lb .or. istart > this%ub .or. istop < this%lb .or. istop > this%ub) &
         error stop "b_replace(): out of bound bounds" 
      call b_indeces(this,istart,jstart,iistart)
      call b_indeces(this,istop,jstop ,iistop)
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
         jsource = 0
         iisource = 0
         do
            call b_getiir(jstart,jstop,iistart,iistop,inc,j,iir)
            do k = 1, size(iir)
               if (btest(that%a(jsource),iisource)) then ; this%a(j) = ibset(this%a(j),iir(k))
                                                    else ; this%a(j) = ibclr(this%a(j),iir(k))
               end if
               iisource = iisource+1
               if (iisource == l) then
                  iisource = 0
                  jsource = jsource+1
               end if
            end do
            if (j == jstop) exit
         end do
      end if
   end subroutine 



   pure subroutine b_extract(this,istart,istop,inc,that)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: istart, istop, inc
   type(bitfield_t), intent(inout) :: that
   integer :: k, i, iistart, iistop, jstart, jstop, j, jdest, iidest, n
   integer, allocatable :: iir(:)
      if ((istop-istart)*inc < 0) return
      if (istart < this%lb .or. istart > this%ub .or. istop  < this%lb .or. istop  > this%ub) &
         error stop "b_extract(): out of bound indeces" 
      if (allocated(that%a)) error stop "b_pull: destination is already allocated"
      n = (istop-istart)/inc + 1
      call b_allocate1(that,n)
      call b_indeces(this,istart,jstart,iistart)
      call b_indeces(this,istop ,jstop ,iistop)
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
         jdest = 0
         iidest = 0
         do
            call b_getiir(jstart,jstop,iistart,iistop,inc,j,iir)
            do k = 1, size(iir)
               if (btest(this%a(j),iir(k))) then ; that%a(jdest) = ibset(that%a(jdest),iidest)
                                            else ; that%a(jdest) = ibclr(that%a(jdest),iidest)
               end if
               iidest = iidest+1
               if (iidest == l) then
                  iidest = 0
                  jdest = jdest+1
               end if
            end do
            if (j == jstop) exit
         end do
      end if
   end subroutine 
   
   pure function b_fextract(this,istart,istop,inc) result(that)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: istart, istop, inc
   type(bitfield_t) :: that
      call b_extract(this,istart,istop,inc,that)
   end function
   
      
      


   pure logical function b_allall(this)
   class(bitfield_t), intent(in) :: this
      b_allall = b_allrange(this,this%lb,this%ub,1)
   end function 

   pure logical function b_allrange(this,istart,istop,inc) result(v)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: istart, istop, inc
   integer :: i, j, iistart, iistop, jstart, jstop
   integer, allocatable :: iir(:)
      v = .true.
      if (istart > istop) return
      call b_indeces(this,istart,jstart,iistart)
      call b_indeces(this,istop ,jstop ,iistop)

      j = jstart
      call b_getiir(jstart,jstop,iistart,iistop,inc,j,iir)
      v = all(btest(this%a(j),iir))
      if (.not.v) return
      if (jstop == jstart) return
      if (abs(inc) == 1) then
         deallocate(iir)
         do j = jstart + sign(1,inc), jstop - sign(1,inc), sign(1,inc)
            v = v .and. this%a(j) == ones
            if (.not.v) return
         end do
         call b_getiir(jstart,jstop,iistart,iistop,inc,jstop,iir)
         v = v .and. all(btest(this%a(jstop),iir))
      else
         do
            call b_getiir(jstart,jstop,iistart,iistop,inc,j,iir)
            v = v .and. all(btest(this%a(j),iir))
            if (.not.v) return
            if (j == jstop) exit
         end do
      end if
   end function 

   pure logical function b_anyall(this)
   class(bitfield_t), intent(in) :: this
      b_anyall = b_anyrange(this,this%lb,this%ub,1)
   end function 

   pure logical function b_anyrange(this,istart,istop,inc) result(v)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: istart, istop, inc
   integer :: j, iistart, iistop, jstart, jstop
   integer, allocatable :: iir(:)
   type(bitfield_t) :: that
      if (istart > istop) return
      call b_indeces(this,istart,jstart,iistart)
      call b_indeces(this,istop ,jstop ,iistop)

      j = jstart
      call b_getiir(jstart,jstop,iistart,iistop,inc,j,iir)
      v = any(btest(this%a(j),iir))
      if (v) return
      if (jstop == jstart) return
      if (abs(inc) == 1) then
         deallocate(iir)
         do j = jstart + sign(1,inc), jstop - sign(1,inc), sign(1,inc)
            v = v .or. this%a(j) /= zeros
            if (v) return
         end do
         call b_getiir(jstart,jstop,iistart,iistop,inc,jstop,iir)
         v = v .or. any(btest(this%a(jstop),iir))
      else
         do
            call b_getiir(jstart,jstop,iistart,iistop,inc,j,iir)
            v = v .or. any(btest(this%a(j),iir))
            if (v) return
            if (j == jstop) exit
         end do
      end if
   end function 
   
   

   pure integer function b_countall(this) result(v)
   class(bitfield_t), intent(in) :: this
      v = b_countrange(this,this%lb,this%ub,1)
   end function 

   pure integer function b_countrange(this,istart,istop,inc) result(v)
   class(bitfield_t), intent(in) :: this
   integer, intent(in) :: istart, istop, inc
   integer :: j, iistart, iistop, jstart, jstop
   integer, allocatable :: iir(:)
   type(bitfield_t) :: that
      if ((istop-istart)*inc < 0) return
      call b_indeces(this,istart,jstart,iistart)
      call b_indeces(this,istop ,jstop ,iistop)

      j = jstart
      call b_getiir(jstart,jstop,iistart,iistop,inc,j,iir)
      v = count(btest(this%a(j),iir))
      if (jstop == jstart) return
      if (abs(inc) == 1) then
         deallocate(iir)
         do j = jstart + sign(1,inc), jstop - sign(1,inc), sign(1,inc)
            v = v + popcnt(this%a(j))
         end do
         call b_getiir(jstart,jstop,iistart,iistop,inc,jstop,iir)
         v = v + count(btest(this%a(jstop),iir))
      else
         do
            call b_getiir(jstart,jstop,iistart,iistop,inc,j,iir)
            v = v + count(btest(this%a(j),iir))
            if (j == jstop) exit
         end do
      end if

   end function 
   
   
   
   pure subroutine b_indeces(this,i,j,ii)
   type(bitfield_t), intent(in) :: this
   integer, intent(in) :: i
   integer, intent(out) :: j, ii
      ii = i-this%lb
      !j = ii/l ; ii = ii - j*l
      ! Speed-up of about 15% on random accesses if shiftr() and shiftl() are used,
      ! but it is fully portable?
      j = shiftr(ii,l2l); ii = ii - shiftl(j,l2l)
   end subroutine
   
   pure subroutine b_getiir(jstart,jstop,iistart,iistop,inc,j,iir)
   integer, intent(in) :: jstart, jstop, iistart, iistop, inc
   integer, intent(inout) :: j
   integer, allocatable, intent(inout) :: iir(:)
   integer :: ii, iinew
   if (.not.allocated(iir)) then
      if (jstart == jstop) then
         iir = [(ii,ii=iistart,iistop,inc)]
      else if (j == jstart) then
         if (inc > 0) then ; iir = [(ii,ii=iistart,l-1,inc)]
                      else ; iir = [(ii,ii=iistart,0,inc)]
         end if
      else if (j == jstop) then
         if (inc > 0) then ; iir = [(ii,ii=iistop,0,-inc)]
                      else ; iir = [(ii,ii=iistop,l-1,-inc)]
         end if
      end if
   else
      iinew = iir(size(iir)) + inc
      do while (iinew >= l)
         iinew = iinew - l ; j = j + 1
      end do
      do while (iinew < 0)
         iinew = iinew + l ; j = j - 1
      end do
      if (j == jstop) then
         iir = [(ii,ii=iinew,iistop,inc)]
      else
         if (inc > 0) then ; iir = [(ii,ii=iinew,l-1,inc)]
                      else ; iir = [(ii,ii=iinew,0,inc)]
         end if
      end if
   end if
   end subroutine
     
end module