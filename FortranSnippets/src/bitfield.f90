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
! call b%allocate(1000)    ! allocates 1000 1-bit logicals
! call b%setall(.false.)   ! sets all of them to .false.
! call b%set(100,.true.)   ! sets the 100th to .true.
! v = b%get(200)           ! gets the value of the 200th
! call b%deallocate()      ! deallocates the bitfield
!***********************************************************************************************
module bitfield
!use iso_fortran_env
implicit none

private

integer, parameter :: l=bit_size(0)

type, public :: bitfield_t
    integer, allocatable, private :: a(:)
    integer, private :: n=-1
    contains
    procedure :: allocate => b_allocate
    procedure :: deallocate => b_deallocate
    procedure :: set => b_set
    procedure :: get => b_get
    procedure :: setall => b_setall
    procedure :: size => b_size
end type

contains

    subroutine b_allocate(this,n)
    class(bitfield_t), intent(inout) :: this
    integer, intent(in) :: n
    allocate( this%a(0:(n-1)/l) )
    this%n = n
    end subroutine 

    subroutine b_deallocate(this)
    class(bitfield_t), intent(inout) :: this
    deallocate( this%a )
    this%n = -1
    end subroutine 

    subroutine b_set(this,i,v)
    class(bitfield_t), intent(inout) :: this
    integer, intent(in) :: i
    logical, intent(in) :: v
    integer :: ii, j
    ii = i-1
    j = ii/l
    if (v) then
        this%a(j) = ibset(this%a(j),ii-l*j)
    else
        this%a(j) = ibclr(this%a(j),ii-l*j)
    end if
    end subroutine 

    logical function b_get(this,i)
    class(bitfield_t), intent(in) :: this
    integer, intent(in) :: i
    integer :: ii, j
    ii = i-1
    j = ii/l
    b_get = btest(this%a(j),ii-l*j)
    end function 

    subroutine b_setall(this,v)
    class(bitfield_t), intent(inout) :: this
    logical, intent(in) :: v
    integer :: i
    integer :: a
    do i = 0, l-1
        if (v) then
            a = ibset(a,i)
        else
            a = ibclr(a,i)
        end if
    end do
    this%a(:) = a
    end subroutine 

    integer function b_size(this)
    class(bitfield_t), intent(in) :: this
    b_size = this%n
    end function 

end module