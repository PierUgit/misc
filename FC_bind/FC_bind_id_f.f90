! ===========================================
! file: FC_bind_id_f.f90
module somemodule
   use, intrinsic :: iso_c_binding
   implicit none
    
   ! c_float is needed only if one wants to 
   ! access %a directly from C
   type :: sometype
      integer(c_int), allocatable :: a(:)
   end type
    
contains

   ! example routine that processes the content of the type
   subroutine sometype_x2_f(x)
      type(sometype), intent(inout) :: x
      
      x%a = 2 * x%a
   end subroutine
   
end module
! ===========================================

! ===========================================
! file: somemodule_wrap.f90 
module somemodule_wrap
   use, intrinsic :: iso_c_binding
   use somemodule 
   implicit none
   
   ! again, "target" is needed only if one wants to access the %a component from C
   type(sometype), allocatable, target :: h(:)
   logical, allocatable :: active(:)

contains

   ! function that creates a sometype instance,
   ! allocates the %a component,
   ! and returns a C pointer to the instance 
   integer(c_int) function sometype_create(len) bind(c)
      integer(c_int), value :: len
      
      integer :: id
      
      if (.not.allocated(h)) then
         allocate(h(0))
         allocate(active(0))
      end if
      id = findloc(active,.false.,dim=1)
      if (id == 0) then
         h = [h, sometype()]
         active = [active, .false.]
         id = size(h)
      end if
      active(id) = .true.
      allocate(h(id)%a(len))
      sometype_create = id
   end function

   ! routine that releases a handle 
   subroutine sometype_free(id) bind(c)
      integer(c_int) :: id
            
      deallocate(h(id)%a)
      active(id) = .false.
      if (id == size(h)) then
         do while (id > 0 .and. .not.active(id))
            h = h(1:id-1)
            active = active(1:id-1)
            id = id-1
         end do
      end if
      id = 0
   end subroutine
   
   ! function that returns a C pointer to the %a component, 
   ! allowing to work directly on it from C
   type(c_ptr) function sometype_a_ptr(id, n) bind(c)
      integer(c_int), value :: id
      integer(c_int), intent(out) :: n
            
      n = size(h(id)%a)
      sometype_a_ptr = c_loc(h(id)%a)
   end function

   ! wrapper routine to a Fortran routine that performs 
   ! some processing on a sometype instance
   subroutine sometype_x2(id) bind(c)
      integer(c_int), value :: id
        
      call sometype_x2_f(h(id))
   end subroutine
   
end module
! ===========================================

