! ===========================================
! file: FC_bind_handle_f.f90
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

contains

   ! function that creates a sometype instance,
   ! allocates the %a component,
   ! and returns a C pointer to the instance 
   type(c_ptr) function sometype_create(len) bind(c)
      integer(c_int), value :: len
      
      type(sometype), pointer :: p
      
      allocate(p)
      allocate(p%a(len))
      sometype_create = c_loc(p)
   end function

   ! routine that releases a handle 
   subroutine sometype_free(ptr) bind(c)
      type(c_ptr) :: ptr
      
      type(sometype), pointer :: p
      
      call c_f_pointer(ptr,p)
      deallocate(p%a)   ! would actually be automatically deallocated
      deallocate(p)
      ptr = c_null_ptr
   end subroutine
   
   ! function that returns a C pointer to the %a component, 
   ! allowing to work directly on it from C
   type(c_ptr) function sometype_a_ptr(ptr, n) bind(c)
      type(c_ptr), value :: ptr
      integer(c_int), intent(out) :: n
      
      type(sometype), pointer :: p
      
      call c_f_pointer(ptr,p)
      n = size(p%a)
      sometype_a_ptr = c_loc(p%a)
   end function

   ! wrapper routine to a Fortran routine that performs 
   ! some processing on a sometype instance
   subroutine sometype_x2(ptr) bind(c)
      type(c_ptr), value :: ptr
    
      type(sometype), pointer :: p
    
      call c_f_pointer(ptr,p)
      call sometype_x2_f(p)
   end subroutine
   
end module
! ===========================================

