!***********************************************************************************************
module fmmap_m
!***********************************************************************************************
use, intrinsic :: iso_c_binding
use, intrinsic :: iso_fortran_env
implicit none

   private
   public :: fmmap_int, fmmap_t, fmmap_nbytes, fmmap_nelems
   public :: fmmap_create, fmmap_destroy

   integer, parameter :: fmmap_int = c_long_long
   character(c_char) :: c
   integer, parameter :: bitsperbyte = storage_size(c)
   type fmmap_t
      private
      type(c_ptr), public   :: cptr = c_null_ptr
      integer(c_int)        :: cfd  = -1
      integer(c_long_long)  :: cn   = 0
   end type
   
   type(fmmap_t), allocatable :: table(:)
   
   character(*), parameter :: msg0="*** fmmap_create(): "
   character(*), parameter :: msg1="the rank of must be 1 to 7"
   character(*), parameter :: msg2="wrong size of sh"
   character(*), parameter :: msg3="requested pointer shape incompatible with file size"
   
   interface
   
      integer(c_int) function c_mmap_create( cp, n, cfm, cfilename, cfd ) BIND(C)
         import :: c_ptr, c_int, c_long_long, c_bool
         type(c_ptr),          intent(out)  :: cp
         integer(c_long_long), value        :: n
         character(len=1),     intent(in)   :: cfilename(*)
         integer(c_int),       value        :: cfm
         integer(c_int),       intent(out)  :: cfd
      end function c_mmap_create

      integer(c_int) function c_mmap_destroy( cp, n, cfd ) BIND(C)
         import :: c_ptr, c_int, c_long_long
         type(c_ptr),          value :: cp
         integer(c_long_long), value :: n
         integer(c_int),       value :: cfd
      end function c_mmap_destroy
      
   end interface
   
   interface fmmap_create
      module procedure fmmap_create_cptr
      module procedure fmmap_create_real,    fmmap_create_dp
      module procedure fmmap_create_integer, fmmap_create_di
   end interface
   
   interface fmmap_destroy
      module procedure fmmap_destroy_cptr
      module procedure fmmap_destroy_real,    fmmap_destroy_dp
      module procedure fmmap_destroy_integer, fmmap_destroy_di
   end interface
   
contains
   
   !********************************************************************************************
   integer(fmmap_int) function fmmap_nbytes(n,ss)
   !********************************************************************************************
   integer(fmmap_int), intent(in) :: n
   integer,           intent(in) :: ss
   !********************************************************************************************
   fmmap_nbytes = n * (ss / bitsperbyte)
   end function fmmap_nbytes
   
   !********************************************************************************************
   integer(fmmap_int) function fmmap_nelems(nbytes,ss)
   !********************************************************************************************
   integer(fmmap_int), intent(in) :: nbytes
   integer,           intent(in) :: ss
   
   integer(fmmap_int) :: bytesperelem
   !********************************************************************************************
   bytesperelem = ss / bitsperbyte
   fmmap_nelems = nbytes / bytesperelem
   if (fmmap_nelems * bytesperelem /= nbytes) then
      error stop "*** fmmap_nelems(): the number of bytes does not form an integer number of elements"
   end if
   end function fmmap_nelems
   
   !********************************************************************************************
   subroutine fmmap_table_push(x)
   !********************************************************************************************
   type(fmmap_t), intent(in) :: x
   
   integer :: i
   !********************************************************************************************
   if (.not.allocated(table)) allocate(table(0))
   i = 1
   do while (i <= size(table))
      if (c_associated( table(i)%cptr, c_null_ptr )) exit
      i = i+1
   end do
   if (i <= size(table)) then
      table(i) = x
   else
      table = [ table, x ]
   end if
   end subroutine fmmap_table_push
   
   !********************************************************************************************
   subroutine fmmap_table_pull(x,cptr)
   !********************************************************************************************
   type(fmmap_t), intent(out) :: x
   type(c_ptr),   intent(in)  :: cptr
   
   integer :: i
   !********************************************************************************************
   i = 1
   do while (i <= size(table))
      if (c_associated( table(i)%cptr, cptr )) exit
      i = i+1
   end do
   if (i > size(table)) then
      error stop "*** fmmap_destroy(): pointer not found in the internal table"
   end if
   x = table(i)
   table(i)%cptr = c_null_ptr
   end subroutine fmmap_table_pull
   
   

   !********************************************************************************************
   subroutine fmmap_create_cptr(x,n,filemode,filename)
   !********************************************************************************************
   type(fmmap_t),    intent(out)           :: x
   integer(fmmap_int)                      :: n
   character(*),     intent(in)            :: filemode 
   character(*),     intent(in),  optional :: filename
   
   integer(c_int) :: cfm
   integer :: i, lu, stat
   character(:), allocatable :: filename___
   character(128) :: msg
   !********************************************************************************************
   
   if (storage_size(0_c_int)/file_storage_size /= c_sizeof(0_c_int)) then
      error stop "*** fmmap_init: the file storage unit is not a byte"
   end if 
   
   x%cptr = c_null_ptr
   
   if (filemode == 'scratch') then
      cfm = 1
      x%cn = n
      if (present(filename)) then
         filename___ = trim(filename)//"fmmaptmp"
      else 
         filename___ = "./fmmaptmp"
      end if
   else if (filemode == 'old') then
      cfm = 2
      filename___ = filename
      inquire(file=trim(filename___), size=x%cn)
      if (x%cn < 0) then
         error stop "*** fmmap_create_cptr: unable to get the file size"
      end if
      n = x%cn
   else if (filemode == 'new') then
      cfm = 3
      x%cn = n
      filename___ = filename
      open(newunit=lu,file=filename___,status='new',form='unformatted',access='stream')
      !if (lu < 0) return ! apparently lu can be <0 ??
      write(lu,pos=x%cn) c_null_char
      close(lu)
   else
      error stop "*** fmmap_create_cptr: wrong filemode"
   end if
   
   stat = c_mmap_create( x%cptr                      &
                       , x%cn                        &
                       , cfm                         &
                       , filename___ // c_null_char  &
                       , x%cfd                       )
   if (stat /= 0) then
      write(msg,*) "*** fmmap_create_cptr: error code ", stat
      error stop trim(msg)
   end if
                  
   end subroutine fmmap_create_cptr


   !********************************************************************************************
   subroutine fmmap_destroy_cptr(x)
   !********************************************************************************************
   type(fmmap_t), intent(inout) :: x
   
   integer :: i, stat
   character(128) :: msg
   !********************************************************************************************
   
   if (.not.c_associated(x%cptr)) then
      error stop "*** fmmap_destroy_cptr: attempt to free a non associated pointer"
   end if
      
   stat = c_mmap_destroy( x%cptr             &
                        , x%cn               &
                        , x%cfd              )
   if (stat /= 0) then
      write(msg,*) "*** fmmap_destroy_cptr: error code ", stat
      error stop trim(msg)
   end if
   
   end subroutine fmmap_destroy_cptr
   
   
   
   
   
   !********************************************************************************************
   subroutine fmmap_create_real(p,sh,filemode,filename,lbound)
   !********************************************************************************************
   real, pointer :: p(..)
   real, pointer :: q(:)

   include "fmmap_create.fi"
        
   end subroutine fmmap_create_real


   !********************************************************************************************
   subroutine fmmap_create_dp(p,sh,filemode,filename,lbound)
   !********************************************************************************************
   double precision, pointer :: p(..)
   double precision, pointer :: q(:)

   include "fmmap_create.fi"
        
   end subroutine fmmap_create_dp


   !********************************************************************************************
   subroutine fmmap_create_integer(p,sh,filemode,filename,lbound)
   !********************************************************************************************
   integer, pointer :: p(..)
   integer, pointer :: q(:)

   include "fmmap_create.fi"
        
   end subroutine fmmap_create_integer


   !********************************************************************************************
   subroutine fmmap_create_di(p,sh,filemode,filename,lbound)
   !********************************************************************************************
   integer, parameter :: di = selected_int_kind(r=15)
   integer(kind=di), pointer :: p(..)
   integer(kind=di), pointer :: q(:)

   include "fmmap_create.fi"
        
   end subroutine fmmap_create_di


   !********************************************************************************************
   subroutine fmmap_destroy_real(p)
   !********************************************************************************************
   real, pointer :: p(..)

   type(fmmap_t) :: x
   !********************************************************************************************  
   call fmmap_table_pull(x,c_loc(p))
   call fmmap_destroy_cptr(x)
   
   end subroutine fmmap_destroy_real
   

   !********************************************************************************************
   subroutine fmmap_destroy_dp(p)
   !********************************************************************************************
   double precision, pointer :: p(..)

   type(fmmap_t) :: x
   !********************************************************************************************  
   call fmmap_table_pull(x,c_loc(p))
   call fmmap_destroy_cptr(x)
   
   end subroutine fmmap_destroy_dp


   !********************************************************************************************
   subroutine fmmap_destroy_integer(p)
   !********************************************************************************************
   integer, pointer :: p(..)

   type(fmmap_t) :: x
   !********************************************************************************************
   call fmmap_table_pull(x,c_loc(p))
   call fmmap_destroy_cptr(x)
   
   end subroutine fmmap_destroy_integer
   

   !********************************************************************************************
   subroutine fmmap_destroy_di(p)
   !********************************************************************************************
   integer, parameter :: di = selected_int_kind(r=15)
   integer(kind=di), pointer :: p(..)

   type(fmmap_t) :: x
   !********************************************************************************************
   call fmmap_table_pull(x,c_loc(p))
   call fmmap_destroy_cptr(x)
   
   end subroutine fmmap_destroy_di
   

end module fmmap_m