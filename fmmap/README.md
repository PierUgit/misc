# fmmap: memory mapped files in Fortran

## Introduction

These routines provide *some* of the features of the posix/C memory mapped files with a Fortran interface.

2 usages:
- allocating arrays that are potentially bigger the RAM+swap size, and are which are backed in a temporary file (anonymous mapping)
- opening existing files or creating new files, and mapping them to an array

## Usage

There are 2 different approaches:
- one which is "type agnostic", as it just manipulates bytes and returns a C pointer. The programmer has to manage the conversion between elements and bytes and between the C and a Fortran pointer... Which is not a big deal anyway
- another one that hides all the C stuff and returns directly a Fortran pointer. Overall simpler. It could be implemented for all the intrinsic types/kinds, but for demonstration it is just implemented right now for `real`, `double precision`, `integer`, and `integer(kind=selected_real_kind(r=15))`.

The approaches are complementary.

### "type agnostic"

Example:
```fortran
use iso_C_binding
use ffmap_m

type sometype
   integer :: i
   double precision :: a
   character(len=7) :: str
end type
type(sometype), pointer :: pt(:)
type(fmmap_t) :: x 


integer(fmmap_int) :: n, nbytes
...
...
n = 1000
nbytes = fmmap_nbytes(n, storage_size(pt)) ! converts 1000 elements to a number of bytes
call fmmap_create(x, nbytes, 'scratch')    ! anonymous mapping
call c_f_pointer(x%cptr, pt, [n])          ! conversion to a Fortran pointer
...
call fmmap_destroy(x)                      ! close the temporary file, etc...
```

### intrinsic types

Example:
```fortran
use ffmap_m   ! note that iso_c_binding is not needed

integer, pointer :: pi(:)
integer(fmmap_int) :: n
...
n = 1000
call fmmap_create(pi, [n], 'new', "./foo1.bin") ! named mapping
...                                             ! returns a 1D pointer of size n
call fmmap_destroy(pi)
```

Other example:
```fortran
use ffmap_m

double precision, pointer :: pr(:,:,:)
integer(fmmap_int) :: n, m
...
n = 1000; m = 100
call fmmap_create(pr, [n,m], 'old', "./foo2.bin") ! named mapping
...                                               ! returns a 3D pointer of size n*m*p
                                                  ! where p is determined by the file size
call fmmap_destroy(pr)
```

