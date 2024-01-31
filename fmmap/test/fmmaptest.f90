program fmmaptest
use iso_c_binding
use fmmap_m
implicit none

type(fmmap_t) :: x
double precision, pointer :: pr(:)
integer, pointer :: pi2(:,:), pi3(:,:,:)
integer :: i, stat
integer(fmmap_int) :: n, nbytes

type sometype
   integer :: i
   double precision :: a
   character(len=7) :: str
end type
type(sometype), pointer :: pt(:)

n = 1000_fmmap_int

call fmmap_create(pr,[n],'scratch')
pr = [(real(i), i=1,n)]
print*, size(pr), pr(n)
call fmmap_destroy(pr)

call fmmap_create(pi2,[n,n],'new',"./fun1.bin")
pi2(:,:) = 1
pi2(n,n) = -1
call fmmap_destroy(pi2)

call fmmap_create(pi3,[n,n/2],'old',"./fun1.bin")
print*, shape(pi3), pi3(1,1,1), pi3(n,n/2,2)
call fmmap_destroy(pi3)

nbytes = fmmap_nbytes(n,storage_size(pt))
call fmmap_create(x,nbytes,'scratch',"./")
call c_f_pointer(x%cptr, pt, [n])
call random_number( pt(:)%a )
pt(:)%i = [(i, i=1,n)]
pt(:)%str = "Hello"
print*, pt(n)
call fmmap_destroy(x)

end program