!***********************************************************************************************
module sums
!***********************************************************************************************
! Some summations routines for runtime and accuracy benchmark
!***********************************************************************************************
use iso_fortran_env, only: sp => real32, &
                           dp => real64, &
                           qp => real128
implicit none

integer, parameter :: ep = selected_real_kind(p=precision(0.0_dp)+1)

contains

   ! simple do loop with single precision accumulator
   pure real(sp) function sum_sp(a) result(s)
   real(sp), intent(in) :: a(:)
   integer :: i
   s = 0.0_sp
   !!$OMP SIMD REDUCTION(+:s)
   do i = 1, size(a)
      s = s + a(i)
   end do
   end function
   
   ! simple do loop with double precision accumulator
   pure real(sp) function sum_dp(a) result(s)
   real(sp), intent(in) :: a(:)
   integer :: i
   real(dp) :: ss
   ss = 0.0_dp
   !!$OMP SIMD REDUCTION(+:ss)
   do i = 1, size(a)
      ss = ss + a(i)
   end do
   s = ss
   end function
   
   ! simple do loop with extended precision accumulator
   pure real(sp) function sum_ep(a) result(s)
   real(sp), intent(in) :: a(:)
   integer :: i
   real(ep) :: ss
   ss = 0.0_ep
   !!$OMP SIMD REDUCTION(+:ss)
   do i = 1, size(a)
      ss = ss + a(i)
   end do
   s = ss
   end function
   
   ! simple do loop with quadruple precision accumulator (emulated)
   pure real(sp) function sum_qp(a) result(s)
   real(sp), intent(in) :: a(:)
   integer :: i
   real(qp) :: ss
   ss = 0.0_qp
   !!$OMP SIMD REDUCTION(+:ss)
   do i = 1, size(a)
      ss = ss + a(i)
   end do
   s = ss
   end function
   
   ! pairwise summation, possibly chunked
   pure recursive real(sp) function psum(a,chunk) result(s)
   real(sp), intent(in) :: a(:)
   integer, intent(in), optional :: chunk
   integer :: n, k, chunk___
   chunk___ = 2; if (present(chunk)) chunk___ = max(chunk___,chunk)
   n = size(a)
   if (n <= chunk___) then
      s = sum(a)
      return
   end if
   k = (n+1)/2
   s = psum(a(1:k),chunk) + psum(a(k+1:n),chunk)
   end function
   
   ! pairwise summation in full double precision
   pure recursive real(dp) function psum_dp(a) result(s)
   real(sp), intent(in) :: a(:)
   integer :: n, k
   n = size(a)
   if (n == 2) then
      s = real(a(1),dp) + real(a(2),dp)
      return
   else if (n == 1) then
      s = real(a(1),dp)
      return
   end if
   k = (n+1)/2
   s = psum_dp(a(1:k)) + psum_dp(a(k+1:n))
   end function

   ! Kahan summation 
   !!! possibly defeated by agressive compiler optimizations !!!
   ! taken for Beliavsky on Fortran Discourse
   pure real(sp) function ksum(a) result(s)
   real(sp), intent(in) :: a(:)
   integer :: i, n
   real(sp) :: ks, temp, t, c
   n    = size(a)
   ks   = 0.0_sp
   temp = 0.0_sp
   t    = 0.0_sp
   c    = 0.0_sp
   do i=1,n
      temp         = a(i) - c
      t            = ks + temp
      c            = (t - ks) - temp
      ks           = t
   end do
   s = ks
   end function ksum

   ! Kahan summation, chunked
   !!! possibly defeated by agressive compiler optimizations !!!
   ! taken for Beliavsky on Fortran Discourse
   pure real(sp) function ksumc(a,chunk) result(s)
   real(sp), intent(in) :: a(:)
   integer, intent(in) :: chunk
   integer :: i, n, jend, kend
   real(sp) :: ks(chunk), temp(chunk), t(chunk), c(chunk)
   n    = size(a)
   ks   = 0.0_sp
   temp = 0.0_sp
   t    = 0.0_sp
   c    = 0.0_sp
   do i=1,n,chunk
      jend         = min(i+chunk-1,n)
      kend         = jend-i+1
      temp(1:kend) = a(i:jend)  - c(1:kend)
      t(1:kend)    = ks(1:kend) + temp(1:kend)
      c(1:kend)    = (t(1:kend) - ks(1:kend)) - temp(1:kend)
      ks(1:kend)   = t(1:kend)
   end do
   s = sum(ks)
   end function ksumc




   ! simple do loop with single precision accumulator
   real(sp) function sum_err_sp(a,err) result(s)
   real(sp), intent(in) :: a(:)
   real(dp), intent(out) :: err
   integer :: i
   real(dp) :: tmp
   s = 0.0_sp
   err = 0.0_dp
   do i = 1, size(a)
      s = s + a(i)
      call random_number(tmp)
      err = err + (tmp-0.5)*spacing(s)
   end do
   end function

   ! pairwise summation, possibly chunked
   recursive real(sp) function psum_err(a,err,chunk) result(s)
   real(sp), intent(in) :: a(:)
   real(dp), intent(out) :: err
   integer, intent(in), optional :: chunk
   integer :: n, k, chunk___
   real(dp) :: tmp, err1, err2
   chunk___ = 2; if (present(chunk)) chunk___ = max(chunk___,chunk)
   n = size(a)
   if (n <= chunk___) then
      s = sum_err_sp(a,err)
      return
   end if
   k = (n+1)/2
   s = psum_err(a(1:k),err1,chunk) + psum_err(a(k+1:n),err2,chunk)
   call random_number(tmp)
   err = err1 + err2 + (tmp-0.5)*spacing(s)
   end function

end module


