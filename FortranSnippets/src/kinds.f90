!***********************************************************************************************
! Author: Pierre Hugonnet
! https://github.com/PierUgit
! License: GPL v3 
!***********************************************************************************************
! (hopefully) useful real and integer kind values
!
! For the real type the module defines the following kinds:
! - rk_single and rk_double for the default real and "double precision"
! - rk_sp  (single precision):        precision >= 6,  range >= 37
! - rk_dpw (weak double precision):   precision >= 10, range >= 37
! - rk_dp  (double precision):        precision >= 12, range >= 307  (*)
! - rk_ep  (extened precision):       precision >= 18, range >= 100  (*)
! - rk_ddp (double double precision): precision >= 24, range >= 100  (*)
! - rk_qp  (quadruple precision):     precision >= 24, range >= 1000 (*)
! 
! (*) note that these kinds are always defined, even if the requirements are not met.
!     rk_dp  falls back to rk_dpw if no kind meets the requirements
!     rk_ep  falls back to rk_dp  if no kind meets the requirements
!     rk_ddp falls back to rk_ep  if no kind meets the requirements
!     rk_qp  falls back to rk_ddp if no kind meets the requirements
!     This advantage of this approach is that codes that use these kind constants will always 
!     compile. The downside is that one has to test the actual precision/range and 
!     take a decision at the execution. To make it easier, the module also provides the
!     constants rk_dp_prec,  rk_dp_range,  rk_ep_prec, rk_ep_range, 
!               rk_ddp_prec, rk_ddp_range, rk_qp_prec, rk_qp_range
!     
! Note that Fortran standard only *recommends* precision(0.0) >= 6,  range(0.0) >= 37
!                             but *requires*   precision(0d0) >= 10, range(0d0) >= 37
! Consequentely, rk_sp and rk_dpw always meet the requirements 
! and are *always* either rk_single or rk_double
!
! For the integer type the module provides the kind ik_long, which is 
! - either the default kind if it has a range >= 18
! - selected_int_kind(r=18) otherwise
!
! Note that the standard requires the compilers to provide an integer with a range >= 18
!
! For the logical type the module provides the kind lk_short, which is currently
! the c_bool kind from the iso_c_binding module
!***********************************************************************************************
module mykinds
use iso_c_binding, only : c_bool
implicit none

private

integer, parameter ::  sp_pmin = 6,   sp_rmin = 37,   &
                      dpw_pmin = 10, dpw_rmin = 37,  &
                       dp_pmin = 12,  dp_rmin = 100,  &
                       ep_pmin = 18,  ep_rmin = 100,  &
                      ddp_pmin = 24, ddp_rmin = 100,  &
                       qp_pmin = 24,  qp_rmin = 1000

integer, parameter :: rk_single = kind(0.0), &
                      rk_double = kind(0d0)

! determining rk_sp
logical, parameter :: &
   is_single_least_sp = precision(0.0) >= sp_pmin .and. range(0.0) >= sp_rmin
integer, parameter :: rk_sp = merge(rk_single,rk_double,is_single_least_sp)

! determining rk_dpw
logical, parameter :: &
   is_single_least_dpw = precision(0.0) >= dpw_pmin .and. range(0.0) >= dpw_rmin
integer, parameter ::                                       &
   rk_dpw = merge(rk_single,rk_double,is_single_least_dpw), &
   rk_dpw_prec = precision(0.0_rk_dpw),                     &
   rk_dpw_range = range(0.0_rk_dpw)
    
! determining rk_dp
logical, parameter :: &
   is_dpw_least_dp = rk_dpw_prec >= dp_pmin .and. rk_dpw_range >= dp_rmin
integer, parameter ::                                        &
   dp___ = selected_real_kind(p=dp_pmin,r=dp_rmin),          &
   rk_dp = merge(rk_dpw,dp___,is_dpw_least_dp .or. dp___<0), &
   rk_dp_prec  = precision(0.0_rk_dp),                       &
   rk_dp_range = range(0.0_rk_dp)

! determining rk_ep
logical, parameter :: &
   is_dp_least_ep = rk_dp_prec >= ep_pmin .and. rk_dp_range >= ep_rmin
integer, parameter ::                                      &
   ep___ = selected_real_kind(p=18,r=100),                 &
   rk_ep = merge(rk_dp,ep___,is_dp_least_ep .or. ep___<0), &
   rk_ep_prec  = precision(0.0_rk_ep),                     &
   rk_ep_range = range(0.0_rk_ep)

! determining rk_ddp
integer, parameter ::                                  &
   ddp___ = selected_real_kind(p=ddp_pmin,r=ddp_rmin), &
   rk_ddp = merge(ddp___,rk_ep,ddp___>0),              &
   rk_ddp_prec  = precision(0.0_rk_ddp),               &
   rk_ddp_range = range(0.0_rk_ddp)

! determining rk_qp
integer, parameter ::                                &
   qp___  = selected_real_kind(p=qp_pmin,r=qp_rmin), &
   rk_qp  = merge(qp___,rk_ddp,qp___>0),             &
   rk_qp_prec  = precision(0.0_rk_qp),               &
   rk_qp_range = range(0.0_rk_qp)

public :: rk_sp, rk_dpw, rk_dp, rk_ep, rk_ddp, rk_qp
public :: rk_dp_prec, rk_dp_range,   &
          rk_ep_prec, rk_ep_range,   &
          rk_ddp_prec, rk_ddp_range, &
          rk_qp_prec, rk_qp_range


integer, parameter :: ik_default = kind(0)

! determining ik_long
logical, parameter :: is_int_least18 = range(0) >= 18
integer, parameter :: ik_long___ = selected_int_kind(r=18)
integer, parameter :: ik_long = merge(kind(0),ik_long___,is_int_least18)
integer, parameter :: ik_long_range = range(0_ik_long)

! determining ik_verylong
logical, parameter :: is_int_least18 = range(0) >= 36
integer, parameter :: ik_verylong___ = selected_int_kind(r=36)
integer, parameter :: ik_verylong = merge(ik_verylong___,ik_long,ik_verylong___>0)
integer, parameter :: ik_verylong_range = range(0_ik_verylong)

public :: ik_default, ik_long, ik_verylong, ik_long_range, ik_verylong_range


integer, parameter :: lk_default = kind(.true.)
integer, parameter :: lk_short   = c_bool

public :: lk_default, lk_shortest

end module

