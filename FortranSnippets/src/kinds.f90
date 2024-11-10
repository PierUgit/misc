!***********************************************************************************************
! Author: Pierre Hugonnet
! https://github.com/PierUgit
! License: GPL v3 
!***********************************************************************************************
! (hopefully) useful real and integer kind values
!
! For the real type the module defines the following kinds:
! - rk_default and rk_doubleprecision for the default real and "double precision"
! - rk_single       (single precision):        precision >= 6,  range >= 37
! - rk_weakdouble   (weak double precision):   precision >= 10, range >= 37
! - rk_double       (double precision):        precision >= 12, range >= 307  (*)
! - rk_extended     (extened precision):       precision >= 18, range >= 100  (*)
! - rk_doubledouble (double double precision): precision >= 24, range >= 100  (*)
! - rk_quad         (quadruple precision):     precision >= 24, range >= 1000 (*)
! 
! (*) note that these kinds are always defined, even if the requirements are not met.
!     rk_double  falls back to rk_weakdouble if no kind meets the requirements
!     rk_extended  falls back to rk_double  if no kind meets the requirements
!     rk_doubledouble falls back to rk_extended  if no kind meets the requirements
!     rk_quad  falls back to rk_doubledouble if no kind meets the requirements
!     The advantage of this approach is that codes that use these kind constants will always 
!     compile. The downside is that one has to test the actual precision/range and 
!     take a decision at the execution. To make it easier, the module also provides the
!     constants rk_double_prec,  rk_double_range,  rk_extended_prec, rk_extended_range, 
!               rk_doubledouble_prec, rk_doubledouble_range, rk_quad_prec, rk_quad_range
!     
! Note that Fortran standard only *recommends* precision(0.0) >= 6,  range(0.0) >= 37
!                             but *requires*   precision(0d0) >= 10, range(0d0) >= 37
! Consequentely, rk_single and rk_weakdouble always meet the requirements 
! and are *always* either rk_default or rk_doubleprecision
!
! It also provides alternatives to the kinds real32 and real64, the difference being that
! these kinds have likely *at least* (instead of exactly) 32 or 64 bits
! - rk_sortof32
! - rk_sortof64
!
! For the integer type the module provides the kinds
! - ik_default (the default kind)
! - ik_long, which is 
!   - the default kind if it has a range >= 18
!   - selected_int_kind(r=18) otherwise
! - ik_longer, which is
!   - selected_int_kind(r=36) is it exists
!   - ik_long otherwise
!
! It also provides alternatives to the kinds int32 and int64, the difference being that
! these kinds have likely *at least* (instead of exactly) 32 or 64 bits
! - ik_sortof32
! - ik_sortof64
!
! Note that the standard requires the compilers to provide an integer with a range >= 18
!
! For the logical type the module provides the kinds
! - lk_default (the default kind)
! - lk_short, which is currently the c_bool kind from the iso_c_binding module
! - lk_shortest (not yet)
!***********************************************************************************************
module mykinds
use iso_c_binding, only : c_bool
implicit none

private

integer, parameter ::  sp_pmin = 6,   sp_rmin = 37,   &
                      wdp_pmin = 10, wdp_rmin = 37,  &
                       dp_pmin = 12,  dp_rmin = 100,  &
                       ep_pmin = 18,  ep_rmin = 100,  &
                      ddp_pmin = 24, ddp_rmin = 100,  &
                       qp_pmin = 24,  qp_rmin = 1000

integer, parameter :: rk_default = kind(0.0), &
                      rk_doubleprecision = kind(0d0)

! determining rk_single
logical, parameter :: &
   is_default_least_sp = precision(0.0) >= sp_pmin .and. range(0.0) >= sp_rmin
integer, parameter :: rk_single = merge(rk_default,rk_doubleprecision,is_default_least_sp)

! determining rk_weakdouble
logical, parameter :: &
   is_def_least_wdp = precision(0.0) >= wdp_pmin .and. range(0.0) >= wdp_rmin
integer, parameter ::                                       &
   rk_weakdouble = merge(rk_default,rk_doubleprecision,is_def_least_wdp), &
   rk_weakdouble_prec = precision(0.0_rk_weakdouble),                     &
   rk_weakdouble_range = range(0.0_rk_weakdouble)
    
! determining rk_double
logical, parameter :: &
   is_wdp_least_dp = rk_weakdouble_prec >= dp_pmin .and. rk_weakdouble_range >= dp_rmin
integer, parameter ::                                        &
   dp___ = selected_real_kind(p=dp_pmin,r=dp_rmin),          &
   rk_double = merge(rk_weakdouble,dp___,is_wdp_least_dp .or. dp___<0), &
   rk_double_prec  = precision(0.0_rk_double),                       &
   rk_double_range = range(0.0_rk_double)

! determining rk_extended
logical, parameter :: &
   is_dp_least_ep = rk_double_prec >= ep_pmin .and. rk_double_range >= ep_rmin
integer, parameter ::                                      &
   ep___ = selected_real_kind(p=18,r=100),                 &
   rk_extended = merge(rk_double,ep___,is_dp_least_ep .or. ep___<0), &
   rk_extended_prec  = precision(0.0_rk_extended),                     &
   rk_extended_range = range(0.0_rk_extended)

! determining rk_doubledouble
integer, parameter ::                                  &
   ddp___ = selected_real_kind(p=ddp_pmin,r=ddp_rmin), &
   rk_doubledouble = merge(ddp___,rk_extended,ddp___>0),              &
   rk_doubledouble_prec  = precision(0.0_rk_doubledouble),               &
   rk_doubledouble_range = range(0.0_rk_doubledouble)

! determining rk_quad
integer, parameter ::                                &
   qp___  = selected_real_kind(p=qp_pmin,r=qp_rmin), &
   rk_quad  = merge(qp___,rk_doubledouble,qp___>0),             &
   rk_quad_prec  = precision(0.0_rk_quad),               &
   rk_quad_range = range(0.0_rk_quad)

public :: rk_single, rk_weakdouble, rk_double, rk_extended, rk_doubledouble, rk_quad
public :: rk_double_prec, rk_double_range,   &
          rk_extended_prec, rk_extended_range,   &
          rk_doubledouble_prec, rk_doubledouble_range, &
          rk_quad_prec, rk_quad_range

integer, parameter :: rk_sortof32 = rk_single
integer, parameter :: rk_sortof64 = rk_double

public :: rk_sortof32, rk_sortof64



integer, parameter :: ik_default = kind(0)

! determining ik_long
logical, parameter :: is_def_least18 = range(0) >= 18
integer, parameter :: ik_long___ = selected_int_kind(r=18)
integer, parameter :: ik_long = merge(kind(0),ik_long___,is_def_least18)
integer, parameter :: ik_long_range = range(0_ik_long)

! determining ik_longer
integer, parameter :: ik_longer___ = selected_int_kind(r=36)
integer, parameter :: ik_longer = merge(ik_longer___,ik_long,ik_longer___>0)
integer, parameter :: ik_longer_range = range(0_ik_longer)

public :: ik_default, ik_long, ik_longer, ik_long_range, ik_longer_range

integer, parameter :: ik_sortof32 = selected_int_kind(r=9)
integer, parameter :: ik_sortof64 = ik_long

public :: ik_sortof32, ik_sortof64


integer, parameter :: lk_default = kind(.true.)
integer, parameter :: lk_short   = c_bool

public :: lk_default, lk_short

end module

