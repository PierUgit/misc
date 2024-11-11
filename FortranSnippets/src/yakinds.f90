!***********************************************************************************************
! Author: Pierre Hugonnet
! https://github.com/PierUgit
! License: GPL v3 
!***********************************************************************************************
! # Yet Another KINDS
!
! The module provides REAL / INTEGER / LOGICAL kind constants, based on three principles:
! - The kinds are based on precision and range requirements, without any reference
!   to the sizes in memory (apart for the LOGICAL kinds), so that the codes can focus
!   on the numerical properties only.
! - The default kinds (kind(0.0), kind(0d0), kind(0), kind(.false.)) are given priority
!   as long as they fit the requirements, so that it maximizes the compatibility with the
!   legacy librairies that typically use these kinds
! - The kinds are always defined (>0): if the requirements for a kind cannot be
!   met, the kind with the requirements that are just below is selected.  
!   The advantage of this approach is that codes that use these kinds can always 
!   be compiled. The downside is that one has to test the actual precision/range at 
!   runtime and make a decision.
!
! ## REAL kind constants
! - r_default : the default real (kind(1.0))
! - r_doubleprecision : the "double precision" real (kind(1d0))
! - r_half         (half precision):          precision >= 3,  range >= 5     
! - r_single       (single precision):        precision >= 6,  range >= 37    **
! - r_sesqui       (1.5x precision):          precision >= 10, range >= 37    **
! - r_double       (double precision):        precision >= 12, range >= 100   * x
! - r_extended     (extended precision):      precision >= 18, range >= 100   * x
! - r_doubledouble (double-double precision): precision >= 24, range >= 100     x
! - r_quad         (quadruple precision):     precision >= 24, range >= 1000    x
! 
! ** : these kinds are necessarily either r_default or r_doubleprecision
! * : these kinds are r_default or r_doubleprecision in priority (if possible)
! x : no guarantee to meet the requirements
!
! Note that Fortran standard:
! - *recommends* precision(0.0) >= 6,  range(0.0) >= 37
! - *requires*   precision(0d0) >= 10, range(0d0) >= 37
!
! ### Fallbacks
! - r_double  falls back to r_sesqui if the requirements are not met
! - r_extended  falls back to r_double if the requirements are not met
! - r_doubledouble falls back to r_extended if the requirements are not met
! - r_quad  falls back to r_doubledouble if the requirements are not met
!     
! ## INTEGER kind constants
! - i_default : the default integer (kind(0))
! - i_shorter :         range >= 2
! - i_short :           range >= 4
! - i_medium :          range >= 9    *
! - i_long :            range >= 18   *
! - i_longer :          range >= 36     x
!
! * : these kinds are i_default in priority (if possible)
! x : no guarantee to meet the requirements
!
! Note that the standard requires the compilers to provide an integer with a range >= 18.
! Consequently, only i_longer is not guaranteed to meet the requirements (in that case, 
! it falls back to i_long).
!
! ## LOGICAL kind constants
! - l_default (the default kind)
! - l_short, which is currently the c_bool kind from the iso_c_binding module
!***********************************************************************************************
module yakinds
use iso_c_binding, only : c_bool
implicit none

   private

   integer, parameter ::  hp_pmin = 3,   hp_rmin = 4,    &
                          sp_pmin = 6,   sp_rmin = 37,   &
                         sep_pmin = 10, sep_rmin = 37,   &
                          dp_pmin = 12,  dp_rmin = 100,  &
                          ep_pmin = 18,  ep_rmin = 100,  &
                         ddp_pmin = 24, ddp_rmin = 100,  &
                          qp_pmin = 24,  qp_rmin = 1000

   integer, parameter :: r_default = kind(0.0), &
                         r_doubleprecision = kind(0d0)

   ! determining r_half
   integer, parameter :: r_half = selected_real_kind(p=hp_pmin,r=hp_rmin)

   ! determining r_single
   logical, parameter :: &
      is_def_least_sp = precision(0.0) >= sp_pmin .and. range(0.0) >= sp_rmin
   integer, parameter :: r_single = merge(r_default,r_doubleprecision,is_def_least_sp)

   ! determining r_sesqui
   logical, parameter :: &
      is_def_least_sep = precision(0.0) >= sep_pmin .and. range(0.0) >= sep_rmin
   integer, parameter :: r_sesqui = merge(r_default,r_doubleprecision,is_def_least_sep)
    
   ! determining r_double
   logical, parameter :: &
      is_sep_least_dp = precision(0.0_r_sesqui) >= dp_pmin .and. &
                        range(0.0_r_sesqui) >= dp_rmin
   integer, parameter ::                                             &
      dp___ = selected_real_kind(p=dp_pmin,r=dp_rmin),               &
      r_double = merge(r_sesqui,dp___,is_sep_least_dp .or. dp___<0)

   ! determining r_extended
   logical, parameter :: &
      is_dp_least_ep = precision(0.0_r_double) >= ep_pmin .and. &
                       range(0.0_r_double) >= ep_rmin
   integer, parameter ::                                              &
      ep___ = selected_real_kind(p=18,r=100),                         &
      r_extended = merge(r_double,ep___,is_dp_least_ep .or. ep___<0)

   ! determining r_doubledouble
   integer, parameter ::                                     &
      ddp___ = selected_real_kind(p=ddp_pmin,r=ddp_rmin),    &
      r_doubledouble = merge(ddp___,r_extended,ddp___>0)

   ! determining r_quad
   integer, parameter ::                                &
      qp___  = selected_real_kind(p=qp_pmin,r=qp_rmin), &
      r_quad  = merge(qp___,r_doubledouble,qp___>0)

   public :: r_default, r_doubleprecision
   public :: r_half, r_single, r_sesqui, r_double, r_extended, r_doubledouble, r_quad



   integer, parameter :: i_default = kind(0)
   
   ! determining i_xshort
   integer, parameter :: i_shorter = selected_int_kind(r=2)
   
   ! determining i_short
   integer, parameter :: i_short = selected_int_kind(r=4)

   ! determining i_medium
   logical, parameter :: is_def_least9 = range(0) >= 9
   integer, parameter :: medium___ = selected_int_kind(r=9)
   integer, parameter :: i_medium = merge(kind(0),medium___,is_def_least9)

   ! determining i_long
   logical, parameter :: is_def_least18 = range(0) >= 18
   integer, parameter :: long___ = selected_int_kind(r=18)
   integer, parameter :: i_long = merge(kind(0),long___,is_def_least18)

   ! determining i_longer
   integer, parameter :: longer___ = selected_int_kind(r=36)
   integer, parameter :: i_longer = merge(longer___,i_long,longer___>0)

   public :: i_default, i_shorter, i_short, i_medium, i_long, i_longer 



   integer, parameter :: l_default = kind(.true.)
   integer, parameter :: l_short   = c_bool

   public :: l_default, l_short

end module yakinds

