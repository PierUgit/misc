

module mykinds
use iso_fortran_env, only : int64
implicit none

private

integer, parameter ::  dp_pmin = 12,  dp_rmin = 100,  &
                       ep_pmin = 18,  ep_rmin = 100,  &
                      ddp_pmin = 24, ddp_rmin = 100,  &
                       qp_pmin = 24,  qp_rmin = 1000

integer, parameter :: rk_single = kind(0.0), &
                      rk_double = kind(0d0)

integer, parameter :: rk_sp = rk_single

logical, parameter :: &
    is_single_least_dp = precision(0.0) >= dp_pmin .and. range(0.0) >= dp_rmin
integer, parameter ::                                      &
    rk_dp = merge(rk_single,rk_double,is_single_least_dp), &
    rk_dp_precision = precision(0.0_rk_dp),                &
    rk_dp_range     = range(0.0_rk_dp)
    
logical, parameter :: &
    is_dp_least_ep = rk_dp_precision >= ep_pmin .and. rk_dp_range >= ep_rmin
integer, parameter ::                                       &
    ep___ = selected_real_kind(p=18,r=100),                 &
    rk_ep = merge(rk_dp,ep___,is_dp_least_ep .or. ep___<0), &
    rk_ep_precision = precision(0.0_rk_ep),                 &
    rk_ep_range     = range(0.0_rk_ep)

integer, parameter ::                                   &
    ddp___ = selected_real_kind(p=ddp_pmin,r=ddp_rmin), &
    rk_ddp = merge(ddp___,rk_ep,ddp___>0),              &
    rk_ddp_precision = precision(0.0_rk_ddp),           &
    rk_ddp_range     = range(0.0_rk_ddp)

integer, parameter ::                                 &
    qp___  = selected_real_kind(p=qp_pmin,r=qp_rmin), &
    rk_qp  = merge(qp___,rk_ddp,qp___>0),             &
    rk_qp_precision = precision(0.0_rk_qp),           &
    rk_qp_range     = range(0.0_rk_qp)

public :: rk_sp, rk_dp, rk_ep, rk_ddp, rk_qp

logical, parameter :: is_int_least15 = range(0) >= 15
integer, parameter :: ik_long = merge(kind(0),selected_int_kind(r=15),is_int_least15)

public :: ik_long

end module

