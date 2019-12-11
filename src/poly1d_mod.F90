module poly1d_mod

    use kinds_mod,      only : sp, dp, ip
    use rtn_info_mod,   only : rtn_info_t, on_error
    use lstsq_mod,      only : lstsq

    implicit none
    private

    public polyfit, polyval

    interface polyfit
        procedure :: polyfit_sp
        procedure :: polyfit_dp
    end interface polyfit

    interface polyval
        procedure :: polyval_sp
        procedure :: polyval_dp
    end interface polyval


contains


    subroutine polyfit_sp(x, y, coef, info)
        implicit none
        integer(kind=ip), parameter :: wp = sp
#include "polyfit.inc"
    end subroutine polyfit_sp


    subroutine polyfit_dp(x, y, coef, info)
        implicit none
        integer(kind=ip), parameter :: wp = dp
#include "polyfit.inc"
    end subroutine polyfit_dp


    subroutine polyval_sp(x, y, coef, info) 
        implicit none
        integer(kind=ip), parameter :: wp = sp
#include "polyval.inc"
    end subroutine polyval_sp


    subroutine polyval_dp(x, y, coef, info) 
        implicit none
        integer(kind=ip), parameter :: wp = dp
#include "polyval.inc"
    end subroutine polyval_dp


end module poly1d_mod
