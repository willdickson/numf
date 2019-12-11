module array_create_mod

    use kinds_mod, only : sp, dp, ip
    implicit none
    private

    public linspace, logspace

    interface linspace
        procedure :: linspace_sp
        procedure :: linspace_dp
    end interface linspace

    interface logspace
        procedure :: logspace_sp
        procedure :: logspace_dp
    end interface logspace

contains


    function linspace_sp(a, b, n) result(x)
        implicit none
        integer(kind=ip), parameter :: wp = sp
#include "linspace.inc"
    end function linspace_sp


    function linspace_dp(a, b, n) result(x)
        implicit none
        integer(kind=ip), parameter :: wp = dp
#include "linspace.inc"
    end function linspace_dp


    function logspace_sp(a, b, n, base) result(x)
        implicit none
        integer(kind=ip), parameter :: wp = sp
#include "logspace.inc"
    end function logspace_sp


    function logspace_dp(a, b, n, base) result(x)
        implicit none
        integer(kind=ip), parameter :: wp = dp
#include "logspace.inc"
    end function logspace_dp


end module array_create_mod
