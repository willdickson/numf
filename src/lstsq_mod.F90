module lstsq_mod

    use flab_kinds_mod, only : sp, dp, ip
    use rtn_info_mod,   only : rtn_info_t, on_error
    implicit none

    private

    public lstsq, rtn_info_t

    interface lstsq
        procedure :: lstsq_dp_mat
        procedure :: lstsq_sp_mat
        procedure :: lstsq_dp_vec
        procedure :: lstsq_sp_vec
    end interface lstsq

    interface lstsq_mat
        procedure :: lstsq_dp_mat
        procedure :: lstsq_sp_mat
    end interface lstsq_mat

    interface ggelsy 
        subroutine sgelsy(m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank, work, lwork, flag)
            import sp
            integer, intent(in)          :: m
            integer, intent(in)          :: n
            integer, intent(in)          :: nrhs
            integer, intent(in)          :: lda
            integer, intent(in)          :: ldb
            integer, intent(in)          :: lwork
            integer, intent(out)         :: flag
            integer, intent(out)         :: rank
            integer, intent(inout)       :: jpvt(*)
            real(kind=sp), intent(in)    :: rcond
            real(kind=sp), intent(inout) :: a(lda,*)
            real(kind=sp), intent(inout) :: b(ldb,*)
            real(kind=sp), intent(out)   :: work(*)
        end subroutine sgelsy

        subroutine dgelsy(m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank, work, lwork, flag)
            import dp
            integer, intent(in)          :: m
            integer, intent(in)          :: n
            integer, intent(in)          :: nrhs
            integer, intent(in)          :: lda
            integer, intent(in)          :: ldb
            integer, intent(in)          :: lwork
            integer, intent(out)         :: flag
            integer, intent(out)         :: rank
            integer, intent(inout)       :: jpvt(*)
            real(kind=dp), intent(in)    :: rcond
            real(kind=dp), intent(inout) :: a(lda,*)
            real(kind=dp), intent(inout) :: b(ldb,*)
            real(kind=dp), intent(out)   :: work(*)
        end subroutine dgelsy
    end interface ggelsy


contains

    subroutine lstsq_dp_mat(a,b,x,info)
        implicit none
        integer(kind=ip), parameter :: wp=dp
#include "lstsq_mat.inc"
    end subroutine lstsq_dp_mat


    subroutine lstsq_sp_mat(a,b,x,info)
        implicit none
        integer(kind=ip), parameter :: wp=sp
#include "lstsq_mat.inc"
    end subroutine lstsq_sp_mat


    subroutine lstsq_dp_vec(a,b,x,info)
        implicit none
        integer(kind=ip), parameter :: wp=dp
#include "lstsq_vec.inc"
    end subroutine lstsq_dp_vec


    subroutine lstsq_sp_vec(a,b,x,info)
        implicit none
        integer(kind=ip), parameter :: wp=sp
#include "lstsq_vec.inc"
    end subroutine lstsq_sp_vec

end module lstsq_mod
