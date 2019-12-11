program test_lapack

    use flab_kinds_mod, only : sp, dp, ip
    use rtn_info_mod, only : rtn_info_t, on_error
    use lstsq_mod, only : lstsq
    use array_create_mod, only : logspace
    use poly1d_mod, only : polyfit

    implicit none

    integer(kind=ip), parameter         :: wp = sp
    integer(kind=ip), parameter         :: n =10
    integer(kind=ip), parameter         :: m = 2
    real(kind=wp)                       :: x(n)
    real(kind=wp)                       :: y(n)
    real(kind=wp)                       :: coef(m)
    type(rtn_info_t)                    :: info
    integer(kind=ip)                    :: i

    real(kind=wp)                       :: z(n)

    x = [(1.0*i, i=1,n)]
    y = 5.0*x

    call polyfit(x,y,coef,info)

    do i=1,m
        print *, i, coef(i)
    end do
    print *, ''

    z = logspace(1.0,3.0,n)
    do i=1,n
        print *, i, z(i)
    end do

contains
    

end program test_lapack


