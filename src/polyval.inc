! polyval.inc
!
! subroutine polyval(x, y, coef, info) 
!    implicit none
!    integer(kind=ip), parameter :: wp = sp or wp = dp
!
! ---------------------------------------------------------------------------------------

    ! Arguments
    real(kind=wp), intent(in)                :: coef(:)
    real(kind=wp), intent(in)                :: x(:)
    real(kind=wp), intent(out)               :: y(:)
    type(rtn_info_t), intent(out), optional  :: info
    
    ! Local variables
    real(kind=wp), allocatable    :: a(:,:)
    integer(kind=ip)              :: num_eqn
    integer(kind=ip)              :: num_prm
    integer(kind=ip)              :: flag
    integer(kind=ip)              :: i
    
    if (size(coef) <= 1) then
        call on_error('size(coef) <= 1', __FILE__, __LINE__, info)
        return
    end if
    
    if (size(x) /= size(y)) then
        call on_error('size(x) /= size(y)', __FILE__, __LINE__, info)
        return
    end if
    
    if (size(x) < size(coef)) then
        call on_error('size(x) <= size(coef)', __FILE__, __LINE__, info)
        return
    end if
    
    num_eqn = size(x)
    num_prm = size(coef)
    
    allocate(a(num_eqn, num_prm), stat=flag)
    if (flag /= 0) then
        call on_error('a memory allocation failed', __FILE__, __LINE__, info)
        return
    end if
    
    do i=1,num_prm
        a(:,i) = x**(num_prm-i)
    end do
    
    y = matmul(a,x)
    
    if (present(info)) then
        info%success = .true.
        info%message = ''
    end if

! ---------------------------------------------------------------------------------------
! end subroutine polyval
