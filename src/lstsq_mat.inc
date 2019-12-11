! lstsq_mat.inc
!
! subroutine lstsq_mat(a,b,x,info)
!     implicit none
!     integer(kind=ip), parameter :: wp=dp
!
! ---------------------------------------------------------------------------------------

    ! Subroutine arguments
    real(kind=wp), intent(in)               :: a(:,:)
    real(kind=wp), intent(in)               :: b(:,:)
    real(kind=wp), intent(out)              :: x(:,:)
    type(rtn_info_t), intent(out), optional :: info
    
    ! Local variables
    integer(kind=ip)               :: m          ! number rows a
    integer(kind=ip)               :: n          ! number cols a
    integer(kind=ip)               :: nrhs       ! number cols b 
    integer(kind=ip)               :: lda        ! leading dim of a
    integer(kind=ip)               :: ldb        ! leading dim of b
    real(kind=wp), allocatable     :: a_tmp(:,:) ! temporary a (modified by lapack)
    real(kind=wp), allocatable     :: b_tmp(:,:) ! temporary b (modified by lapack
    real(kind=wp), allocatable     :: work(:)    ! working array (used by lapack)
    integer(kind=ip), allocatable  :: jpvt(:)    ! pivoting information
    real(kind=wp)                  :: rcond      ! use to determine rank a
    integer(kind=ip)               :: rank       ! rank a
    integer(kind=ip)               :: lwork      ! size of work array
    integer(kind=ip)               :: flag       ! error flag 
    
    m = size(a,1)
    n = size(a,2)
    nrhs = size(b,2)
    lda = size(a,1)
    ldb = size(b,1)
    rcond = -1.0
    
    if ( present(info) ) then
        info%success = .true.
        info%message = ''
    end if
    
    ! Check that a has correct shape 
    if (m < n) then
        call on_error('size(a,1) < size(a,2)', __FILE__, __LINE__, info)
        return
    end if
    
    ! Check compatibility of a and b matrices
    if (m /= size(b,1)) then
        call on_error('size(a,1) /= size(b,1)', __FILE__, __LINE__, info)
        return
    end if 
    
    ! Check compatibility of a and x matrices 
    if (n /= size(x,1)) then
        call on_error('size(a,2) /= size(x,1)', __FILE__, __LINE__, info)
        return
    end if
    
    ! Allocate working a,b and pivot arrays
    allocate(a_tmp(m,n), stat=flag)
    if (flag /= 0) then
        call on_error('a_tmp memory allocation failed', __FILE__, __LINE__, info)
        return
    end if
    allocate(b_tmp(m,nrhs), stat=flag)
    if (flag /= 0) then
        call on_error('b_tmp memory allocation failed', __FILE__, __LINE__, info)
        return
    end if
    allocate(jpvt(n), stat=flag)
    if (flag /= 0) then
        call on_error('jpvt memory allocation failed', __FILE__, __LINE__, info)
        return 
    end if
    allocate(work(1), stat=flag)
    if (flag /= 0) then
        call on_error('work(1) memory allocation failed', __FILE__, __LINE__, info)
        return 
    end if
    
    a_tmp = a
    b_tmp = b
    jpvt = 0
    
    ! Call ggelsy with lwork =-1 to  determine optimal size of work array. 
    ! Optimal size returned in work(1)
    lwork = -1
    call ggelsy(m, n, nrhs, a_tmp, lda, b_tmp, ldb, jpvt, rcond, rank, work, lwork, flag)
    lwork = int(work(1),kind=ip)
    deallocate(work)
    allocate(work(lwork),stat=flag)
    if (flag /= 0) then
        call on_error('work(lwork) memory allocation failed', __FILE__, __LINE__, info)
        return
    end if
    
    ! Call ggelsy to solve linear system, b_tmp(1:n)  overwritten  with solution
    call ggelsy(m, n, nrhs, a_tmp, lda, b_tmp, ldb, jpvt, rcond, rank, work, lwork, flag)
    if (flag /= 0) then
        call on_error('ggelsy illegal argument', __FILE__, __LINE__, info)
        return
    end if
    x(1:n,1) = b_tmp(1:n,1)

! ---------------------------------------------------------------------------------------
! end subroutine lstsq_mat
