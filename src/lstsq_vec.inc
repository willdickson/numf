! lstsq_vec.inc

! Subroutine arguments
real(kind=wp), intent(in)               :: a(:,:)
real(kind=wp), intent(in)               :: b(:)
real(kind=wp), intent(out)              :: x(:)
type(rtn_info_t), intent(out), optional :: info

! Local variables
real(kind=wp), allocatable              :: b_tmp(:,:) 
real(kind=wp), allocatable              :: x_tmp(:,:) 
integer(kind=ip)                        :: flag

if ( present(info) ) then
    info%success = .true.
    info%message = '' 
end if

if ( size(a,1) /= size(b) ) then
    call on_error('size(a,1) /= size(b)',__FILE__, __LINE__, info)
    return
end if 

allocate(b_tmp(size(a,1),1),stat=flag)
if (flag /= 0) then
    call on_error('b_tmp memory allocation failed', __FILE__, __LINE__, info)
    return
end if

allocate(x_tmp(size(x),1),stat=flag)
if (flag /= 0) then
    call on_error('b_tmp memory allocation failed', __FILE__, __LINE__, info)
    return
end if

b_tmp(:,1) = b 
x_tmp = 0.0
call lstsq_mat(a,b_tmp,x_tmp,info)
x = x_tmp(:,1)
