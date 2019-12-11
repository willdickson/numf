! Arguments
real(kind=wp), intent(in)    :: a
real(kind=wp), intent(in)    :: b
integer(kind=ip), intent(in) :: n
real(kind=wp)                :: x(n)

! Local variables
integer(kind=ip)             :: i
real(kind=wp)                :: dx

dx = (b-a)/real(n-1,kind=sp)
do i=1,n-1
    x(i) = a + (i-1)*dx
end do
x(n) = b
