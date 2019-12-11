! logspace.inc

! Arguments
real(kind=wp), intent(in)            :: a
real(kind=wp), intent(in)            :: b
integer(kind=ip), intent(in)         :: n
real(kind=wp), intent(in), optional  :: base
real(kind=wp)                        :: x(n)

! Local variable
real(kind=wp)                        :: base_val

if (.not. present(base)) then
    base_val = 10.0
else
    base_val = base
end if
x = base_val**linspace(a,b,n)
