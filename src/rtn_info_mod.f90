module rtn_info_mod

    use flab_kinds_mod, only : ip
    implicit none

    private

    public rtn_info_t, on_error

    type rtn_info_t
        logical                       :: success 
        character(len=:), allocatable :: message 
    contains
        procedure :: print => print_rtn_info
    end type rtn_info_t

contains

    subroutine print_rtn_info(this)
        class(rtn_info_t), intent(in) :: this 
            print *, ''
            print '(A,L)', 'success: ', this%success
            print '(A,A)', 'message: ', this%message
    end subroutine print_rtn_info

    subroutine on_error(msg, fname, line, info)
        implicit none
        ! Arguments
        character(len=*), intent(in)            :: msg
        character(len=*), intent(in)            :: fname
        integer(kind=ip), intent(in)            :: line
        type(rtn_info_t), intent(out), optional :: info 
        ! Local variables
        character(len=255)                      :: full_msg
        write (full_msg,'(A,A,A,I0,A,A)') 'error: file ', fname, ', line ', line, ', ', msg
        if (present(info)) then
            info%success = .false.
            info%message = trim(full_msg)
        else 
            print *, full_msg
            stop 
        end if
    end subroutine on_error

end module rtn_info_mod
