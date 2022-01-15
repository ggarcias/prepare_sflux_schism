module mod_nc_tools
use netcdf
implicit none

    contains
        subroutine check(status, operation)
            integer, intent(in) :: status
            character(len=*), intent(in) :: operation
            if (status == NF90_NOERR) return
            print *, "Error encountered during ", operation
            print *, nf90_strerror(status)
            STOP 1
        end subroutine check

end module
