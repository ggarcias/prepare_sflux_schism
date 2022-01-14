module mod_const
    use mod_precision
    implicit none

    integer(sp), parameter :: resolution= 1

    real(kind=sp) :: aux_f = -32767.0
    real(sp), parameter :: nan_f = transfer(-4194304_int32, 1._real32)
    real(dp), parameter :: nan_d = transfer(-4194304_int64, 1._real64)
    real(dp), parameter :: Rr = 6371008.7714  !Earth radius in metres
    real(dp), parameter :: pi= 4.0*atan(1.0)


    ! constants netcdf
end module
