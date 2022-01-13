program PREPARE_SFLUX
    use mod_const
    use mod_precision
    use mod_datetime
    use mod_nc_tools
    use netcdf
    implicit none

    character(len=100) :: sflux_name

    integer :: HereError
    integer :: lon, lat

    real (kind = dp) :: diff_julian_time


    namelist /sflux/ sflux_name
     ! time_window, spill, oil_centre, radius, mu, dt, tstart, tend

    OPEN(UNIT=100, FILE='namelist.sflux', ACTION='READ', IOSTAT = HereError,  STATUS = 'UnkNOWN')
    READ(100, NML = sflux)
    CLOSE(100)

    lon = 10
    lat = 10
    call create_sflux_ncfile(sflux_name, lon, lat) 
    print *, "============ DONE ================"



end program PREPARE_SFLUX
