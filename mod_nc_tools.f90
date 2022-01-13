module mod_nc_tools
    use mod_precision
    use mod_const
    use mod_datetime
    use netcdf
    implicit none

    ! -------------------------------------------------------------------------
    ! Set ID's
    ! -------------------------------------------------------------------------
    integer :: ID_lon, ID_lonf, ID_time
    integer :: ID_lat, ID_latf, ID_spherical
    integer :: ID_u10, ID_v10, ID_sflux_time
    integer :: ID_lonnf, ID_lattf, ID_stmp, ID_spfh
    integer :: ID_siceconc, ID_msl

    contains

    subroutine check(status, operation)
        integer, intent(in) :: status
        character(len=*), intent(in) :: operation
        if (status == NF90_NOERR) return
        print *, "Error encountered during ", operation
        print *, nf90_strerror(status)
        STOP 1
    end subroutine check

    subroutine create_sflux_ncfile(ncname, lon, lat)
        implicit none

        ! ---------------------------------------------------------------------------------
        ! Initalize dimensions and variables of the NetCDF which contains the atmospheric 
        ! forcing
        ! ---------------------------------------------------------------------------------
        integer, intent(in) :: lon, lat 
        character (len=*), intent(in) :: ncname

        integer :: ncid

        real(kind=sp) :: aux_f = -32767.0

        type(datetime) :: datenow 
        
        ! Assigns current machine time to datenow
        datenow = datenow % now()

        ! Create the netCDF file. The nf90_clobber parameter tells netCDF to
        ! overwrite this file, if it already exists.
        call check( nf90_create(ncname, NF90_CLOBBER, ncid), "Create NetCDF sflux")

        ! --------------------------------------------------------------------------
        ! Define dimensions.
        ! --------------------------------------------------------------------------

        ! Define the dimensions. NetCDF will hand back an ID for each.
        call check( nf90_def_dim(ncid, "longitude", lon, id_lon), "define lon")

        call check( nf90_def_dim(ncid, "latitude", lat, id_lat), "define lat")

        call check( nf90_def_dim(ncid, "time", NF90_UNLIMITED, ID_time), &
            "define time")

        ! --------------------------------------------------------------------------
        ! Create global attributes.
        ! --------------------------------------------------------------------------

        call check(nf90_put_att(ncid, NF90_GLOBAL, 'title', "Atmospheric forcing input file: SCHISM format"),&
            "add global attribute")
        call check(nf90_put_att(ncid, NF90_GLOBAL, 'institution', "JRC"), &
            "add global attribute")
        call check(nf90_put_att(ncid, NF90_GLOBAL, 'source', "ERA5 data"), & 
            "add global attribute")
        call check(nf90_put_att(ncid, NF90_GLOBAL, 'history', datenow % isoformat()),&
            "add global attribute")
        call check(nf90_put_att(ncid, NF90_GLOBAL, 'comment', '' ),&
            "add global attribute")
        call check(nf90_put_att(ncid, NF90_GLOBAL, 'conventions', "SCHISM"),&
            "add global attribute")

        ! --------------------------------------------------------------------------
        ! Define configuration variables.
        ! --------------------------------------------------------------------------
        ! latitude
        call check( nf90_def_var(ncid, "latitude", NF90_FLOAT, (/ ID_lat /), ID_latf), "define variable lat")
        call check( nf90_put_att(ncid, ID_latf, "standard_name", "latitude" ), "define attributes")
        call check( nf90_put_att(ncid, ID_latf, "long_name", "latitude" ), "define attributes")
        call check( nf90_put_att(ncid, ID_latf, "units", "degree_north" ), "define attributes")
        call check( nf90_put_att(ncid, ID_latf, "axis", "Y" ), "define attributes")
        call check( nf90_put_att(ncid, ID_latf, "_FillValue", nan_f), "define attributes")

        ! longitude
        call check( nf90_def_var(ncid, "longitude", NF90_FLOAT, (/ ID_lon /), ID_lonf), "define variable lat")
        call check( nf90_put_att(ncid, ID_lonf, "standard_name", "longitude" ), "define attributes")
        call check( nf90_put_att(ncid, ID_lonf, "long_name", "longitude" ), "define attributes")
        call check( nf90_put_att(ncid, ID_lonf, "units", "degree_east" ), "define attributes")
        call check( nf90_put_att(ncid, ID_lonf, "axis", "X" ), "define attributes")
        call check( nf90_put_att(ncid, ID_lonf, "_FillValue", nan_f), "define attributes")

        ! time
        call check( nf90_def_var(ncid, "time", NF90_DOUBLE, (/ ID_time /), ID_sflux_time), "define variable sms_time")
        call check( nf90_put_att(ncid, ID_sflux_time, "standard_name", "time" ), "define attributes")
        call check( nf90_put_att(ncid, ID_sflux_time, "long_name", "time" ), "define attributes")
        call check( nf90_put_att(ncid, ID_sflux_time, "axis", "T" ), "define attributes")
        call check( nf90_put_att(ncid, ID_sflux_time, "units", "days since 2000-01-01" ), "define attributes")
        call check( nf90_put_att(ncid, ID_sflux_time, "calendar", "standard"), "define attributes")
        call check( nf90_put_att(ncid, ID_sflux_time, "base_date", (/2000, 1, 1, 0/)), "define attributes")
        call check( nf90_put_att(ncid, ID_sflux_time, "_FillValue", nan_d), "define attributes")

        !u10
        call check( nf90_def_var(ncid, "u10", NF90_FLOAT, (/ ID_lon, ID_lat, ID_time /), ID_u10),&
            "define variable u10")
        call check( nf90_put_att(ncid, ID_u10, "long_name", "10 metre U wind component" ), &
            "define attributes")
        call check( nf90_put_att(ncid, ID_u10, "units", "m s**-1" ), "define attributes")
        call check( nf90_put_att(ncid, ID_u10, "_FillValue", aux_f), "define attributes")
        call check( nf90_put_att(ncid, ID_u10, "missing_value", aux_f), "define attributes")

        !v10
        call check( nf90_def_var(ncid, "v10", NF90_FLOAT, (/ ID_lon, ID_lat, ID_time /), ID_v10), &
            "define variable v10")
        call check( nf90_put_att(ncid, ID_v10, "long_name", "10 metre V wind component" ),&
            "define attributes")
        call check( nf90_put_att(ncid, ID_v10, "units", "m s**-1" ), "define attributes")
        call check( nf90_put_att(ncid, ID_v10, "_FillValue", aux_f ), "define attributes")
        call check( nf90_put_att(ncid, ID_v10, "missing_value", aux_f), "define attributes")

        !msl
        call check( nf90_def_var(ncid, "msl", NF90_FLOAT, (/ ID_lon, ID_lat, ID_time /), ID_msl),&
            "define variable msl")
        call check( nf90_put_att(ncid, ID_msl, "standard_name", "air_pressure_at_mean_sea_level" ),&
            "define attributes")
        call check( nf90_put_att(ncid, ID_msl, "long_name", "Mean sea level pressure" ), &
            "define attributes")
        call check( nf90_put_att(ncid, ID_msl, "units", "Pa" ), "define attributes")
        call check( nf90_put_att(ncid, ID_msl, "_FillValue", aux_f ), "define attributes")
        call check( nf90_put_att(ncid, ID_msl, "missing_value", aux_f), "define attributes")

        !siceconc
        call check( nf90_def_var(ncid, "siceconc", NF90_FLOAT, (/ ID_lon, ID_lat, ID_time /), &
            ID_siceconc), "define variable siceconc")
        call check( nf90_put_att(ncid, ID_siceconc, "standard_name", "sea_ice_area_fraction" ), &
            "define attributes")
        call check( nf90_put_att(ncid, ID_siceconc, "long_name", "Sea ice area fraction" ), &
            "define attributes")
        call check( nf90_put_att(ncid, ID_siceconc, "units", "(0 - 1)" ), "define attributes")
        call check( nf90_put_att(ncid, ID_siceconc, "_FillValue", aux_f ), "define attributes")
        call check( nf90_put_att(ncid, ID_siceconc, "missing_value", aux_f), "define attributes")

        ! lon
        call check( nf90_def_var(ncid, "lon", NF90_FLOAT, (/ ID_lon /), ID_lonnf), "define variable lon")
        call check( nf90_put_att(ncid, ID_lonnf, "_FillValue", nan_f ), "define attributes")

        ! lat
        call check( nf90_def_var(ncid, "lat", NF90_FLOAT, (/ ID_lat /), ID_lattf), "define variable lat")
        call check( nf90_put_att(ncid, ID_lattf, "_FillValue", nan_f ), "define attributes")

        ! spfh
        call check( nf90_def_var(ncid, "spfh", NF90_DOUBLE, (/ ID_lon, ID_lat /), ID_spfh), &
            "define variable spfh")
        call check( nf90_put_att(ncid, ID_spfh, "_FillValue", nan_d ), "define attributes")

        ! stmp
        call check( nf90_def_var(ncid, "stmp", NF90_DOUBLE, (/ ID_lon, ID_lat /), ID_stmp), &
            "define variable stmp")
        call check( nf90_put_att(ncid, ID_stmp, "_FillValue", nan_d ), "define attributes")

        ! End define mode. This tells netCDF we are done defining metadata.
        call check( nf90_enddef(ncid), "End definition sms netcdf file" )
    end subroutine

end module
