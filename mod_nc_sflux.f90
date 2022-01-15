module mod_nc_sflux
    use mod_precision
    use mod_const
    use mod_datetime
    use mod_strings
    use mod_nc_tools
    use netcdf
    implicit none

    ! -------------------------------------------------------------------------
    ! Set ID's
    ! -------------------------------------------------------------------------
    integer :: ID_lon, ID_lonf, ID_time
    integer :: ID_lat, ID_latf
    integer :: ID_u10, ID_v10, ID_sflux_time
    integer :: ID_lonnf, ID_lattf, ID_stmp, ID_spfh
    integer :: ID_siconc, ID_msl

    real(kind = dp) :: scale_factor, add_offset, fillvalue
    real(kind = dp), allocatable, dimension(:) :: latitude, longitude, time
    real(kind = dp), allocatable, dimension(:, :) :: latt, lonn, mask
    real(kind = dp), allocatable, dimension(:,:,:) :: u10, v10,&
        msl, siconc
    real(kind = dp), allocatable, dimension(:,:,:) :: auxx



    contains


    subroutine create_sflux_ncfile(ncname, lon, lat)
        implicit none

        ! ---------------------------------------------------------------------------------
        ! Initalize dimensions and variables of the NetCDF which contains the atmospheric 
        ! forcing
        ! ---------------------------------------------------------------------------------
        integer, intent(in) :: lon, lat 
        character (len=*), intent(in) :: ncname

        integer :: ncid


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

        call check(nf90_put_att(ncid, NF90_GLOBAL, "title", "Atmospheric forcing input file: SCHISM format"),&
            "add global attribute")
        call check(nf90_put_att(ncid, NF90_GLOBAL, "institution", "JRC"), &
            "add global attribute")
        call check(nf90_put_att(ncid, NF90_GLOBAL, "source", "ERA5 data"), & 
            "add global attribute")
        call check(nf90_put_att(ncid, NF90_GLOBAL, "history", datenow % isoformat()),&
            "add global attribute")
        call check(nf90_put_att(ncid, NF90_GLOBAL, "comment", "" ),&
            "add global attribute")
        call check(nf90_put_att(ncid, NF90_GLOBAL, "conventions", "SCHISM"),&
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
        ! TODO
        ! define units and base date automatically
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
        call check( nf90_def_var(ncid, "siconc", NF90_FLOAT, (/ ID_lon, ID_lat, ID_time /), &
            ID_siconc), "define variable siconc")
        call check( nf90_put_att(ncid, ID_siconc, "standard_name", "sea_ice_area_fraction" ), &
            "define attributes")
        call check( nf90_put_att(ncid, ID_siconc, "long_name", "Sea ice area fraction" ), &
            "define attributes")
        call check( nf90_put_att(ncid, ID_siconc, "units", "(0 - 1)" ), "define attributes")
        call check( nf90_put_att(ncid, ID_siconc, "_FillValue", aux_f ), "define attributes")
        call check( nf90_put_att(ncid, ID_siconc, "missing_value", aux_f), "define attributes")

        ! lon
        call check( nf90_def_var(ncid, "lon", NF90_FLOAT, (/ ID_lon, ID_lat /), ID_lonnf), "define variable lon")
        call check( nf90_put_att(ncid, ID_lonnf, "_FillValue", nan_f ), "define attributes")

        ! lat
        call check( nf90_def_var(ncid, "lat", NF90_FLOAT, (/ ID_lon, ID_lat /), ID_lattf), "define variable lat")
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

   
    subroutine get_dimensions_era5(ncname, ilon, ilat, itime)
        implicit none

        character(len = *), intent(in) :: ncname

        integer, intent(out) :: ilon, ilat, itime

        character(len = 25) :: name
        integer :: ncid, dimid

        ! Reading dimensions of latitudes longitudes
        call check(nf90_open(ncname, NF90_NOWRITE, ncid), &
            "open era5 netcdf file")
        call check(nf90_inq_dimid(ncid, "longitude", dimid), "id lon dimension")
        call check(nf90_inquire_dimension(ncid, dimid, name, ilon), &
            "getting lon dimension")
        call check(nf90_inq_dimid(ncid, "latitude", dimid), "id lat dimension")
        call check(nf90_inquire_dimension(ncid, dimid, name, ilat), &
            "getting lat dimension")
        call check(nf90_inq_dimid(ncid, "time", dimid), "id time dimension")
        call check(nf90_inquire_dimension(ncid, dimid, name, itime), &
            "getting time dimension")

        call check(nf90_close(ncid), "closing nc")
    end subroutine 

    
    subroutine get_lon_lat_era5_nc(ncname)
        implicit none

        character(len = *), intent(in) :: ncname

        integer :: ilon, ilat, itime
        integer :: ncid, varid

        call get_dimensions_era5(ncname, ilon, ilat, itime)

        !allocate longitude, latitude, u and v
        allocate(longitude(ilon))
        allocate(latitude(ilat))

        allocate(lonn(ilon, ilat))
        allocate(latt(ilon, ilat))

        call check(nf90_open(ncname, NF90_NOWRITE, ncid), &
            "open era5 netcdf file")

        ! Reading latitude, longitude
        call check(nf90_inq_varid(ncid, "longitude", varid), "id longitude var")
        call check(nf90_get_var(ncid, varid, longitude), "getting longitude")
        call check(nf90_inq_varid(ncid, "latitude", varid), "id latitude var")
        call check(nf90_get_var(ncid, varid, latitude), "getting latitude")

        longitude = longitude - 180.0
        latitude = -1.0 * latitude

        lonn = SPREAD(longitude, 2, ilat)
        latt = SPREAD(latitude, 1, ilon)

        call check(nf90_close(ncid), "closing nc")
    end subroutine


    subroutine get_time_era5(ncname)
        implicit none

        character(len = *), intent(in) :: ncname

        integer :: ilon, ilat, itime
        integer :: ncid, varid

        call get_dimensions_era5(ncname, ilon, ilat, itime)

        !allocate time
        allocate(time(itime))

        call check(nf90_open(ncname, NF90_NOWRITE, ncid), &
            "open era5 netcdf file")

        ! Reading time
        call check(nf90_inq_varid(ncid, "time", varid), "id time var")
        call check(nf90_get_var(ncid, varid, time), "getting time")
        call check(nf90_close(ncid), "closing nc")

    end subroutine

!    subroutine get_time_era5_nc(ncname, time)
!        implicit none
!    end subroutine

    subroutine get_mask_era5_nc(ncname)
        implicit none

        character(len = *), intent(in) :: ncname

        integer :: ilon, ilat, itime
        integer :: ncid, varid

        call get_dimensions_era5(ncname, ilon, ilat, itime)

        !allocate longitude, latitude, u and v
        allocate(auxx(ilon, ilat, itime))
        allocate(mask(ilon, ilat))

        call check(nf90_open(ncname, NF90_NOWRITE, ncid), &
            "open era5 netcdf file")

        ! Reading mask
        call check(nf90_inq_varid(ncid, "u10", varid), "varid u10")
        call check(nf90_get_var(ncid, varid, auxx), "getting u10")
        call check(nf90_get_att(ncid, varid, "_FillValue", fillvalue),&
            "Fill Value")
        !mask = auxx(:, :, 1) * merge(1, 0, auxx(:, :, 1) == fillvalue)
        call check(nf90_close(ncid), "closing nc")

        mask = 0.0
        where (auxx(:,:, 1) ==  fillvalue) mask = 1.0
        deallocate(auxx)
    end subroutine


    subroutine get_u10_v10_era5_nc(ncname)
        implicit none

        character(len = *), intent(in) :: ncname

        integer :: ilon, ilat, itime
        integer :: ncid, varid

        call get_dimensions_era5(ncname, ilon, ilat, itime)

        !allocate longitude, latitude, u and v
        allocate(u10(ilon, ilat, itime))
        allocate(v10(ilon, ilat, itime))

        call check(nf90_open(ncname, NF90_NOWRITE, ncid), &
            "open era5 netcdf file")

        ! Reading u10, v10
        call check(nf90_inq_varid(ncid, "u10", varid), "varid u10")
        call check(nf90_get_var(ncid, varid, u10), "getting u10")
        call check(nf90_get_att(ncid, varid, "scale_factor", scale_factor),&
            "scale factor")
        call check(nf90_get_att(ncid, varid, "add_offset", add_offset),&
            "add_offset")
        call check(nf90_get_att(ncid, varid, "_FillValue", fillvalue),&
            "Fill Value")
        where (u10 .ne. fillvalue)  u10 = (u10 * scale_factor) + add_offset
        u10 = u10(:, ilat:1:-1, :)
        ! shift values (because lon was shifted)
        u10 = CSHIFT(u10, int(ilon/2.0), 1)


        call check(nf90_inq_varid(ncid, "v10", varid), "varid v10")
        call check(nf90_get_var(ncid, varid, v10), "gettign v10")
        call check(nf90_get_att(ncid, varid, "scale_factor", scale_factor),&
            "scale factor")
        call check(nf90_get_att(ncid, varid, "add_offset", add_offset),&
            "add_offset")
        call check(nf90_get_att(ncid, varid, "_FillValue", fillvalue),&
            "Fill Value")
        call check(nf90_close(ncid), "closing nc")

        where (v10 .ne. fillvalue)  v10 = (v10 * scale_factor) + add_offset
        v10 = v10(:, ilat:1:-1, :)
        ! shift values (because lon was shifted)
        v10 = CSHIFT(v10, int(ilon/2.0), 1)

    end subroutine
    

    subroutine get_msl_era5_nc(ncname)
        implicit none

        character(len = *), intent(in) :: ncname

        integer :: ilon, ilat, itime
        integer :: ncid, varid

        call get_dimensions_era5(ncname, ilon, ilat, itime)

        !allocate longitude, latitude, u and v
        allocate(msl(ilon, ilat, itime))

        call check(nf90_open(ncname, NF90_NOWRITE, ncid), &
            "open era5 netcdf file")

        ! Reading msl
        call check(nf90_inq_varid(ncid, "msl", varid), "varid msl")
        call check(nf90_get_var(ncid, varid, msl), "getting msl")
        call check(nf90_get_att(ncid, varid, "scale_factor", scale_factor),&
            "scale factor")
        call check(nf90_get_att(ncid, varid, "add_offset", add_offset),&
            "add_offset")
        call check(nf90_get_att(ncid, varid, "_FillValue", fillvalue),&
            "Fill Value")
        call check(nf90_close(ncid), "closing nc")

        where (msl .ne. fillvalue)  msl = (msl * scale_factor) + add_offset
        msl = msl(:, ilat:1:-1, :)
        ! shift values (because lon was shifted)
        msl = CSHIFT(msl, int(ilon/2.0), 1)

    end subroutine


    subroutine get_siconc_era5_nc(ncname)
        implicit none

        character(len = *), intent(in) :: ncname

        integer :: ilon, ilat, itime
        integer :: ncid, varid

        call get_dimensions_era5(ncname, ilon, ilat, itime)

        !allocate longitude, latitude, u and v
        allocate(siconc(ilon, ilat, itime))

        call check(nf90_open(ncname, NF90_NOWRITE, ncid), &
            "open era5 netcdf file")

        ! Reading siconc
        call check(nf90_inq_varid(ncid, "siconc", varid), "varid siconc")
        call check(nf90_get_var(ncid, varid, siconc), "getting siconc")
        call check(nf90_get_att(ncid, varid, "scale_factor", scale_factor),&
            "scale factor")
        call check(nf90_get_att(ncid, varid, "add_offset", add_offset),&
            "add_offset")
        call check(nf90_get_att(ncid, varid, "_FillValue", fillvalue),&
            "Fill Value")
        call check(nf90_close(ncid), "closing nc")

        where (siconc .ne. fillvalue)  siconc = (siconc * scale_factor) + add_offset
        siconc = siconc(:, ilat:1:-1, :) 
        ! shift values (because lon was shifted)
        siconc = CSHIFT(siconc, int(ilon/2.0), 1)

!        siconc = siconc * merge(0, 1, siconc == fillvalue)
!        do i = 1, size(siconc, 3)
!            siconc(:, :, i) = siconc(:, :, i) * mask + aux_f
!        end do

    end subroutine


    subroutine merge_data_sflux_nc(sflux_name, number_of_nc)
        implicit none

        character (len = *), intent(in) :: sflux_name
        integer, intent(in) :: number_of_nc

        character(len = 100) :: era5_nc

        integer :: interror, i, j, k, ncid
        integer :: ilon, ilat, itime, timescale

        real(kind = dp) :: ttime, deltatime
        real(kind = dp):: diff_julian_time, timescale_f


        open(UNIT=11, FILE="out.dat", ACTION="READ", IOSTAT=interror, STATUS="UnkNOWN")

        print *, "===== Merging ", number_of_nc, " files ======"

        ! initialize time and set deltatime
        ttime = 0.0
        deltatime = 1.0/24.0

        ! initialize counter
        k = 1


        do i = 1, number_of_nc !LOOP TO READ OVER EACH NC
            READ(11,*) era5_nc
            era5_nc = "data/"//trim(era5_nc)
            print *, "merging ... ", era5_nc

            if (i == 1) then
                call get_lon_lat_era5_nc(era5_nc)
                call check(nf90_open(sflux_name, NF90_WRITE, ncid), "open sflux file")

                call check(nf90_put_var(ncid, ID_lonf, longitude), "writing longitude") 
                call check(nf90_put_var(ncid, ID_latf, latitude), "writing latitude") 
                call check(nf90_put_var(ncid, ID_lonnf, lonn), "writing longitude") 
                call check(nf90_put_var(ncid, ID_lattf, latt), "writing latitude") 
                call check(nf90_put_var(ncid, ID_stmp, real(lonn*0.0,8) ), "writing stmp") 
                call check(nf90_put_var(ncid, ID_spfh, real(lonn*0.0,8) ), "writing spfh") 

                call check(nf90_close(ncid), "closing nc") 
            !    call get_mask_era5_nc(era5_nc)
                ! shift time to seconds since 2010-01-01 00:00:00
                call shift_time(era5_nc, "time", diff_julian_time, timescale)
            end if

            call check(nf90_open(sflux_name, NF90_WRITE, ncid), "open sflux file")
            call get_dimensions_era5(era5_nc, ilon, ilat, itime)
            call get_u10_v10_era5_nc(era5_nc)
            call get_msl_era5_nc(era5_nc)
            call get_siconc_era5_nc(era5_nc)
            call get_time_era5(era5_nc)

            do j = 1, itime
                call check(nf90_put_var(ncid, ID_time, &
                    (time(j) * timescale + diff_julian_time)/24.0/3600.0 , &
                    start = [k]), "writing time") 
                call check(nf90_put_var(ncid, ID_u10, real(u10(:,:,j), 4), &
                    start = [1, 1, k]), "writing u10") 
                call check(nf90_put_var(ncid, ID_v10, real(v10(:,:,j), 4), &
                    start = [1, 1, k]), "writing v10") 
                call check(nf90_put_var(ncid, ID_msl, real(msl(:,:,j), 4), &
                    start = [1, 1, k]), "writing msl") 
                call check(nf90_put_var(ncid, ID_siconc, real(siconc(:,:,j), 4), &
                    start = [1, 1, k]), "writing siconc") 
                ttime = ttime + deltatime
                k = k + 1
            end do
            call check(nf90_close(ncid), "closing nc") 

            deallocate(u10)
            deallocate(v10)
            deallocate(msl)
            deallocate(siconc)
            deallocate(time)

        end do

        close(11)

!        deallocate(mask)


    end subroutine


end module
