program PREPARE_SFLUX
    use mod_const
    use mod_precision
    use mod_datetime
    use mod_nc_sflux
    use netcdf
    implicit none

    character(len=100) :: sflux_name, sample_nc

    integer :: HereError
    integer :: ilon, ilat, itime, number_of_nc


    namelist /sflux/ sflux_name
     ! time_window, spill, oil_centre, radius, mu, dt, tstart, tend

    OPEN(UNIT=100, FILE="namelist.sflux", ACTION="READ", IOSTAT = HereError,  STATUS = "UnkNOWN")
    READ(100, NML = sflux)
    CLOSE(100)

    call system("ls data/ > out.dat")
    call system("wc out.dat >out2.dat")
    OPEN(UNIT=11, FILE="out2.dat", ACTION="READ", STATUS="UnkNOWN")
    READ(11,*) number_of_nc
    CLOSE(11)
    OPEN(UNIT=11, FILE="out.dat", ACTION="READ", STATUS="UnkNOWN")
    READ(11,*) sample_nc
    sample_nc = "data/"//trim(sample_nc)
    print *, "Extracting dimensions from sample data file: ", sample_nc
    CLOSE(11)

    ! Get dimensions from sample data
    call get_dimensions_era5(sample_nc, ilon, ilat, itime)

    ! Initializing netcdf structure
    call create_sflux_ncfile(sflux_name, ilon, ilat) 

    ! Merge all era5 netcdfs in data/ directory
    call merge_data_sflux_nc(sflux_name, number_of_nc)


    !deallocate matrices
    if (allocated(u10)) deallocate(u10)
    if (allocated(v10)) deallocate(v10)
    if (allocated(msl)) deallocate(msl)
    if (allocated(siconc)) deallocate(siconc)


    print *, "============ DONE ================"
end program PREPARE_SFLUX
