module initialization
    use atmosphere_dataset, only: atm_t
    use output_dataset,     only: output_t
    use io_routines,        only: io_read, io_newunit
    use string,             only: str
    use constants

    implicit none

contains
    subroutine init(reference, esm, output)
        implicit none
        type(atm_t), intent(inout) :: reference, esm
        type(output_t), intent(inout) :: output

        character(len=kMAX_FILE_LENGTH), dimension(:), allocatable :: all_files
        character(len=kMAX_FILE_LENGTH), dimension(:), allocatable :: ref_files, esm_files
        integer :: nfiles
        real, allocatable, dimension(:) :: lat, lon
        real, allocatable, dimension(:,:,:) :: z
        integer :: nx, ny, nz
        integer, allocatable, dimension(:) :: dim_sizes

        allocate(all_files(kMAX_NUMBER_FILES))
        nfiles = read_forcing_file_names("ref_file_list.txt", all_files)
        allocate(ref_files(nfiles))
        ref_files = all_files(1:nfiles)

        call reference%init(filenames=ref_files,            &
                            varnames=["qv"], & ! character(len=kMAX_VARNAME_LENGTH) ::&
                                                            !"qv","theta","u","v","p"],        &
                            z_name="z",                     &
                            lat_name="lat",                 &
                            lon_name="lon",                 &
                            time_name="time",               &
                            ref_start="1980-01-01 00:00:00",&
                            ref_end="1985-12-31 00:00:00",  &
                            cor_start="1980-01-01 00:00:00",&
                            cor_end="1980-12-31 00:00:00")

        ! initialize the ESM dataset
        nfiles = read_forcing_file_names("esm_file_list.txt", all_files)
        allocate(esm_files(nfiles))
        esm_files = all_files(1:nfiles)


        call esm%init(      filenames=esm_files,           &
                            varnames=["qv"], &!character(len=kMAX_VARNAME_LENGTH) ::&
                                      !"qv","theta","u","v","p"],        &
                            ! varnames=[character(len=kMAX_VARNAME_LENGTH) ::&
                            !           "qv","theta","u","v","p"],        &
                            z_name="z",                     &
                            lat_name="lat",                 &
                            lon_name="lon",                 &
                            time_name="time",               &
                            ref_start="2000-01-01 00:00:00",&
                            ref_end="2010-12-31 00:00:00",  &
                            cor_start="2000-01-01 00:00:00",&
                            cor_end="2019-12-31 00:00:00")

        call io_read(ref_files(1), "lat", lat)
        call io_read(ref_files(1), "lon", lon)
        call io_read(ref_files(1), "z", z)
        nx = size(lon)
        ny = size(lat)
        nz = size(z,3)

        dim_sizes = [ nx, ny, nz, -1]

        call output%init("output.nc",                           &
                        [character(len=kMAX_VARNAME_LENGTH) ::  &
                                   "qv"],&!,"theta","u","v","p"],   &
                        dim_sizes,                              &
                        ! ["time", " lev", " lat"," lon"],        &
                        [" lon", " lat", " lev","time"],        &
                        z, lat, lon)


    end subroutine init


    !> ----------------------------------------------------------------------------
    !!  Read in the name of the boundary condition files from a text file
    !!
    !!  @param      filename        The name of the text file to read
    !!  @param[out] forcing_files   An array to store the filenames in
    !!  @retval     nfiles          The number of files read.
    !!
    !! ----------------------------------------------------------------------------
    function read_forcing_file_names(filename, forcing_files) result(nfiles)
        implicit none
        character(len=*), intent(in) :: filename
        character(len=*), intent(inout), dimension(:) :: forcing_files
        integer :: nfiles
        integer :: file_unit
        integer :: i, error
        character(len=kMAX_FILE_LENGTH) :: temporary_file

        open(unit=io_newunit(file_unit), file=filename)
        i=0
        error=0
        do while (error==0)
            read(file_unit, *, iostat=error) temporary_file
            if (error==0) then
                i=i+1
                forcing_files(i) = temporary_file
            endif
        enddo
        close(file_unit)
        nfiles = i
        ! print out a summary
        write(*,*) "  Fileset to be used:"
        if (nfiles>10) then
            write(*,*) "    nfiles=", trim(str(nfiles)), ", too many to print."
            write(*,*) "    First file:", trim(forcing_files(1))
            write(*,*) "    Last file: ", trim(forcing_files(nfiles))
        else
            do i=1,nfiles
                write(*,*) "      ",trim(forcing_files(i))
            enddo
        endif

    end function read_forcing_file_names

end module initialization
