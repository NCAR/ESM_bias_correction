module initialization
    use atmosphere_dataset, only: atm_t
    use output_dataset,     only: output_t
    use io_routines,        only: io_read, io_newunit
    use string,             only: str
    use constants

    implicit none

    type parameters_t
        CHARACTER(len=kMAX_FILE_LENGTH), allocatable, dimension(:) :: filenames
        CHARACTER(len=kMAX_FILE_LENGTH), allocatable, dimension(:) :: varnames
        CHARACTER(len=kMAX_VARNAME_LENGTH) :: z_name
        CHARACTER(len=kMAX_VARNAME_LENGTH) :: lat_name, lon_name, time_name
        CHARACTER(len=kMAX_FILE_LENGTH) :: ref_start, ref_end
        CHARACTER(len=kMAX_FILE_LENGTH) :: cor_start, cor_end
        CHARACTER(len=kMAX_FILE_LENGTH) :: outputfile
        integer :: n_segments
    end type parameters_t


contains
    subroutine init(reference, esm, output)
        implicit none
        type(atm_t), intent(inout) :: reference, esm
        type(output_t), intent(inout) :: output

        character(len=kMAX_FILE_LENGTH) :: options_file

        real, allocatable, dimension(:) :: lat, lon
        real, allocatable, dimension(:,:,:) :: z
        integer :: nx, ny, nz
        integer, allocatable, dimension(:) :: dim_sizes
        type(parameters_t) :: ref_options, esm_options

        options_file = get_options_file()
        ref_options = read_config(options_file,"r")
        esm_options = read_config(options_file,"e")

        call reference%init(filenames   = ref_options%filenames,    & !ref_files,            &
                            varnames    = ref_options%varnames,     & !["qv"],                &
                            z_name      = ref_options%z_name,       & !"z",                     &
                            lat_name    = ref_options%lat_name,     & !"lat",                 &
                            lon_name    = ref_options%lon_name,     & !"lon",                 &
                            time_name   = ref_options%time_name,    & !"time",               &
                            ref_start   = ref_options%ref_start,    & !"1980-01-01 00:00:00",&
                            ref_end     = ref_options%ref_end,      & !"1985-12-31 00:00:00",  &
                            cor_start   = ref_options%cor_start,    & !"1980-01-01 00:00:00",&
                            cor_end     = ref_options%cor_end,      & !"1980-12-31 00:00:00",  &
                            n_segments  = ref_options%n_segments) !100)

        ! initialize the ESM dataset
        call esm%init(      filenames   = esm_options%filenames,    & !ref_files,            &
                            varnames    = esm_options%varnames,     & !["qv"],                &
                            z_name      = esm_options%z_name,       & !"z",                     &
                            lat_name    = esm_options%lat_name,     & !"lat",                 &
                            lon_name    = esm_options%lon_name,     & !"lon",                 &
                            time_name   = esm_options%time_name,    & !"time",               &
                            ref_start   = esm_options%ref_start,    & !"1980-01-01 00:00:00",&
                            ref_end     = esm_options%ref_end,      & !"1985-12-31 00:00:00",  &
                            cor_start   = esm_options%cor_start,    & !"1980-01-01 00:00:00",&
                            cor_end     = esm_options%cor_end,      & !"1980-12-31 00:00:00",  &
                            n_segments  = esm_options%n_segments)  !100)

        call io_read(ref_options%filenames(1), ref_options%lat_name, lat)
        call io_read(ref_options%filenames(1), ref_options%lon_name, lon)
        call io_read(ref_options%filenames(1), ref_options%z_name, z)

        nx = size(lon)
        ny = size(lat)
        nz = size(z,3)

        dim_sizes = [ nx, ny, nz, 0]

        call output%init(ref_options%outputfile,            &
                        esm_options%varnames,               &
                        dim_sizes,                          &
                        [" lon", " lat", " lev","time"],    &
                        esm_options%cor_start, z, lat, lon)


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


    function read_config(options_file, nml_type) result(parameters)
        implicit none
        character(len=*), intent(in) :: options_file
        character(len=*), intent(in) :: nml_type
        type(parameters_t) :: parameters

        character(len=kMAX_FILE_LENGTH), dimension(:), allocatable :: all_files
        character(len=kMAX_FILE_LENGTH), dimension(:), allocatable :: forcing_files
        integer :: nfiles

        character(len=kMAX_FILE_LENGTH) :: varnames(kMAX_NUMBER_FILES), z_name, lat_name, lon_name, time_name, &
                                           ref_start, ref_end, cor_start, cor_end, &
                                           outputfile, filelist
        integer :: n_segments, nvars, i, name_unit

        namelist /ESM_parameters/ filelist, varnames, z_name, lat_name, lon_name, time_name, &
                                        ref_start, ref_end, cor_start, cor_end, n_segments, outputfile

        namelist /reference_parameters/ filelist, varnames, z_name, lat_name, lon_name, time_name, &
                                        ref_start, ref_end, cor_start, cor_end, n_segments, outputfile


        !defaults:
        varnames = "---------"
        varnames(1) = "qv"
        filelist="file_list.txt"
        z_name="z"
        lat_name="lat"
        lon_name="lon"
        time_name="time"
        ref_start="1980-01-01 00:00:00"
        ref_end="1985-12-31 00:00:00"
        cor_start="1980-01-01 00:00:00"
        cor_end="1980-12-31 00:00:00"
        n_segments=100
        outputfile="output.nc"


        ! read namelists
        if (nml_type == "e") then
            open(io_newunit(name_unit), file=options_file)
            read(name_unit,nml=ESM_parameters)
            close(name_unit)
        elseif (nml_type == "r") then
            open(io_newunit(name_unit), file=options_file)
            read(name_unit,nml=reference_parameters)
            close(name_unit)
        endif

        ! setup an array containing just the file names read from filelist
        allocate(all_files(kMAX_NUMBER_FILES))
        nfiles = read_forcing_file_names(filelist, all_files)
        allocate(forcing_files(nfiles))
        forcing_files = all_files(1:nfiles)

        ! setup an array for the variable names
        nvars = 0
        do i=1, kMAX_NUMBER_FILES
            if (varnames(i) /= "---------") then
                nvars = nvars + 1
            endif
        enddo
        print*, "found n vars: ", nvars
        allocate(parameters%varnames(nvars))

        parameters%varnames = varnames(1:nvars)
        parameters%filenames = forcing_files
        parameters%z_name = z_name
        parameters%lat_name = lat_name
        parameters%lon_name = lon_name
        parameters%time_name = time_name
        parameters%ref_start = ref_start
        parameters%ref_end = ref_end
        parameters%cor_start = cor_start
        parameters%cor_end = cor_end
        parameters%n_segments = n_segments
        parameters%outputfile = outputfile

    end function read_config

    !> ----------------------------------------------------------------------------
    !!  Read in the name of the options files from the command line
    !!
    !!  @retval     options_file    The name of the options parameter file to use.
    !!                              Default = icar_options.nml
    !!
    !! ----------------------------------------------------------------------------
    function get_options_file() result(options_file)
        implicit none
        character(len=kMAX_FILE_LENGTH) :: options_file

        ! Internal variables
        integer :: error
        logical :: file_exists
        ! default options filename
        character(len=*), parameter :: default_options_file = "esm_correction_options.nml"

        ! if a commandline argument was supplied, read the options filename from there
        if (command_argument_count()>0) then
            ! read the commandline argument
            call get_command_argument(1, options_file, status=error)
            ! if there was any problem revert to the default filename
            if (error > 0) then
                options_file = default_options_file

            ! error -1 means the filename supplied was too long
            elseif (error == -1) then
                write(*,*) "Options filename = ", trim(options_file), " ...<cutoff>"
                write(*,*) "Maximum filename length = ", kMAX_FILE_LENGTH
                stop "ERROR: options filename too long"
            endif

        ! If not arguments were supplied use the default filename
        else
            options_file = default_options_file
        endif

        ! Check that the options file actually exists
        INQUIRE(file=trim(options_file), exist=file_exists)

        ! if options file does not exist, print an error and quit
        if (.not.file_exists) then
            write(*,*) "Using options file = ", trim(options_file)
            stop "Options file does not exist. "
        endif
    end function


end module initialization
