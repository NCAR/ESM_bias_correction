module atmosphere_dataset

    use io_routines, only: io_read, io_write, io_getdims
    use time_periods, only: time_period_data_t
    use time_obj, only: time_t
    use qm_obj, only: qm_transform
    use vertical_interp, only: vinterp_t
    use geographic, only: geo_transform
    implicit none

    type atm_t

        integer :: n_variables
        integer, allocatable :: n_levels(:)

        ! store the reference period mean z coordinate in double precision so that the
        ! accumulation and averaging stage won't have significant precision errors
        real(kind=dp), dimension(:,:,:), allocatable :: z_mean
        ! temporary data before developing the QM? (nlon, nlat, nlevels ntimes)
        real, dimension(:,:,:,:), allocatable :: data
        ! store the latitude and longitude coordinates
        real, dimension(:,:), allocatable :: lat, lon

        type(time_period_data_t) :: reference
        type(time_period_data_t) :: correction
        type(qm_transform) :: qm
        type(geo_transform), target :: geo
        type(vinterp_t) :: vLUT

        character(len=kMAX_FILE_LENGTH), DIMENSION(:), ALLOCATABLE :: filenames
        character(len=kMAX_VARNAME_LENGTH), DIMENSION(:), ALLOCATABLE :: varnames

        character(len=kMAX_VARNAME_LENGTH) :: z_name, lat_name, lon_name, time_name

    contains

        init
        generate_means
        generate_mean_calculations_for
        generate_bc_with
        apply_bc

    end type atm_t

contains
    subroutine init(this, filenames, varnames, z_name, lat_name, lon_name, time_name, &
                    ref_start, ref_end, cor_start, cor_end)
        implicit none
        integer :: i

        ! store primary dataset information in object variables
        this%filenames = filenames
        this%varnames = varnames

        this%z_name = z_name
        this%lat_name = lat_name
        this%lon_name = lon_name
        this%time_name = time_name

        ! initialize the lat/lon data
        call this%initialize_geo_data()

        ! initialize the reference and correction time periods
        ! for the reference dataset, the correction period doesnt matter
        call this%reference%init(filenames, time_name)
        call this%reference%find_period(ref_start, ref_end)

        call this%correction%init(filenames, time_name)
        call this%correction%find_period(cor_start, cor_end)

        ! initialize the vertical interpolation by
        ! giving it the information needed to read the vertical coordinate
        call vLUT%init(filenames, z_name)

        ! read the dimensions of all variables from the first file
        ! if the data are 4-d variable (x, y, z, t) store the third dimension size as nlevels
        ! this is primarily to handle 2D vs 3D variables
        call allocate(this%nlevels(len(varnames)))
        do i=1, len(varnames)
            dims = io_getdims(filenames(1), varnames(i))
            if (dims(1) > 3) then
                this%nlevels(i) = dims(4)
            else
                this%nlevels(i) = 1
            endif
        enddo

    end subroutine init


    subroutine initialize_geo_data()
        implicit none


        call io_read(this%filenames(1), this%lat_name, this%lat)
        call io_read(this%filenames(1), this%lon_name, this%lon)

        if (size(this%lat, 2) == 1) then
            ! lat and lon data were provided in one-d
            stop "Not yet able to handle 1D lat/lon data"
        endif

        this%nlon = size(this%lat,1)
        this%nlat = size(this%lat,2)

        call this%geo%init(this%lat, this%lon)

    end subroutine initialize_geo_data


    subroutine load_reference_period(this, variable_index)
        implicit none
        if (allocated(this%data)) deallocate(this%data)
        allocate(this%data(this%nlon, this%nlat, this%nlevels(variable_index), this%reference%n_timesteps))
        this%data = 0

        allocate(z(this%nlon, this%nlat, maxval(this%nlevels(variable_index))))

        allocate(temp_data(this%nlon, this%nlat, this%nlevels(variable_index)))


        call this%reference%reset_counter()
        do i=1, this%reference%n_timesteps
            z = this%reference%next( this%z_name))
            this%data(i,:,:,:) = this%vLUT%interp(z, this%reference%current( this%variables( variable_index)))
        enddo

    end subroutine load_reference_period


    subroutine generate_means(this)
        implicit none
        integer :: i

        if (allocated(this%z_data)) deallocate(this%z_data)
        allocate(this%z_data(this%nlon, this%nlat, this%nlevels))
        this%z_data = 0

        call this%reference%reset_counter()
        do i=1, this%reference%n_timesteps
            this%z_data = this%z_data + this%reference%next(this%z_name)
        enddo

        this%z_data = this%z_data / this%reference%n_timesteps

    end subroutine generate_means


    subroutine generate_mean_calculations_for(this, ref_dataset)
        implicit none

        ! generate the geographic lookup table to transform this dataset to the reference dataset
        call this%geo%geo_LUT(ref_dataset%geo)

        if (.not.allocated(ref_dataset%z_data)) call ref_dataset%generate_means()

        call this%vLUT%set_z(ref_dataset%z_data)
        ! call this%generate_means()

    end subroutine generate_mean_calculations_for


    subroutine generate_bc_with(ref_dataset, variable_index)
        implicit none

        this%reference%set_geo_transform(this%geo)

        this%load_reference_period(variable_index)
        ref_dataset%load_reference_period(variable_index)

        ! this%qm%develop(this%data, ref_dataset%data)

    end subroutine generate_bc_with


    subroutine apply_bc(varname)
        implicit none

        this%correction%set_geo_transform(this%geo)

        call this%correction%reset_counter()
        do i=1, this%correction%n_timesteps
            temp_data = this%correction%next(varname)
            ! call qm%apply(temp_data)
            ! this%output%write(temp_data)
        enddo

    end subroutine apply_bc


end module atmosphere_dataset
