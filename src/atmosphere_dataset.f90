module atmosphere_dataset

    use constants, only: dp, kMAX_VARNAME_LENGTH, kMAX_FILE_LENGTH
    use io_routines, only: io_read, io_write, io_getdims, io_maxDims
    use output_dataset, only: output_t
    use time_periods, only: time_period_data_t
    ! use time_obj, only: time_t
    use bias_correction_obj, only: bc_transform
    use vertical_interp, only: vinterp_t
    use geographic, only: geo_transform
    implicit none

    type atm_t

        integer :: n_variables
        integer, allocatable :: nlevels(:), nlon, nlat

        ! store the reference period mean z coordinate in double precision so that the
        ! accumulation and averaging stage won't have significant precision errors
        real(kind=dp), dimension(:,:,:), pointer :: z_data
        ! temporary data before developing the QM (nlon, nlat, nlevels, ntimes)
        real, dimension(:,:,:,:), allocatable :: data
        ! store the latitude and longitude coordinates
        real, dimension(:,:), allocatable :: lat, lon

        type(time_period_data_t) :: reference
        type(time_period_data_t) :: correction
        type(bc_transform) :: bc
        type(geo_transform), pointer :: geo
        type(vinterp_t) :: vLUT

        character(len=kMAX_FILE_LENGTH), DIMENSION(:), ALLOCATABLE :: filenames
        character(len=kMAX_VARNAME_LENGTH), DIMENSION(:), ALLOCATABLE :: varnames

        character(len=kMAX_VARNAME_LENGTH) :: z_name, lat_name, lon_name, time_name

    contains
        procedure, public  :: init => init
        procedure, public  :: initialize_geo_data => initialize_geo_data
        procedure, public  :: generate_means => generate_means
        procedure, public  :: generate_mean_calculations_for => generate_mean_calculations_for
        procedure, public  :: generate_bc_with => generate_bc_with
        procedure, public  :: load_reference_period => load_reference_period
        procedure, public  :: apply_bc => apply_bc
    end type atm_t

contains
    subroutine init(this, filenames, varnames, z_name, lat_name, lon_name, time_name, &
                    ref_start, ref_end, cor_start, cor_end)
        implicit none
        class(atm_t), intent(inout) :: this
        CHARACTER(len=*), intent(in) :: filenames(:)
        CHARACTER(len=*), intent(in) :: varnames(:)
        CHARACTER(len=*), intent(in) :: z_name
        CHARACTER(len=*), intent(in) :: lat_name, lon_name, time_name
        CHARACTER(len=*), intent(in) :: ref_start, ref_end
        CHARACTER(len=*), intent(in) :: cor_start, cor_end

        integer :: i
        integer :: dims(io_maxDims)

        ! store primary dataset information in object variables
        this%filenames = filenames
        this%varnames = varnames
        this%n_variables = size(varnames)

        this%z_name = z_name
        this%lat_name = lat_name
        this%lon_name = lon_name
        this%time_name = time_name

        this%geo => NULL()
        this%z_data => NULL()

        ! initialize the lat/lon data
        call this%initialize_geo_data()

        ! initialize the reference and correction time periods
        ! for the reference dataset, the correction period doesnt matter
        call this%reference%init(filenames, time_name)
        call this%reference%find_period(ref_start, ref_end)

        call this%correction%init(filenames, time_name)
        call this%correction%find_period(cor_start, cor_end)

        ! read the dimensions of all variables from the first file
        ! if the data are 4-d variable (x, y, z, t) store the third dimension size as nlevels
        ! this is primarily to handle 2D vs 3D variables
        allocate(this%nlevels(size(varnames)))

        do i=1, size(varnames)
            call io_getdims(trim(filenames(1)), trim(varnames(i)), dims)
            print*, trim(varnames(i)), " has n-dimensions: ", dims(1)
            if (dims(1) > 3) then
                this%nlevels(i) = dims(4)
            else
                this%nlevels(i) = 1
            endif
        enddo
        print*, "Number of levels in each variable: ", this%nlevels

    end subroutine init


    subroutine initialize_geo_data(this)
        implicit none
        class(atm_t), intent(inout) :: this

        real, DIMENSION(:,:), allocatable :: temp
        integer :: nx, ny, i

        call io_read(trim(this%filenames(1)), trim(this%lat_name), this%lat)
        call io_read(trim(this%filenames(1)), trim(this%lon_name), this%lon)

        if (size(this%lat, 2) == 1) then
            ! lat and lon data were provided in one-d
            nx = size(this%lon,1)
            ny = size(this%lat,1)
            allocate(temp(nx, ny))
            do i=1,nx
                temp(i,:) = this%lat(:,1)
            enddo
            deallocate(this%lat)
            allocate(this%lat, source=temp)

            do i=1,ny
                temp(:,i) = this%lon(:,1)
            enddo
            deallocate(this%lon)
            allocate(this%lon, source=temp)
            deallocate(temp)

            ! stop "Not yet able to handle 1D lat/lon data"
        endif

        this%nlon = size(this%lat,1)
        this%nlat = size(this%lat,2)

        allocate(this%geo)
        call this%geo%init(this%lat, this%lon)

    end subroutine initialize_geo_data


    subroutine load_reference_period(this, nz, variable_index)
        implicit none
        class(atm_t), intent(inout) :: this
        integer, INTENT(IN) :: nz
        integer, intent(in) :: variable_index

        real, DIMENSION(:,:,:), ALLOCATABLE :: temp_data, vinterped_data, z
        integer :: i, nx, ny

        if (allocated(this%data)) deallocate(this%data)
        ! allocate(this%data(this%nlon, this%nlat, nz, this%reference%n_timesteps))
        ! this%data = 0

        allocate(z(this%nlon, this%nlat, size(this%z_data, 3)))
        allocate(temp_data(this%nlon, this%nlat, this%nlevels(variable_index)))
        allocate(vinterped_data(this%nlon, this%nlat, nz))

        print*, "loading_reference"

        call this%reference%reset_counter()
        do i=1, this%reference%n_timesteps

            z = this%reference%next( this%z_name)

            temp_data = this%reference%current( this%varnames( variable_index))

            call this%vLUT%vinterp(z, temp_data, vinterped_data)

            if (.not.allocated(this%data)) then
                nx = size(vinterped_data,1)
                ny = size(vinterped_data,2)
                allocate(this%data(nx, ny, nz, this%reference%n_timesteps))
                this%data = 0
            endif
            this%data(:,:,:,i) = vinterped_data
        enddo

    end subroutine load_reference_period


    !>----------------------
    !> compute the mean vertical location that all other data will be interpolated to.
    subroutine generate_means(this)
        implicit none
        class(atm_t), intent(inout) :: this

        integer :: i

        ! if (associated(this%z_data)) then
        !     if (allocated(this%z_data)) deallocate(this%z_data)
        ! endif

        allocate(this%z_data(this%nlon, this%nlat, maxval(this%nlevels)))
        this%z_data = 0

        call this%reference%reset_counter()
        do i=1, this%reference%n_timesteps
            this%z_data = this%z_data + this%reference%next(this%z_name)
        enddo

        this%z_data = this%z_data / this%reference%n_timesteps

    end subroutine generate_means


    subroutine generate_mean_calculations_for(this, ref_dataset)
        implicit none
        class(atm_t), intent(inout) :: this
        type(atm_t), intent(inout) :: ref_dataset


        ! generate the geographic lookup table to transform this dataset to the reference dataset
        call this%geo%setup_geo_LUT(ref_dataset%geo)
        print*, "in: generate_mean_calculations_for"
        if (.not.associated(ref_dataset%z_data)) call ref_dataset%generate_means()

        ! initialize the vertical interpolation by
        ! giving it the information needed to read the vertical coordinate
        call this%vLUT%set_z(ref_dataset%z_data)
        call ref_dataset%vLUT%set_z(ref_dataset%z_data)

        print*, "exit: generate_mean_calculations_for"

    end subroutine generate_mean_calculations_for


    subroutine generate_bc_with(this, ref_dataset, variable_index)
        implicit none
        class(atm_t), intent(inout) :: this
        type(atm_t), intent(inout) :: ref_dataset
        integer, intent(in) :: variable_index

        integer :: nz

        nz = ref_dataset%nlevels(variable_index)

        print*, "in: generate_bc_with", variable_index

        call this%reference%set_geo_transform(this%geo)

        print*, "this%load_reference_period"
        call this%load_reference_period(nz, variable_index)
        print*, "ref_dataset%load_reference_period"
        call ref_dataset%load_reference_period(nz, variable_index)

        ! call this%bc%develop(this%data, ref_dataset%data)

    end subroutine generate_bc_with


    subroutine apply_bc(this, var_index, output)
        implicit none
        class(atm_t), intent(inout) :: this
        integer, intent(in) :: var_index
        type(output_t), intent(inout) :: output

        character(len=kMAX_VARNAME_LENGTH) :: varname
        integer :: i, nx, ny, nz

        real, DIMENSION(:,:,:), ALLOCATABLE :: z, temp_data, vinterped_data

        if (allocated(this%data)) deallocate(this%data)

        varname = this%varnames(var_index)
        print*, trim(varname)

        call this%correction%set_geo_transform(this%geo)

        call this%correction%reset_counter()
        do i=1, this%correction%n_timesteps
            z = this%correction%next( this%z_name)
            temp_data = this%correction%current(varname)

            call this%vLUT%vinterp(z, temp_data, vinterped_data)

            if (.not.allocated(this%data)) then
                nx = size(vinterped_data,1)
                ny = size(vinterped_data,2)
                nz = size(vinterped_data,3)
                allocate(this%data(nx, ny, nz, this%reference%n_timesteps))
                this%data = 0
            endif
            this%data(:,:,:,i) = vinterped_data

            ! call this%bc%apply(temp_data)
        enddo
        call output%write(varname, this%data)

    end subroutine apply_bc


end module atmosphere_dataset
