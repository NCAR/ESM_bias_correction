module time_periods
    use constants
    use geographic, only: geo_transform

    type time_period_data_t

        ! internal counters to keep track of the files and timesteps in the reference period
        integer :: file_start, file_end
        integer :: step_start, step_end
        ! internal counters to keep track of a current time in the reference period
        integer :: file, step, n_timesteps
        ! keep a netcdf fileID for the reference data so we don't keep re-opening and closing the netcdf file?
        integer :: ncid

        character(len=kMAX_FILE_LENGTH), allocatable, DIMENSION(:) :: files
        character(len=kMAX_VARNAME_LENGTH) :: time_variable

        type(geo_transform), pointer :: geo

    contains
        procedure, public  :: init => init
        procedure, public  :: find_period => find_period
        procedure, public  :: next => next
        procedure, public  :: current => current
        procedure, public  :: reset_counter => reset_counter
        procedure, public  :: set_geo_transform => set_geo_transform

    end type time_period_data_t


contains
    subroutine init(this, files, time_variable)
        implicit none
        class(time_period_data_t), intent(inout) :: this
        character(len=*), INTENT(IN) :: files(:)
        character(len=*), INTENT(IN) :: time_variable

        ! allocate(this%files(size(files)))
        this%files = files
        this%time_variable = time_variable

    end subroutine init


    subroutine find_period(this, start_time, end_time)
        implicit none
        class(time_period_data_t), intent(inout) :: this
        character(len=*), intent(in) :: start_time, end_time


    end subroutine find_period

    subroutine next(this)
        implicit none
        class(time_period_data_t), intent(inout) :: this


    end subroutine next

    subroutine current(this)
        implicit none
        class(time_period_data_t), intent(inout) :: this


    end subroutine current

    subroutine reset_counter(this)
        implicit none
        class(time_period_data_t), intent(inout) :: this


    end subroutine reset_counter

    subroutine set_geo_transform(this, geo)
        implicit none
        class(time_period_data_t), intent(inout) :: this
        type(geo_transform), target, intent(inout) :: geo

        this%geo => geo

    end subroutine set_geo_transform


end module time_periods
