module time_periods
    use geographic, only: geo_transform

    type time_period_data_t

        ! internal counters to keep track of the files and timesteps in the reference period
        integer :: file_start, file_end
        integer :: step_start, step_end
        ! internal counters to keep track of a current time in the reference period
        integer :: file, step, n_timesteps
        ! keep a netcdf fileID for the reference data so we don't keep re-opening and closing the netcdf file?
        integer :: ncid

        type(geo_transform), pointer, geo

    contains
        procedure, public  :: init => init
        procedure, public  :: find_period => find_period
        procedure, public  :: next => next
        procedure, public  :: current => current
        procedure, public  :: reset_counter => reset_counter
        procedure, public  :: set_geo_transform => set_geo_transform

    end type time_period_data_t



    subroutine find_period
        implicit none


    end subroutine find_period

    subroutine next
        implicit none


    end subroutine next

    subroutine current
        implicit none


    end subroutine current

    subroutine reset_counter
        implicit none


    end subroutine reset_counter

    subroutine set_geo_transform(this, geo)
        implicit none
        class(time_period_data_t), intent(inout) :: this
        type(geo_transform), intent(inout) :: geo

        this%geo => geo

    end subroutine set_geo_transform


    ! call this%reference%reset_counter()
    ! do i=1, this%reference%n_timesteps
    !     this%z_data = this%z_data + this%reference%next(this%z_name)
    ! enddo


end module time_periods
