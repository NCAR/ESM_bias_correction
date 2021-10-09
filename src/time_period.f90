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
        init
        find_period
        next
        reset_counter
        set_geo_transform

    end type time_period_data_t

end module time_periods
