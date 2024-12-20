module time_periods
    use constants
    use geographic, only: geo_transform
    use io_routines, only: io_getdims, io_maxDims, io_read
    use time_io, only : read_times, find_timestep_in_file
    use time_object, only : Time_type
    use time_delta_object, only: time_delta_t

    type time_period_data_t

        ! internal counters to keep track of the files and timesteps in the reference period
        integer :: file_start, file_end
        integer :: step_start, step_end
        ! internal counters to keep track of a current time in the reference period
        integer :: file, step, n_timesteps, all_timesteps
        ! keep a netcdf fileID for the reference data so we don't keep re-opening and closing the netcdf file?
        integer :: ncid

        ! if the time period should exclude some section (e.g. a validation period)
        integer :: start_exclude, end_exclude
        integer :: file_start_exclude, step_start_exclude
        integer :: file_end_exclude, step_end_exclude

        integer, ALLOCATABLE, DIMENSION(:) :: steps_per_file
        integer, ALLOCATABLE, DIMENSION(:) :: file_start_points
        integer :: start_point, end_point
        character(len=kMAX_FILE_LENGTH), allocatable, DIMENSION(:) :: files
        character(len=kMAX_VARNAME_LENGTH) :: time_variable

        type(Time_type), allocatable, dimension(:) :: times
        type(time_delta_t) :: dt

        type(geo_transform), pointer :: geo

    contains
        procedure, public  :: init => init
        procedure, public  :: find_period => find_period
        procedure, public  :: exclude_period => exclude_period
        procedure, public  :: should_exclude => should_exclude
        procedure, public  :: next => next
        procedure, public  :: current => current
        procedure, public  :: reset_counter => reset_counter
        procedure, public  :: set_geo_transform => set_geo_transform
        procedure, public  :: get_times => get_times

    end type time_period_data_t


contains
    subroutine init(this, files, time_variable)
        implicit none
        class(time_period_data_t), intent(inout) :: this
        character(len=*), INTENT(IN) :: files(:)
        character(len=*), INTENT(IN) :: time_variable

        integer :: dims(io_maxDims)
        integer :: i, file_start, file_end
        type(Time_type), allocatable, dimension(:) :: times

        this%all_timesteps = 0

        allocate(this%steps_per_file(size(files)))
        allocate(this%file_start_points(size(files)))
        this%files = files
        this%time_variable = time_variable

        ! find out how many time steps are in each file
        !  also track useful data like the number of time steps in each file
        do i=1, size(files)
            call io_getdims(files(i), time_variable, dims)

            this%all_timesteps = this%all_timesteps + dims(2)
            this%steps_per_file(i) = dims(2)
        enddo
        ! this%all_timesteps = sum(this%steps_per_file)
        print*, "Number of timesteps found dataset:", this%all_timesteps


        allocate(this%times(this%all_timesteps))
        file_start = 1
        ! load the time data into an object variable
        !  also track useful numbers like the time step at the start of each file
        do i=1, size(files)
            call read_times(files(i), time_variable, times)

            file_end = file_start + size(times) - 1

            this%times(file_start:file_end) = times
            this%file_start_points(i) = file_start

            file_start = file_end+1
        enddo

        this%geo => NULL()

        this%start_exclude = -1
        this%end_exclude = -1
        this%file_start_exclude = -1
        this%step_start_exclude = -1
        this%file_end_exclude = -1
        this%step_end_exclude = -1

    end subroutine init


    subroutine find_period(this, start_time, end_time)
        implicit none
        class(time_period_data_t), intent(inout) :: this
        character(len=*), intent(in) :: start_time, end_time

        type(Time_type) :: start_t, end_t
        integer :: start_step, start_file, end_step, end_file

        call start_t%set(start_time)
        call end_t%set(end_time)

        if (end_t < start_t) then
            print*, "ERROR, start of time period is after end of time period"
            print*, "Start:"//trim(start_t%as_string())
            print*, "End:"//trim(end_t%as_string())
        endif

        call find_point_in_files(this, start_t, start_file, start_step, find_before=.False.)
        call find_point_in_files(this, end_t, end_file, end_step, find_before=.True.)

        this%start_point = this%file_start_points(start_file) + start_step - 1
        this%end_point = this%file_start_points(end_file) + end_step - 1
        this%n_timesteps = this%end_point - this%start_point + 1

        this%file_start = start_file
        this%step_start = start_step
        this%file_end = end_file
        this%step_end = end_step
        this%dt = this%times(2) - this%times(1)

    end subroutine find_period

    subroutine exclude_period(this, start_time, end_time)
        implicit none
        class(time_period_data_t), intent(inout) :: this
        character(len=*), intent(in) :: start_time, end_time

        type(Time_type) :: start_t, end_t
        integer :: start_step, start_file, end_step, end_file

        if (this%start_exclude /= -1) then
            print*, "ERROR: can not add two exlusion periods to the same time_period object."
            print*, "or add the same exclusion period twice. Modify code to prevent this. "
            stop "Feature not implemented"
        endif

        call start_t%set(start_time)
        call end_t%set(end_time)

        if (end_t < start_t) then
            print*, "ERROR, start of time period is after end of time period"
            print*, "Start:"//trim(start_t%as_string())
            print*, "End:"//trim(end_t%as_string())
        endif

        call find_point_in_files(this, start_t, start_file, start_step, find_before=.False.)
        call find_point_in_files(this, end_t, end_file, end_step, find_before=.True.)

        this%start_exclude = this%file_start_points(start_file) + start_step - 1
        this%end_exclude = this%file_start_points(end_file) + end_step - 1

        this%file_start_exclude = start_file
        this%step_start_exclude = start_step
        this%file_end_exclude = end_file
        this%step_end_exclude = end_step

        this%n_timesteps = this%n_timesteps - (this%end_exclude - this%start_exclude + 1)

    end subroutine exclude_period


    function get_times(this) result(times)
            implicit none
            class(time_period_data_t), intent(in) :: this
            double precision, allocatable :: times(:)

            integer :: i

            if (this%start_exclude /= -1) then
                print*, "ERROR: can not get times for an object that is excluding some time period. "
                print*, "add logic to skip exclusion time period(?) to get_times function in time_period.f90."
                stop "Feature not implemented"
            endif

            allocate(times(this%n_timesteps))
            do i=1,this%n_timesteps
                times(i) = (i-1) * (this%dt%seconds() / 86400.0) ! convert time delta to days
            enddo

    end function get_times


    subroutine find_point_in_files(this, time, file, step, find_before)
        implicit none
        class(time_period_data_t), intent(in) :: this
        type(Time_type), intent(in) :: time
        integer, intent(inout) :: file, step
        logical, intent(in) :: find_before

        integer :: i, fileend
        LOGICAL :: found

        found = .False.
        file = 1
        step = -1

        i=1
        fileend = this%file_start_points(file) + this%steps_per_file(file) - 1

        do while (.not.found)
            ! if we are off the end of the current file, look at the next file
            if (fileend < i) then
                file = file + 1
                fileend = this%file_start_points(file) + this%steps_per_file(file) - 1
            endif

            ! if the time we are looking for is before the current time, we are done!
            if (time <= this%times(i)) then
                step = i - this%file_start_points(file) + 1
                found = .True.
            else
                ! else prepare to look at the next time step in the file
                i = i+1
                if (i > this%all_timesteps) then
                    found = .True.
                    file = -1
                    step = -1
                    print*, "ERROR: unable to find timestep:", trim(time%as_string())
                    print*, "times(1)=",trim(this%times(1)%as_string())
                    print*, "times(n)=",trim(this%times(this%all_timesteps)%as_string())
                    print*, "n = ", this%all_timesteps
                    file = size(this%file_start_points)
                    step = this%steps_per_file(file)
                endif
            endif
        enddo

        if (step==-1) return

        if (find_before) then
            i = i - 1
            if (i < 1) return

            if (this%file_start_points(file) > i) then
                file = file - 1
                if (file < 1) then
                    write(*,*) "ERROR: unable to find a time step before : ", trim(time%as_string())
                    write(*,*) "first available timestep : ", trim(this%times(1)%as_string())
                    error stop
                endif
            endif
            step = i - this%file_start_points(file) + 1
        endif


    end subroutine find_point_in_files

    function should_exclude(this, step, file) result(in_exclusion)
        implicit none
        class(time_period_data_t), intent(inout) :: this
        integer, intent(in) :: step, file

        logical :: in_exclusion
        integer :: global_step

        global_step = this%file_start_points(file) + step - 1

        in_exclusion = (global_step >= this%start_exclude).and.(global_step <= this%end_exclude)

    end function

    !>------------------------
    !> load next time step for variable
    !>
    !> Increments internal object time step/file reference
    !> then uses "current" to load the data for that timestep
    !>
    !!-------------------------
    function next(this, varname) result(output_data)
        implicit none
        class(time_period_data_t), intent(inout) :: this
        character(len=*), intent(in) :: varname
        real, DIMENSION(:,:,:), ALLOCATABLE :: output_data

        if (this%start_exclude == -1) then
            associate(step => this%step, file=>this%file)
                step = step + 1

                if (step > this%steps_per_file(file)) then
                    print*, trim(this%files(file))
                    file = file + 1
                    step = 1
                    if (file > size(this%files)) then
                        write(*,*) "ERROR: looking for data past the end of the available data"
                        error stop
                    endif
                endif
            end associate
        else
            associate(step => this%step, file=>this%file)
                if (this%should_exclude(step, file)) then
                    do while (this%should_exclude(step, file))
                        step = step + 1

                        if (step > this%steps_per_file(file)) then
                            print*, trim(this%files(file))
                            file = file + 1
                            step = 1
                            if (file > size(this%files)) then
                                write(*,*) "ERROR: looking for data past the end of the available data"
                                error stop
                            endif
                        endif
                    enddo
                else
                    step = step + 1

                    if (step > this%steps_per_file(file)) then
                        print*, trim(this%files(file))
                        file = file + 1
                        step = 1
                        if (file > size(this%files)) then
                            write(*,*) "ERROR: looking for data past the end of the available data"
                            error stop
                        endif
                    endif
                endif
            end associate
        endif

        output_data = this%current(varname)
    end function next

    !>------------------------
    !> load current time step for variable
    !>
    !> Uses internal object time step/file reference
    !> then uses io_read to load the data for that file/timestep
    !>
    !!-------------------------
    function current(this, varname) result(output_data)
        implicit none
        class(time_period_data_t), intent(inout) :: this
        character(len=*), intent(in) :: varname
        real, DIMENSION(:,:,:), ALLOCATABLE :: output_data, input_data

        call io_read(this%files(this%file), &
                    varname,                &
                    input_data,            &
                    extradim = this%step)

        if (ASSOCIATED(this%geo)) then
            output_data = this%geo%interpolate(input_data)
        else
            output_data = input_data
        endif

    end function current

    subroutine reset_counter(this)
        implicit none
        class(time_period_data_t), intent(inout) :: this

        this%file = this%file_start
        this%step = this%step_start - 1

    end subroutine reset_counter

    subroutine set_geo_transform(this, geo)
        implicit none
        class(time_period_data_t), intent(inout) :: this
        type(geo_transform), target, intent(inout) :: geo

        this%geo => geo

    end subroutine set_geo_transform


end module time_periods
