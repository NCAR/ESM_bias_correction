module output_dataset
    use netcdf
    use io_routines, only: check
    use constants

    implicit none

    type output_t

        character(len=kMAX_FILE_LENGTH) :: output_file

        integer, allocatable, dimension(:) :: dims
        integer, allocatable, dimension(:) :: varids
        integer :: ncid
        real, allocatable, dimension(:,:,:) :: z
        real, allocatable, dimension(:) :: lat, lon
        ! integer, allocatable, dimension(:) :: dimids

        character(len=kMAX_VARNAME_LENGTH), allocatable, dimension(:) :: dimnames

    contains
        procedure, public  :: init => init
        procedure, public  :: close => close
        procedure, public  :: update_z => update_z
        procedure, public  :: write => write_var
    end type output_t

contains
    subroutine init(this, output_file, varnames, dims, dimnames, z, lat, lon)
        implicit none
        class(output_t), intent(inout) :: this
        character(len=*), intent(in) :: output_file
        character(len=*), dimension(:), intent(in) :: varnames
        integer,          dimension(:), intent(inout) :: dims
        character(len=*), dimension(:), intent(in) :: dimnames
        real, intent(in) :: z(:,:,:), lat(:), lon(:)

        integer :: i

        ! fourth dimension is hard coded as time
        dims(4) = NF90_UNLIMITED
        this%dims = dims
        this%dimnames = dimnames
        this%output_file = output_file
        ! save data in case we ever write more than one file.
        this%lat = lat
        this%lon = lon
        this%z = z

        allocate(this%varids(size(varnames)))

        print*, "Writing to outputfile: ", trim(output_file)
        call initialize_output_file(output_file, varnames, dimnames, dims, z, lat, lon, this%ncid, this%varids)

    end subroutine init

    ! subroutine write(this, varname, data_array)
    !     implicit none
    !     class(output_t), intent(inout) :: this
    !     character(len=*), intent(in) :: varname
    !     real, DIMENSION(:,:,:,:) :: data_array
    !
    !     print*, trim(varname), minval(data_array), maxval(data_array)
    !     call write_var(trim(varname)//"_"//trim(this%output_file), varname, data_array, this%ncid)
    !
    ! end subroutine write


    subroutine initialize_output_file(filename, varnames, dims, dim_sizes, z, lat, lon, ncid, varids)
            implicit none
            ! This is the name of the file and variable we will write.
            character(len=*), intent(in) :: filename
            character(len=*), intent(in), dimension(:) :: dims, varnames
            integer, intent(inout), dimension(:) :: dim_sizes

            real, intent(in) :: z(:,:,:), lat(:), lon(:)

            ! This will be the netCDF ID for the file and data variable.
            integer, intent(inout) :: ncid
            integer, intent(inout), dimension(:)  :: varids

            integer, parameter :: ndims = 4
            integer :: temp_dimid,dimids(ndims), temp_varid, coord_varids(3)
            integer :: i

            ! Open the file. NF90_NOWRITE tells netCDF we want read-only access to
            ! the file.
            call check( nf90_create(filename, NF90_CLOBBER, ncid), filename)

            ! define the dimensions
            do i=1,size(dims)
                call check( nf90_def_dim(ncid, trim(adjustl(dims(i))), dim_sizes(i), temp_dimid), &
                            "def_dim:"//adjustl(trim(dims(i))))
                dimids(i) = temp_dimid
            enddo

            ! Create the variable returns varid of the data variable
            do i=1, size(varnames)
                call check( nf90_def_var(ncid, trim(varnames(i)), NF90_REAL, dimids, temp_varid), &
                            trim(filename)//":"//trim(varnames(i)))
                varids(i) = temp_varid
            enddo

            ! setup coordinate variables (don't write time yet?)
            call check( nf90_def_var(ncid, "time", NF90_REAL, dimids(4), temp_varid), trim(filename)//":"//trim("time"))
            ! call check( nf90_put_var(ncid, temp_varid, times), trim(filename)//":"//trim("time"))

            call check( nf90_def_var(ncid, "lat", NF90_REAL, dimids(2), temp_varid), trim(filename)//":"//trim("lat"))
            coord_varids(1) = temp_varid

            call check( nf90_def_var(ncid, "lon", NF90_REAL, dimids(1), temp_varid), trim(filename)//":"//trim("lon"))
            coord_varids(2) = temp_varid

            call check( nf90_def_var(ncid, "z", NF90_REAL, dimids(:3), temp_varid), trim(filename)//":"//trim("z"))
            coord_varids(3) = temp_varid


            ! End define mode. This tells netCDF we are done defining metadata.
            call check( nf90_enddef(ncid) )

            call check( nf90_put_var(ncid, coord_varids(1), lat), "put:"//trim(filename)//":"//trim("lat"))
            call check( nf90_put_var(ncid, coord_varids(2), lon), "put:"//trim(filename)//":"//trim("lon"))
            call check( nf90_put_var(ncid, coord_varids(3), z), "put:"//trim(filename)//":"//trim("z"))
    end subroutine initialize_output_file

    !>------------------------------------------------------------
    !! Same as io_write6d but for 4-dimensional data
    !!
    !! Write a 4-dimensional variable to a netcdf file
    !!
    !! Create a netcdf file:filename with a variable:varname and write data_out to it
    !!
    !! @param   filename    Name of NetCDF file to write/create
    !! @param   varname     Name of the NetCDF variable to write
    !! @param   data_out    4-dimensional array to write to the file
    !!
    !!------------------------------------------------------------
    subroutine write_var(this, varname, data_out)
        implicit none
        class(output_t), intent(inout) :: this
        ! This is the name of the file and variable we will write.
        character(len=*), intent(in) :: varname
        real,intent(in) :: data_out(:,:,:,:)

        character(len=kMAX_FILE_LENGTH) :: filename
        integer :: ncid

        ! We are writing 4D data, assume a nx x nz x ny x nr grid.
        integer :: nx,ny,nz,nr

        ! This will be the netCDF ID for the file and data variable.
        integer :: varid

        filename = "output.nc" !, this%filename
        ncid = this%ncid
        varid = -1
        ! Open the file. NF90_WRITE tells netCDF we want write access to
        ! the file.
        if (ncid==-1) then
            call check( nf90_open(filename, NF90_WRITE, ncid), filename)
        endif

        if (varid==-1) then
            call check( nf90_inq_varid(ncid, varname, varid), "varid:"//trim(varname))
        endif

        ! ideally should look to see what stage the file is at and append new data to the end.... have to think about that some

        print*, "writing data: ", trim(varname), shape(data_out)
        ! write the actual data to the file
        call check( nf90_put_var(ncid, varid, data_out), trim(varname))

    end subroutine write_var

    !>------------------------------------------------------------
    !! Same as io_write6d but for 4-dimensional data
    !!
    !! Write a 4-dimensional variable to a netcdf file
    !!
    !! Create a netcdf file:filename with a variable:varname and write data_out to it
    !!
    !! @param   filename    Name of NetCDF file to write/create
    !! @param   varname     Name of the NetCDF variable to write
    !! @param   data_out    4-dimensional array to write to the file
    !!
    !!------------------------------------------------------------
    subroutine update_z(this, varname, data_out)
        implicit none
        class(output_t), intent(inout) :: this
        ! This is the name of the file and variable we will write.
        character(len=*), intent(in) :: varname
        real(kind=dp),intent(in) :: data_out(:,:,:)

        character(len=kMAX_FILE_LENGTH) :: filename
        integer :: ncid

        ! We are writing 3D data, assume a nx x nz x ny x nr grid.
        integer :: nx,ny,nz

        ! This will be the netCDF ID for the file and data variable.
        integer :: varid

        filename = "output.nc" !, this%filename
        ncid = this%ncid
        varid = -1
        ! Open the file. NF90_WRITE tells netCDF we want write access to
        ! the file.
        if (ncid==-1) then
            call check( nf90_open(filename, NF90_WRITE, ncid), filename)
        endif

        if (varid==-1) then
            call check( nf90_inq_varid(ncid, varname, varid), "varid:"//trim(varname))
        endif

        ! ideally should look to see what stage the file is at and append new data to the end.... have to think about that some

        ! write the actual data to the file
        call check( nf90_put_var(ncid, varid, data_out), trim(varname))

    end subroutine update_z



    subroutine close(this)
        implicit none
        class(output_t), intent(inout) :: this

        ! Close the file, freeing all resources.
        call check( nf90_close(this%ncid), "Closing file")
    end subroutine close


end module output_dataset
