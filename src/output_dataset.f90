module output_dataset
    ! use netcdf
    use io_routines, only: io_write
    use constants

    implicit none

    type output_t

        character(len=kMAX_FILE_LENGTH) :: output_file

        integer, allocatable, dimension(:) :: dims

        character(len=kMAX_VARNAME_LENGTH), allocatable, dimension(:) :: dimnames

    contains
        procedure, public  :: init => init
        procedure, public  :: write => write
    end type output_t

contains
    subroutine init(this, output_file, dims, dimnames)
        implicit none
        class(output_t), intent(inout) :: this
        character(len=*), intent(in) :: output_file
        integer,          dimension(:), intent(in) :: dims
        character(len=*), dimension(:), intent(in) :: dimnames

        this%dims = dims
        this%dimnames = dimnames
        this%output_file = output_file

    end subroutine init

    subroutine write(this, varname, data_array)
        implicit none
        class(output_t), intent(inout) :: this
        character(len=*), intent(in) :: varname
        real, DIMENSION(:,:,:,:) :: data_array

        print*, trim(varname), minval(data_array), maxval(data_array)
        call io_write(trim(varname)//"_"//trim(this%output_file), varname, data_array)!, this%dimnames)

    end subroutine write

end module output_dataset
