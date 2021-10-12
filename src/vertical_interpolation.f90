module vertical_interp

    use data_structures
    use vertical_interpolation, only: vLUT, interpolate_vertically

    type vinterp_t
        DOUBLE PRECISION, DIMENSION(:,:,:), allocatable :: z
        type(vert_look_up_table) :: vLUT
    contains
        procedure, public :: set_z => set_z
        procedure, public :: vinterp => vinterp
    end type vinterp_t

Contains
    subroutine set_z(this, z)
        implicit none
        class(vinterp_t), intent(inout) :: this
        DOUBLE PRECISION, DIMENSION(:,:,:), intent(inout) :: z

        allocate(this%z(size(z,1),size(z,2),size(z,3)))
        this%z = z

    end subroutine set_z

    subroutine vinterp(this, z, data, output_data)
        implicit none
        class(vinterp_t), intent(inout) :: this
        real, DIMENSION(:,:,:), intent(in) :: z
        real, DIMENSION(:,:,:), intent(in) :: data

        real, DIMENSION(:,:,:), ALLOCATABLE, intent(inout) :: output_data
        integer :: nx, ny, nz

        nx = size(this%z, 1)
        ny = size(this%z, 2)
        nz = size(this%z, 3)

        call vLUT(this%z, z, this%vLUT)

        if (.not.allocated(output_data)) allocate(output_data(nx, ny, nz))
        call interpolate_vertically(output_data, data, this%vLUT)

    end subroutine vinterp
end module vertical_interp
