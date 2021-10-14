module geographic

    use data_structures, only: interpolable_type
    use geo, only: geo_LUT, geo_interp

    type geo_transform
        real, DIMENSION(:,:), ALLOCATABLE :: lat, lon
        type(interpolable_type) :: input, output
        logical :: geoLUT_exists
    contains
        procedure, public :: init => init
        procedure, public :: setup_geo_LUT => setup_geo_LUT
        procedure, public :: interpolate => interpolate

    end type geo_transform

contains
    subroutine init(this, lat, lon)
        implicit none
        class(geo_transform), intent(inout) :: this
        real, DIMENSION(:,:), intent(in) :: lat, lon

        this%lat = lat
        this%lon = lon

        this%geoLUT_exists = .False.

    end subroutine init

    subroutine setup_geo_LUT(this, reference_geo)
        implicit none
        class(geo_transform), intent(inout) :: this
        class(geo_transform), intent(in) :: reference_geo

        this%output%lat = this%lat
        this%output%lon = this%lon

        this%input%lat = reference_geo%lat
        this%input%lon = reference_geo%lon

        call geo_LUT(this%input, this%output)

        this%geoLUT_exists = .True.

    end subroutine setup_geo_LUT

    function interpolate(this, input_data) result(output_data)
        implicit none
        class(geo_transform), intent(inout) :: this
        real, DIMENSION(:,:,:), intent(in) :: input_data

        real, DIMENSION(:,:,:), allocatable :: output_data

        integer :: nx, ny, nz

        nx = size(this%output%geolut%x,2)
        ny = size(this%output%geolut%x,3)
        nz = size(input_data,3)
        allocate(output_data(nx, ny, nz))

        call geo_interp(output_data, input_data, this%output%geolut)

    end function interpolate

end module geographic
