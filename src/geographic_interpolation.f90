module geographic

    type geo_transform
        real, DIMENSION(:,:), ALLOCATABLE :: lat, lon

    contains
        procedure, public :: init => init

    end type geo_transform

contains
    subroutine init(this, lat, lon)
        implicit none
        class(geo_transform), intent(inout) :: this
        real, DIMENSION(:,:), intent(in) :: lat, lon

        this%lat = lat
        this%lon = lon

    end subroutine init

end module geographic
