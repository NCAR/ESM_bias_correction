module bias_correction_obj

    use data_structures, only: qm_correction_type

    type qm_transform
        type(qm_correction_type) :: qm

    contains
        procedure, public :: develop => develop
        procedure, public :: apply => apply
    end type qm_transform

contains

    subroutine develop(this, biased_data, unbiased_data)
        implicit none
        class(qm_transform), INTENT(INOUT) :: this
        real, DIMENSION(:,:,:,:), intent(in) :: biased_data, unbiased_data



    end subroutine develop


    subroutine apply(this, biased_data)
        implicit none
        class(qm_transform), INTENT(INOUT) :: this
        real, DIMENSION(:,:,:), intent(inout) :: biased_data


    end subroutine apply


end module bias_correction_obj
