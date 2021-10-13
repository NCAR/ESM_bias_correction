module bias_correction_obj

    use data_structures, only: qm_correction_type
    use quantile_mapping, only: develop_qm, apply_qm

    type bc_transform
        type(qm_correction_type), DIMENSION(:,:,:), allocatable :: qm

    contains
        procedure, public :: develop => develop
        procedure, public :: apply => apply
    end type bc_transform

contains

    ! develop any bias correction technique.
    ! input is the 4-D biased data to be corrected and the 4-D unbiased data
    ! calls the develop_qm routine to develop a separate quantile mapping for each grid cell
    subroutine develop(this, biased_data, unbiased_data)
        implicit none
        class(bc_transform), INTENT(INOUT) :: this
        real, DIMENSION(:,:,:,:), intent(in) :: biased_data, unbiased_data

        ! before computing a correction, the data need to be isolated to a 1D time series
        real, DIMENSION(:), allocatable :: input_data, match_data
        ! holds the quantile mapping developed for one point
        type(qm_correction_type) :: qm
        ! loop index variables
        integer :: nx, ny, nz
        integer :: i, j, k
        ! number of quantiles in the quantile map
        integer :: n_segments

        nx = size(biased_data, 1)
        ny = size(biased_data, 2)
        nz = size(biased_data, 3)

        n_segments = 10

        ! allocate quantile mapping objects for each grid cell
        if (allocated(this%qm)) deallocate(this%qm)
        ALLOCATE(this%qm(nx, ny, nz))

        do k = 1, nz
            do j = 1, ny
                do i = 1, nz
                    input_data = biased_data(i,j,k,:)
                    match_data = unbiased_data(i,j,k,:)

                    call develop_qm(input_data, match_data, qm, n_segments)

                    this%qm(i,j,k) = qm
                enddo
            enddo
        enddo

    end subroutine develop


    ! apply a separate quantile mapping correction for each grid cell.
    subroutine apply(this, biased_data)
        implicit none
        class(bc_transform), INTENT(INOUT) :: this
        real, DIMENSION(:,:,:), intent(inout) :: biased_data

        ! before applying a correction, the data need to be isolated to a 1D time series
        real, DIMENSION(:), allocatable :: input_data, output_data
        type(qm_correction_type) :: qm
        integer :: nx, ny, nz
        integer :: i, j, k

        nx = size(biased_data, 1)
        ny = size(biased_data, 2)
        nz = size(biased_data, 3)

        ! at the moment, biased_data are input one timestep at a time...
        ! it might be more efficient to process more data at once.
        allocate(input_data(1))
        allocate(output_data(1))

        do k = 1, nz
            do j = 1, ny
                do i = 1, nz
                    input_data(1) = biased_data(i,j,k)
                    qm = this%qm(i,j,k)

                    call apply_qm(input_data, output_data, qm)

                    biased_data(i,j,k) = output_data(1)
                enddo
            enddo
        enddo

    end subroutine apply


end module bias_correction_obj
