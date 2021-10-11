program esm_bias_correction

    use atmosphere_dataset, only: atm_t
    use output_dataset,     only: output_t
    use initialization,     only: init

    implicit none

    type(atm_t)     :: reference    !> "truth" dataset, e.g. ERA5
    type(atm_t)     :: esm          !> ESM model to correct, e.g. CESM2
    type(output_t)  :: output       !> handles writing corrected output to a file
    integer :: v

    ! initialize datasets, read in lat / lon, set up file and variable names
    call init(reference, esm, output)

    call reference%generate_means()
    call esm%generate_mean_calculations_for(reference)

    do v = 1, esm%n_variables

        print*, "call esm%generate_bc_with", v
        call esm%generate_bc_with(reference, v)

        print*, "call esm%apply_bc(v, output)"
        call esm%apply_bc(v, output)
    enddo

end program esm_bias_correction
