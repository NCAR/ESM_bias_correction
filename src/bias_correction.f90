program bias_correction

    use atmosphere_dataset, only :: atm_t
    use initialization, only :: init

    implicit none

    type(atm_t) :: reference
    type(atm_t) :: esm

    ! initialize datasets, read in lat / lon, set up file and variable names
    call init(reference, esm)

    call reference%generate_means()
    call esm%generate_mean_calculations_for(reference)

    do v = 1, esm%n_variables
        call esm%generate_bc_with(reference, v)

        call esm%apply_bc(v)
    enddo

end program bias_correction
