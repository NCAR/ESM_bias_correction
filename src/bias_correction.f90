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

    print*, "reference%generate_means()"
    call reference%generate_means()

    print*, "output%update_z"
    call output%update_z(reference%z_name, reference%z_data)

    print*, "esm%generate_mean_calculations_for(reference)"
    call esm%generate_mean_calculations_for(reference)

    call esm%add_exclusions()

    do v = 1, esm%n_variables

        print*, "call esm%generate_bc_with", v
        call esm%generate_bc_with(reference, v)

        print*, "call esm%apply_bc(v, output)"
        call esm%apply_bc(v, output)
    enddo

    ! output time is now written on output%init, as a direct copy from esm, but not it's attributes!! (input and output should use days since 1900 !) (BK 2023/03)
    ! call output%write_time(esm%get_output_times())


    call output%close()

    print*, " "
    print*, " - - - - -   Finished ESM bias correction ! - - - - - - "
    print*, " "

end program esm_bias_correction
