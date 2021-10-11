module initialization
    use atmosphere_dataset, only: atm_t
    use output_dataset,     only: output_t
    use constants

    implicit none

contains
    subroutine init(reference, esm, output)
        implicit none
        type(atm_t), intent(inout) :: reference, esm
        type(output_t), intent(inout) :: output

        call reference%init(filenames=[character(len=1024)::&
                                      "erai.nc"],           &
                            varnames=[character(len=1024) ::&
                                      "qv","theta"],        &
                            z_name="z",                     &
                            lat_name="lat",                 &
                            lon_name="lon",                 &
                            time_name="time",               &
                            ref_start="2000-01-03 00:00:00",&
                            ref_end="2000-01-05 00:00:00",  &
                            cor_start="2000-01-01 12:00:00",&
                            cor_end="2000-01-03 00:00:00")

        ! initialize the ESM dataset
        call esm%init(      filenames=[character(len=1024)::&
                                      "esm.nc"],            &
                            varnames=[character(len=1024) ::&
                                      "qv","theta"],        &
                            z_name="z",                     &
                            lat_name="lat",                 &
                            lon_name="lon",                 &
                            time_name="time",               &
                            ref_start="2000-01-03 00:00:00",&
                            ref_end="2000-01-05 00:00:00",  &
                            cor_start="2000-01-01 12:00:00",&
                            cor_end="2000-01-03 00:00:00")


        call output%init("output.nc", [12, 10, 8], ["  x", "  y", "lev"])


    end subroutine init

end module initialization
