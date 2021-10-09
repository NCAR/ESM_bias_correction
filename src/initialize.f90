module initialization
    use atmosphere_dataset, only: atm_t
    implicit none

contains
    subroutine init(reference, esm)
        implicit none
        type(atm_t), intent(inout) :: reference, esm

        call reference%init(filenames=["erai.nc"],          &
                            varnames=[character(len=1024) ::&
                                      "qv","theta"],        &
                            z_name="z",                     &
                            lat_name="lat",                 &
                            lon_name="lon",                 &
                            time_name="time",               &
                            ref_start="2002-01-01 00:00:00",&
                            ref_end="2003-01-01 00:00:00",  &
                            cor_start="2004-01-01 00:00:00",&
                            cor_end="2005-01-01 00:00:00")

        ! initialize the ESM dataset
        call esm%init(      filenames=["esm.nc"],           &
                            varnames=[character(len=1024) ::&
                                      "qv","theta"],        &
                            z_name="z",                     &
                            lat_name="lat",                 &
                            lon_name="lon",                 &
                            time_name="time",               &
                            ref_start="2002-01-01 00:00:00",&
                            ref_end="2003-01-01 00:00:00",  &
                            cor_start="2004-01-01 00:00:00",&
                            cor_end="2005-01-01 00:00:00")


    end subroutine init

end module initialization
