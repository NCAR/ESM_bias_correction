module qm_obj

    type qm_transform
        type(qm_data) :: qm

    contains
        develop
        apply_bc
    end type qm_transform

end module qm_obj
