module data_structures

    implicit none
    type vert_look_up_table
        integer, ALLOCATABLE :: z(:,:,:,:)
        real, ALLOCATABLE :: w(:,:,:,:)
    end type vert_look_up_table

end module data_structures
