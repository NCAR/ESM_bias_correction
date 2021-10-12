module data_structures

    implicit none

    ! ------------------------------------------------
    !   various data structures for use in geographic interpolation routines
    ! ------------------------------------------------
    ! contains the location of a specific grid point
    type position
        integer::x,y
    end type position
    ! contains location of surrounding 4 grid cells
    type fourpos
        integer::x(4),y(4)
    end type fourpos

    ! a geographic look up table for spatial interpolation, from x,y with weight w
    type geo_look_up_table
        ! x,y index positions, [n by m by 4] where there are 4 surrounding low-res points
        ! for every high resolution point grid point to interpolate to
        integer,allocatable, dimension(:,:,:)   :: x, y
        ! weights to use for each of the 4 surrounding gridpoints.  Sum(over axis 3) must be 1.0
        real,   allocatable, dimension(:,:,:)   :: w
    end type geo_look_up_table


    ! ------------------------------------------------
    ! generic interpolable type so geo interpolation routines will work
    ! ------------------------------------------------
    type interpolable_type
        ! all interpolables must have position (lat, lon)
        real, allocatable, dimension(:,:) :: lat,lon

        ! these are the look up tables that describe how to interpolate vertically (vert_lut) and horizontally (geolut)
        type(geo_look_up_table)::geolut

        logical :: dy_errors_printed, dx_errors_printed

    end type interpolable_type


    ! ------------------------------------------------
    ! A look up table for vertical interpolation. from z with weight w
    ! ------------------------------------------------
    type vert_look_up_table
        ! z index positions for all x,y,z points (x 2 for above and below z levels)
        integer,allocatable, dimension(:,:,:,:) :: z

        ! weights to use for each of the two surrounding points.  Sum (over axis 1) must be 1.0
        real,   allocatable, dimension(:,:,:,:) :: w
    end type vert_look_up_table

end module data_structures
