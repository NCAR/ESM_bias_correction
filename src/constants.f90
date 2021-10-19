module constants

    implicit none

    integer, PARAMETER :: dp = selected_real_kind(10)

    integer, PARAMETER :: kMAX_FILE_LENGTH = 1024
    integer, PARAMETER :: kMAX_VARNAME_LENGTH = 1024
    integer, PARAMETER :: MAXSTRINGLENGTH = 1024
    integer, PARAMETER :: kMAX_NUMBER_FILES = 2400 ! monthly files for 200 years

    integer, PARAMETER :: kMAINTAIN_LON = 1
    integer, PARAMETER :: kDATELINE_CENTERED = 2
    integer, PARAMETER :: kPRIME_CENTERED = 3
    integer, PARAMETER :: kGUESS_LON = 4

end module constants
