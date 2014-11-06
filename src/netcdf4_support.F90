!> @file
!!  Contains a single module, @ref netcdf4_support, which
!!  provides support for use of NetCDF files as input or output.
!!
!! Supports use of NetCDF files as input for time-varying,
!! gridded meteorlogic data, or output for any SWB-generated variable.
!!
!! from the C API:
!! The @c nc_get_vars_ type family of functions read a subsampled (strided)
!! array section of values from a netCDF variable of an open netCDF dataset.
!! The subsampled array section is specified by giving a corner,
!! a vector of edge lengths, and a stride vector. The values are read
!! with the last dimension of the netCDF variable varying fastest.
!!          ^^^^                                  ^^^^^^^ ^^^^^^^
!!
!! from the Fortran 90 API:
!! The values to be read are associated with the netCDF variable by
!! assuming that the first dimension of the netCDF variable
!!                   ^^^^^
!! varies fastest in the Fortran 90 interface.
!! ^^^^^^ ^^^^^^^

!> Provide support for use of NetCDF files as input for time-varying,
!! gridded meteorlogic data, or output for any SWB-generated variable.

module netcdf4_support

  use iso_c_binding
  use constants_and_conversions
  use datetime
  use exceptions
  use logfiles
  use netcdf_c_api_interfaces
  use strings
  use string_list
  use swb_grid
  implicit none

  private

  public :: NC_FILL_FLOAT

  integer(kind=c_int), public :: NC_READONLY          = 0
  integer(kind=c_int), public :: NC_READWRITE         = 1

  integer(kind=c_int), parameter, public ::  NC_NAT    = 0
  integer(kind=c_int), parameter, public ::  NC_BYTE   = 1
  integer(kind=c_int), parameter, public ::  NC_CHAR   = 2
  integer(kind=c_int), parameter, public ::  NC_SHORT  = 3
  integer(kind=c_int), parameter, public ::  NC_INT    = 4
  integer(kind=c_int), parameter, public ::  NC_FLOAT  = 5
  integer(kind=c_int), parameter, public ::  NC_DOUBLE = 6

  integer(kind=c_int), parameter :: NC_FILL_CHAR    = 0
  integer(kind=c_int), parameter :: NC_FILL_BYTE    = -127
  integer(kind=c_int), parameter :: NC_FILL_SHORT   = -32767
  integer(kind=c_int), parameter :: NC_FILL_INT     = -2147483647
  real(kind=c_float),  parameter :: NC_FILL_FLOAT   = 9.9692099683868690e+36
  real(kind=c_double), parameter :: NC_FILL_DOUBLE  = 9.9692099683868690d+36

  ! mode flags for opening and creating datasets
  integer(kind=c_int), parameter :: NC_NOWRITE          = 0
  integer(kind=c_int), parameter :: NC_WRITE            = 1
  integer(kind=c_int), parameter :: NC_CLOBBER          = 0
  integer(kind=c_int), parameter :: NC_NOCLOBBER        = 4
  integer(kind=c_int), parameter :: NC_FILL             = 0
  integer(kind=c_int), parameter :: NC_NOFILL           = 256
  integer(kind=c_int), parameter :: NC_LOCK             = 1024
  integer(kind=c_int), parameter :: NC_SHARE            = 2048
  integer(kind=c_int), parameter :: NC_STRICT_NC3       = 8
  integer(kind=c_int), parameter :: NC_64BIT_OFFSET     = 512
  integer(kind=c_int), parameter :: NC_SIZEHINT_DEFAULT = 0
  integer(kind=c_int), parameter :: NC_ALIGN_CHUNK      = -1
  integer(kind=c_int), parameter :: NC_FORMAT_CLASSIC   = 1
  integer(kind=c_int), parameter :: NC_FORMAT_64BIT     = 2
  integer(kind=c_int), parameter :: NC_FORMAT_NETCDF4   = 3
  integer(kind=c_int), parameter :: NC_FORMAT_NETCDF4_CLASSIC = 4

  ! implementation limits (warning!  should be the same as c interface)
  integer(kind=c_int), parameter :: NC_MAX_DIMS     = 1024
  integer(kind=c_int), parameter :: NC_MAX_ATTRS    = 8192
  integer(kind=c_int), parameter :: NC_MAX_VARS     = 8192
  integer(kind=c_int), parameter :: NC_MAX_NAME     = 256

  integer (kind=c_int), parameter :: NC_SHUFFLE_YES = 1
  integer (kind=c_int), parameter :: NC_SHUFFLE_NO = 0
  integer (kind=c_int), parameter :: NC_DEFLATE_YES = 1
  integer (kind=c_int), parameter :: NC_DEFLATE_NO = 0

  integer(kind=c_int), parameter :: NC_NETCDF4        = 4096
  integer(kind=c_int), parameter :: NC_CLASSIC_MODEL  = 256


  integer(kind=c_int),  parameter :: NC_UNLIMITED = 0
  integer(kind=c_int),  parameter :: NC_GLOBAL    = -1

  integer (kind=c_int), parameter :: NC_FIRST = 0
  integer (kind=c_int), parameter :: NC_LAST  = 1
  integer (kind=c_int), parameter :: NC_BY    = 2

  integer (kind=c_int), public, parameter :: NC_LEFT  = 0
  integer (kind=c_int), public, parameter :: NC_RIGHT = 1
  integer (kind=c_int), public, parameter :: NC_TOP    = 0
  integer (kind=c_int), public, parameter :: NC_BOTTOM = 1

  integer (kind=c_int), parameter :: COLUMN = 1
  integer (kind=c_int), parameter :: ROW = 2

  character (len=25), dimension(4), parameter :: NETCDF_FORMAT_STRING = &
    ["NC_FORMAT_CLASSIC        ", &
     "NC_FORMAT_64BIT          ", &
     "NC_FORMAT_NETCDF4        ", &
     "NC_FORMAT_NETCDF4_CLASSIC" ]

  character (len=6), dimension(0:6), parameter :: NETCDF_DATA_TYPE = &
    ["nat   ", &
     "byte  ", &
     "char  ", &
     "short ", &
     "int   ", &
     "float ", &
     "double" ]

  type NETCDF4_DIMENSION_T
    character (len=64)           :: sDimensionName
    integer (kind=c_int)         :: iDimensionID          = NC_FILL_INT
    integer (kind=c_size_t)      :: iDimensionSize        = NC_FILL_INT
    logical (kind=c_bool)        :: lUnlimited            = lFALSE
  end type NETCDF4_DIMENSION_T

  type NETCDF4_ATTRIBUTE_T
    character (len=64)                         :: sAttributeName
    type (STRING_LIST_T)                       :: slValues
    integer (kind=c_short), allocatable        :: i2Values(:)
    integer (kind=c_int), allocatable          :: iValues(:)
    real (kind=c_float), allocatable           :: rValues(:)
    real (kind=c_double), allocatable          :: dpValues(:)
    integer (kind=c_int)                       :: iAttributeType
    integer (kind=c_size_t)                    :: iAttributeSize
  end type NETCDF4_ATTRIBUTE_T

  type NETCDF4_VARIABLE_T
    character (len=64)                       :: sVariableName         = ""
    integer (kind=c_int)                     :: iVariableID           = NC_FILL_INT
    integer (kind=c_int)                     :: iVariableType
    character (len=64)                       :: sVariableUnits        = "NA"
    integer (kind=c_int)                     :: iNumberOfDimensions
    integer (kind=c_int)                     :: iDimensionID(0:3)     = NC_FILL_INT
    real (kind=c_double)                     :: rScaleFactor          = 1.0_c_double
    real (kind=c_double)                     :: rAddOffset            = 0.0_c_double
    integer (kind=c_int)                     :: iNumberOfAttributes
    integer (kind=c_int)                     :: iStart                = NC_FILL_INT
    integer (kind=c_int)                     :: iCount                = NC_FILL_INT
    integer (kind=c_int)                     :: iStride               = NC_FILL_INT
    type (NETCDF4_ATTRIBUTE_T), pointer      :: pNC_ATT(:)            => null()
  end type NETCDF4_VARIABLE_T

  type NETCDF4_FILE_T
    integer (kind=c_int) :: iNCID
    character (len=256)  :: sFilename
    integer (kind=c_int) :: iFileFormat
    integer (kind=c_int) :: iNC3_UnlimitedDimensionNumber
    integer (kind=c_int) :: iOriginJD
    integer (kind=c_int) :: iFirstDayJD
    integer (kind=c_int) :: iLastDayJD
    integer (kind=c_int) :: iOriginMonth
    integer (kind=c_int) :: iOriginDay
    integer (kind=c_int) :: iOriginYear
    integer (kind=c_int) :: iOriginHH
    integer (kind=c_int) :: iOriginMM
    integer (kind=c_int) :: iOriginSS
    integer (kind=c_int) :: lLeapYearTreatment
    integer (kind=c_size_t), dimension(0:1) :: iColBounds
    integer (kind=c_size_t), dimension(0:1) :: iRowBounds
    integer (kind=c_int) :: iNX
    integer (kind=c_int) :: iNY
    character (len=3) :: sVariableOrder = "tyx"
    real (kind=c_double), dimension(0:1) :: rX
    real (kind=c_double), dimension(0:1) :: rY
    logical (kind=c_bool) :: lX_IncreasesWithIndex = lTRUE
    logical (kind=c_bool) :: lY_IncreasesWithIndex = lFALSE

    real (kind=c_double), dimension(0:1) :: dpFirstAndLastTimeValues

    integer (kind=c_int), dimension(0:2) :: iRowIter
    integer (kind=c_int), dimension(0:2) :: iColIter
    logical (kind=c_bool)                :: lFlipHorizontal = lFALSE
    logical (kind=c_bool)                :: lFlipVertical = lFALSE

    real (kind=c_double), allocatable    :: rX_Coords(:)
    real (kind=c_double), allocatable    :: rY_Coords(:)
    real (kind=c_double), allocatable    :: rDateTimeValues(:)
    real (kind=c_double)                 :: rGridCellSizeX
    real (kind=c_double)                 :: rGridCellSizeY

    type (NETCDF4_DIMENSION_T), pointer  :: pNC_DIM(:)      => null()
    type (NETCDF4_VARIABLE_T), pointer   :: pNC_VAR(:)      => null()
    type (NETCDF4_ATTRIBUTE_T), pointer  :: pNC_ATT(:)      => null()

    type (NETCDF4_DIMENSION_T), pointer  :: pNC_DIM_X       => null()
    type (NETCDF4_DIMENSION_T), pointer  :: pNC_DIM_Y       => null()
    type (NETCDF4_DIMENSION_T), pointer  :: pNC_DIM_Z       => null()      
    type (NETCDF4_DIMENSION_T), pointer  :: pNC_DIM_TIME    => null()        

    type (NETCDF4_VARIABLE_T), pointer   :: pNC_VAR_X       => null()
    type (NETCDF4_VARIABLE_T), pointer   :: pNC_VAR_Y       => null()
    type (NETCDF4_VARIABLE_T), pointer   :: pNC_VAR_LAT     => null()
    type (NETCDF4_VARIABLE_T), pointer   :: pNC_VAR_LON     => null()      
    type (NETCDF4_VARIABLE_T), pointer   :: pNC_VAR_TIME    => null()

    type (NETCDF4_ATTRIBUTE_T), pointer  :: pNC_ATT_X       => null()
    type (NETCDF4_ATTRIBUTE_T), pointer  :: pNC_ATT_Y       => null()
    type (NETCDF4_ATTRIBUTE_T), pointer  :: pNC_ATT_LAT     => null()
    type (NETCDF4_ATTRIBUTE_T), pointer  :: pNC_ATT_LON     => null()      
    type (NETCDF4_ATTRIBUTE_T), pointer  :: pNC_ATT_TIME    => null()

    type (NETCDF4_VARIABLE_T), pointer   :: pNC_VAR_VALUES(:)    => null()

  contains

    procedure :: nf_date_within_range
    generic   :: is_date_valid => nf_date_within_range

    procedure :: nf_deallocate_data_struct
    generic   :: deallocate => nf_deallocate_data_struct

    procedure :: nf_nullify_data_struct
    generic   :: nullify => nf_nullify_data_struct

    procedure :: nf_dump_cdl

    procedure :: nf_open_file_readonly
    generic   :: open => nf_open_file_readonly

    procedure :: nf_close_file
    generic   :: close => nf_close_file

    procedure :: nf_get_time_units
    procedure :: nf_get_time_values

    procedure :: nf_get_variable_units 

  end type NETCDF4_FILE_T

!
!                  /\
!  coordinates     |   |
!  increase        |   |
!  in upward       |   |
!  direction       |   |
!                  |   |
!                  |   | column index increases in downward direction
!                  |   |
!                  |   |
!                      \/
!
!

  public :: NETCDF4_DIMENSION_T, NETCDF4_VARIABLE_T, NETCDF4_ATTRIBUTE_T
  public :: NETCDF4_FILE_T
  
contains

!--------------------------------------------------------------------------------------------------

!> Test whether a given Julian Day is within the range provided in the NetCDF file.
!!
!! @param[inout] this Object of class NETCDF4_FILE_T).
!! @param[in]    iJulianDay Julian Day for the date of interest.
!! @retval       lWithinRange TRUE if iJulianDate is within the date range of dates provided in the 
!!               NetCDF file.

function nf_date_within_range( this, iJulianDay )  result( lWithinRange )

  class (NETCDF4_FILE_T )             :: this
  integer (kind=c_int), intent(in)    :: iJulianDay
  logical (kind=c_bool)               :: lWithinRange

  if ( iJulianDay >= this%iFirstDayJD &
      .and. iJulianDay <= this%iLastDayJD ) then

    lWithinRange = lTRUE

  else

    lWithinRange = lFALSE

  endif

end function nf_date_within_range

!--------------------------------------------------------------------------------------------------

!> This was so clear the other day. Basically, I think we need
!> two functions to convert from index to timeval, and timeval to JD;
!> note that timeval refers to the number of days from the origin
!> of the NetCDF file

!> return the day value (number of days since origin

function nf_julian_day_to_index(this, rJulianDay)  result (iIndex)

  class (NETCDF4_FILE_T )             :: this
  real (kind=c_double), intent(in)    :: rJulianDay
  integer (kind=c_int)                :: iIndex

  iIndex = aint(rJulianDay) - this%iFirstDayJD

end function nf_julian_day_to_index

!--------------------------------------------------------------------------------------------------

function nf_index_to_dayvalue(this, iIndex)   result(rDayValue)

  class (NETCDF4_FILE_T )             :: this
  integer (kind=c_int), intent(in)    :: iIndex
  real (kind=c_double)                :: rDayValue

  if (iIndex < lbound(this%rDateTimeValues, 1) .or. iIndex > ubound(this%rDateTimeValues, 1) ) &
    call die( "Dimension out of bounds", trim(__FILE__), __LINE__)
  
  rDayValue = this%rDateTimeValues(iIndex)

end function nf_index_to_dayvalue

!--------------------------------------------------------------------------------------------------

function nf_dayvalue_to_julian_day(this, rDayValue)   result(rJulianDay)

  class (NETCDF4_FILE_T )             :: this
  real (kind=c_double), intent(in)    :: rDayValue
  real (kind=c_double)                :: rJulianDay

  rJulianDay = real(this%iOriginJD, kind=c_double) &
    + real(this%iOriginHH, kind=c_double) / 24_c_double &
    + real(this%iOriginMM, kind=c_double) / 1440_c_double &
    + real(this%iOriginSS, kind=c_double) / 86400_c_double &
    + rDayValue

end function nf_dayvalue_to_julian_day

!--------------------------------------------------------------------------------------------------

function nf_julian_day_to_index_adj( this, rJulianDay )  result(iStart)

  class (NETCDF4_FILE_T )             :: this
  real (kind=c_double), intent(in)    :: rJulianDay
  integer (kind=c_size_t)             :: iStart

  ! [ LOCALS ]
  integer (kind=c_int)   :: iMinDiff, iDiff
  integer (kind=c_int)   :: iCandidateIndex, iLastCandidate
  integer (kind=c_int)   :: iInitialCandidateIndex
  integer (kind=c_int)   :: iTestIndex
  real (kind=c_double)   :: rTestJD
  integer (kind=c_int)   :: iIndexLower, iIndexUpper, iIndex
  logical (kind=c_bool)  :: lChanged

  iStart = -9999
  iMinDiff = iBIGVAL
  !> First guess at what the appropriate index value should be.
  !> Current JD minus the Origin JD is a good guess.
  iCandidateIndex = nf_julian_day_to_index(this, rJulianDay)

  call assert(iCandidateIndex >=0, "Problem finding the index number of the time " &
    //"variable in NetCDF file "//dquote(this%sFilename), trim(__FILE__), __LINE__)

  iInitialCandidateIndex = iCandidateIndex

  do

    !> calculate the range of *INDEX* values to search over
    iIndexLower = max( lbound(this%rDateTimeValues, 1), iCandidateIndex - 1)
    iIndexUpper = min( ubound(this%rDateTimeValues, 1), iCandidateIndex + 1)

    lChanged = lFALSE

    do iIndex=iIndexLower,iIndexUpper

      rTestJD = nf_dayvalue_to_julian_day(this=this, &
          rDayValue=this%rDateTimeValues(iIndex))

      iTestIndex = aint(rTestJD) - this%iFirstDayJD
      iDiff = abs(iTestIndex - iInitialCandidateIndex)

      if (iDiff < iMinDiff ) then

        iMinDiff = iDiff
        iCandidateIndex = iIndex
        lChanged = lTRUE

      endif

    enddo

    if (.not. lChanged ) exit

  enddo

  if (iMinDiff == 0) iStart = iCandidateIndex

end function nf_julian_day_to_index_adj

!--------------------------------------------------------------------------------------------------

function nf_return_variable_id( this, iVariableIndex)   result(iVariableID)

  class (NETCDF4_FILE_T )             :: this
  integer (kind=c_int), intent(in)    :: iVariableIndex
  integer (kind=c_int)                :: iVariableID

  ! [ LOCALS ]
  type (NETCDF4_VARIABLE_T), pointer :: pNC_VAR

  if ( iVariableIndex < lbound( this%pNC_VAR, 1) .or. iVariableIndex > ubound( this%pNC_VAR, 1) )  &
    call die( "INTERNAL PROGRAMMING ERROR--index out of bounds: "//asCharacter(iVariableIndex), &
      __FILE__, __LINE__ )

  pNC_VAR => this%pNC_VAR(iVariableIndex)

  iVariableID = pNC_VAR%iVariableID

end function nf_return_variable_id

!--------------------------------------------------------------------------------------------------

function nf_return_dimension_id( this, iDimensionIndex)   result(iDimensionID)

  class (NETCDF4_FILE_T )             :: this
  integer (kind=c_int), intent(in)    :: iDimensionIndex
  integer (kind=c_int)                :: iDimensionID

  ! [ LOCALS ]
  type (NETCDF4_DIMENSION_T), pointer :: pNC_DIM

  if ( iDimensionIndex < lbound( this%pNC_DIM, 1) .or. iDimensionIndex > ubound( this%pNC_DIM, 1) )  &
    call die( "INTERNAL PROGRAMMING ERROR--index out of bounds: "//asCharacter(iDimensionIndex), &
      __FILE__, __LINE__ )

  pNC_DIM => this%pNC_DIM(iDimensionIndex)

  iDimensionID = pNC_DIM%iDimensionID

end function nf_return_dimension_id

!--------------------------------------------------------------------------------------------------

function nf_return_variable_index( this, iVariableID)   result(iVariableIndex)

  class (NETCDF4_FILE_T )             :: this
   integer (kind=c_int), intent(in)   :: iVariableID
   integer (kind=c_int)               :: iVariableIndex

   ! [ LOCALS ]
   type (NETCDF4_VARIABLE_T), pointer :: pNC_VAR
   integer (kind=c_int)               :: iIndex
   logical (kind=c_bool)              :: lFound

  lFound = lFALSE

  do iIndex=lbound(this%pNC_VAR, 1), ubound(this%pNC_VAR, 1)

    pNC_VAR => this%pNC_VAR(iIndex)

    if (pNC_VAR%iVariableID == iVariableID) then
      lFound = lTRUE
      exit
    endif

  enddo

  if ( .not. lFound ) &

    call die( "INTERNAL PROGRAMMING ERROR--No matching variable " &
      //"ID found: was looking for Variable ID: "//asCharacter(iVariableID), &
      trim(__FILE__), __LINE__)

  iVariableIndex = iIndex

end function nf_return_variable_index

!--------------------------------------------------------------------------------------------------

function nf_return_attribute_value_string( this, iVariableIndex, sAttributeName)   result(sAttributeValue)

  class (NETCDF4_FILE_T )             :: this
   integer (kind=c_int), intent(in)   :: iVariableIndex
   character (len=*), intent(in )     :: sAttributeName
   character (len=:), allocatable     :: sAttributeValue

   type (NETCDF4_ATTRIBUTE_T), dimension(:), pointer :: pNC_ATT
   integer (kind=c_int) :: iIndex, iIndex2
   logical (kind=c_bool) :: lFound

  ! if no VarIndex, return pointer to GLOBAL attributes struct
  if (iVariableIndex < 0) then

    pNC_ATT => this%pNC_ATT

  else

    if (iVariableIndex < lbound(this%pNC_VAR,1) .or. iVariableIndex > ubound(this%pNC_VAR,1) )  &
      call die("INTERNAL PROGRAMMING ERROR--Index out of bounds referencing this%pNC_VAR" &
      //"~Offending index value: "//asCharacter(iVariableIndex), &
      trim(__FILE__), __LINE__)

    pNC_ATT => this%pNC_VAR(iVariableIndex)%pNC_ATT

  endif

  lFound = lFALSE

  do iIndex=lbound(pNC_ATT,1), ubound(pNC_ATT,1)

    if ( sAttributeName .strequal. pNC_ATT(iIndex)%sAttributeName ) then
      lFound = lTRUE
      exit
    endif

  enddo

  if ( .not. lFound ) &
    call die( "INTERNAL PROGRAMMING ERROR--No matching attribute " &
    //"name found: was looking for attribute with name: "//dquote(sAttributeName), &
    trim(__FILE__), __LINE__)

  sAttributeValue = pNC_ATT(iIndex)%slValues%cat()

end function nf_return_attribute_value_string

!--------------------------------------------------------------------------------------------------

function nf_return_dimension_index( this, iDimensionID)   result(iDimensionIndex)

  class (NETCDF4_FILE_T )             :: this
   integer (kind=c_int), intent(in)   :: iDimensionID
   integer (kind=c_int)               :: iDimensionIndex

   ! [ LOCALS ]
   type (NETCDF4_DIMENSION_T), pointer  :: pNC_DIM
   integer (kind=c_int)                 :: iIndex
   logical (kind=c_bool)                :: lFound

  lFound = lFALSE

  do iIndex=lbound( this%pNC_DIM,1 ), ubound( this%pNC_DIM,1 )

    pNC_DIM => this%pNC_DIM( iIndex )

    if ( pNC_DIM%iDimensionID == iDimensionID ) then
      lFound = lTRUE
      exit
    endif

  enddo

  if ( .not. lFound ) &
    call die( "INTERNAL PROGRAMMING ERROR--No matching dimension " &
    //"ID found: was looking for Dimension ID: "//asCharacter(iDimensionID), &
    trim(__FILE__), __LINE__)

  iDimensionIndex = iIndex

end function nf_return_dimension_index

!--------------------------------------------------------------------------------------------------

function nf_return_dimension_size( this, iDimensionID)   result(iDimensionSize)

  class (NETCDF4_FILE_T )             :: this
   integer (kind=c_int), intent(in)   :: iDimensionID
   integer (kind=c_size_t)            :: iDimensionSize

  ! [ LOCALS ]
  type (NETCDF4_DIMENSION_T), pointer   :: pNC_DIM
  integer (kind=c_int)                  :: iIndex
  logical (kind=c_bool)                 :: lFound

  lFound = lFALSE

  do iIndex=lbound(this%pNC_DIM,1), ubound(this%pNC_DIM,1)

    pNC_DIM => this%pNC_DIM(iIndex)

    if (pNC_DIM%iDimensionID == iDimensionID) then
      lFound = lTRUE
      exit
    endif

  enddo

  if ( .not. lFound ) &
    call die( "INTERNAL PROGRAMMING ERROR--No matching dimension " &
    //"ID found: was looking for Dimension ID: "//asCharacter(iDimensionID), &
    trim(__FILE__), __LINE__)

  iDimensionSize = pNC_DIM%iDimensionSize

end function nf_return_dimension_size

!--------------------------------------------------------------------------------------------------

subroutine nf_define_iteration_bounds(this)

  class (NETCDF4_FILE_T )             :: this

  if (this%lFlipVertical) then
    this%iRowIter(NC_FIRST) = this%iNY
    this%iRowIter(NC_LAST) = 1
    this%iRowIter(NC_BY) = -1
  else
    this%iRowIter(NC_FIRST) = 1
    this%iRowIter(NC_LAST) = this%iNY
    this%iRowIter(NC_BY) = 1
  endif

  if (this%lFlipHorizontal) then
    this%iColIter(NC_FIRST) = this%iNX
    this%iColIter(NC_LAST) = 1
    this%iColIter(NC_BY) = -1
  else
    this%iColIter(NC_FIRST) = 1
    this%iColIter(NC_LAST) = this%iNX
    this%iColIter(NC_BY) = 1
  endif

end subroutine nf_define_iteration_bounds

!--------------------------------------------------------------------------------------------------

subroutine nf_define_start_count_stride(this, pNC_VAR)

  class (NETCDF4_FILE_T )             :: this
  type (NETCDF4_VARIABLE_T), pointer  :: pNC_VAR

  ! [ LOCALS ]
  integer (kind=c_int) :: iIndex

  !> need to subtract 1 from the start index: we're using the
  !> NetCDF C API, in which index values are relative to zero
  this%pNC_VAR_X%iStart = minval(this%iColBounds) - 1
  this%iNX = maxval(this%iColBounds) - minval(this%iColBounds) + 1
  this%pNC_VAR_X%iCount = this%iNX
!        this%iCount(iIndex) = maxval(this%iColBounds) - minval(this%iColBounds)
  this%pNC_VAR_X%iStride = 1_c_size_t

  !> note: this assumes that the row numbers increase from top to bottom,
  !>       while the Y coordinates decrease top to bottom

  this%pNC_VAR_Y%iStart = minval(this%iRowBounds) - 1
  this%iNY = maxval(this%iRowBounds) - minval(this%iRowBounds) + 1
  this%pNC_VAR_Y%iCount = this%iNY
  this%pNC_VAR_Y%iStride = 1_c_size_t

  !>
  !> count must be set to the number of values! maxval minus minval results
  !> in a diagonal pattern in the input as we read in the incorrect number
  !> of results
!        this%iCount(iIndex) = maxval(this%iRowBounds) - minval(this%iRowBounds)

  this%pNC_VAR_TIME%iStart = 0_c_size_t
  this%pNC_VAR_TIME%iCount = 1_c_size_t
  this%pNC_VAR_TIME%iStride = 1_c_size_t

  pNC_VAR%iStart = 1_c_size_t
  pNC_VAR%iCount = this%iNY * this%iNX
  pNC_VAR%iStride = 1_c_size_t


end subroutine nf_define_start_count_stride

!--------------------------------------------------------------------------------------------------

subroutine nf_calculate_native_coord_bounds(this)

  class (NETCDF4_FILE_T )             :: this

  ! [ LOCALS ]
  real (kind=c_double) :: rXmin, rXmax
  real (kind=c_double) :: rYmin, rYmax

  !> find the (x,y) associated with the column and row number bounds
  rXmin = minval(this%rX_Coords(this%iColBounds(NC_LEFT):this%iColBounds(NC_RIGHT)) )
  rXmax = maxval(this%rX_Coords(this%iColBounds(NC_LEFT):this%iColBounds(NC_RIGHT)) )
  rYmin = minval(this%rY_Coords(this%iRowBounds(NC_TOP):this%iRowBounds(NC_BOTTOM)) )
  rYmax = maxval(this%rY_Coords(this%iRowBounds(NC_TOP):this%iRowBounds(NC_BOTTOM)) )

  this%rX(NC_LEFT) = rXmin - this%rGridCellSizeX * 0.5_c_double
  this%rX(NC_RIGHT) = rXmax + this%rGridCellSizeX * 0.5_c_double
  this%rY(NC_TOP) = rYmax + this%rGridCellSizeY * 0.5_c_double
  this%rY(NC_BOTTOM) = rYmin - this%rGridCellSizeY * 0.5_c_double

!#ifdef DEBUG_PRINT
  print *, "Filename: ", this%sFilename
  print *, "Grid cell size (X): ", this%rGridCellSizeX
  print *, "Grid cell size (Y): ", this%rGridCellSizeY

  print *, "Bounds of data subset area, in native coordinates"
  print *, "X (left): ", this%rX(NC_LEFT)
  print *, "X (right): ", this%rX(NC_RIGHT)
  print *, "Y (top): ", this%rY(NC_TOP)
  print *, "Y (bottom): ", this%rY(NC_BOTTOM)
!#endif

end subroutine nf_calculate_native_coord_bounds

!--------------------------------------------------------------------------------------------------

!> Get time values from NetCDF file.
!!
!! This routine obtains a vector of all time values from the NetCDF file associated with NCID.
!!
!! @param[inout] this Object of class NETCDF4_FILE_T

subroutine nf_get_time_values(this)

  class (NETCDF4_FILE_T )             :: this

  ! [ LOCALS ]
  integer (kind=c_int) :: iVariableIndex_time
  integer (kind=c_int) :: iLowerBound, iUpperBound
  integer (kind=c_int) :: iStat

  iStat = 0

  if (allocated(this%rDateTimeValues) ) deallocate(this%rDateTimeValues, stat=iStat)
  call assert(iStat==0, "Failed to deallocate memory for time values", &
    trim(__FILE__), __LINE__)

  allocate( this%rDateTimeValues(0 : this%pNC_DIM_TIME%iDimensionSize-1 ), stat=iStat )
  call assert(iStat==0, "Failed to allocate memory for time values", &
    trim(__FILE__), __LINE__)

  !> @todo allow time to be read in as float, short, or int as well

  call nf_get_variable_vector_double(this=this,    &
       iVariableID=this%pNC_VAR_TIME%iVariableID,  &
       iNC_Start=0_c_size_t,                       &
       iNC_Count=this%pNC_DIM_TIME%iDimensionSize,    &
       iNC_Stride=1_c_size_t,                      &
       dpNC_Vars=this%rDateTimeValues)

end subroutine nf_get_time_values

!--------------------------------------------------------------------------------------------------

!> Read X and Y variable vectors from the NetCDF file.
!!
!! Only 1-D arrays (vectors) of X and Y variables are supported at this time.
!!
!! @param[inout] this Object of class NETCDF4_FILE_T.
!!
!! @pre Before calling this routine, pointers to the X and Y variables must be targeted at
!!      valid variables.

subroutine nf_get_x_and_y(this)

  class (NETCDF4_FILE_T )         :: this

  ! [ LOCALS ]
  integer (kind=c_int) :: iLowerBound, iUpperBound
  integer (kind=c_int) :: iStat

  call assert( associated( this%pNC_VAR_X ), "INTERNAL PROGRAMMING ERROR--attempted use of null pointer", &
    __FILE__, __LINE__ )

  call assert( associated( this%pNC_VAR_Y ), "INTERNAL PROGRAMMING ERROR--attempted use of null pointer", &
    __FILE__, __LINE__ )

  call assert( this%pNC_VAR_X%iNumberOfDimensions == 1, &
    "Dimensions other than one for the x-coordinate variable are currently unsupported.", &
    __FILE__, __LINE__ )

  call assert( this%pNC_VAR_Y%iNumberOfDimensions == 1, &
    "Dimensions other than one for the y-coordinate variable are currently unsupported.", &
    __FILE__, __LINE__ )

  allocate( this%rX_Coords( this%pNC_DIM_X%iDimensionSize ), stat=iStat )
  call assert(iStat==0, "Failed to allocate memory for X-coordinate values", &
    __FILE__, __LINE__ )

  allocate (this%rY_Coords( this%pNC_DIM_Y%iDimensionSize  ), stat=iStat )
  call assert(iStat==0, "Failed to allocate memory for Y-coordinate values", &
    __FILE__, __LINE__ )

  call nf_get_variable_vector_double(this=this, &
       iVariableID=this%pNC_VAR_X%iVariableID, &
       iNC_Start=0_c_size_t, &
       iNC_Count=this%pNC_DIM_X%iDimensionSize, &
       iNC_Stride=1_c_size_t, &
       dpNC_Vars=this%rX_Coords)

  call nf_get_variable_vector_double(this=this, &
       iVariableID=this%pNC_VAR_Y%iVariableID, &
       iNC_Start=0_c_size_t, &
       iNC_Count=this%pNC_DIM_Y%iDimensionSize, &
       iNC_Stride=1_c_size_t, &
       dpNC_Vars=this%rY_Coords)

  iLowerBound = lbound(this%rX_Coords, 1)
  iUpperBound = ubound(this%rX_Coords, 1)

  if (this%rX_Coords(iUpperBound) > this%rX_Coords(iLowerBound) ) then
    this%lX_IncreasesWithIndex = lTRUE
  else
    this%lX_IncreasesWithIndex = lFALSE
  endif

  iLowerBound = lbound(this%rY_Coords, 1)
  iUpperBound = ubound(this%rY_Coords, 1)

  if (this%rY_Coords(iUpperBound) > this%rY_Coords(iLowerBound) ) then
    this%lY_IncreasesWithIndex = lTRUE
  else
    this%lY_IncreasesWithIndex = lFALSE
  endif

  call assert(this%pNC_DIM_X%iDimensionSize > 2, "INTERNAL PROGRAMMING ERROR--" &
    //"NetCDF X dimension size must be greater than 2.", trim(__FILE__), __LINE__)

  call assert(this%pNC_DIM_Y%iDimensionSize > 2, "INTERNAL PROGRAMMING ERROR--" &
    //"NetCDF Y dimension size must be greater than 2.", trim(__FILE__), __LINE__)

  this%rGridCellSizeX = ( maxval(this%rX_Coords) &
                                - minval(this%rX_Coords) ) &
                                / real (this%pNC_DIM_X%iDimensionSize - 1, kind=c_double)

  this%rGridCellSizeY = ( maxval(this%rY_Coords) &
                                - minval(this%rY_Coords) ) &
                                / real (this%pNC_DIM_Y%iDimensionSize - 1, kind=c_double)

end subroutine nf_get_x_and_y

!--------------------------------------------------------------------------------------------------
!> Open a NetCDF file in READONLY mode and determine the format.
!!
!! This is a lower-level subroutine.
!!
!! @param[inout] this Object of class NETCDF4_FILE_T.
!! @param[in] sFilename URL, relative, or absolute path and filename of NetCDF file.

subroutine nf_open_file_readonly( this, sFilename )

  class (NETCDF4_FILE_T )             :: this
  character (len=*) :: sFilename

  ! [ LOCALS ]
  logical (kind=c_bool) :: lFileOpen

  call LOGS%write("Attempting to open READONLY NetCDF file: " &
    //dquote(sFilename))

  call nf_trap( nc_open(trim(sFilename)//c_null_char, &
                NC_READONLY, this%iNCID), __FILE__, __LINE__ )

  call nf_trap( nc_inq_format(ncid=this%iNCID, formatp=this%iFileFormat), &
               __FILE__, __LINE__)

  call LOGS%write("   Succeeded.  ncid: "//asCharacter( this%iNCID ) &
         //"  format: "//trim( NETCDF_FORMAT_STRING( this%iFileFormat ) ) )

  this%sFilename = sFilename

end subroutine nf_open_file_readonly

!--------------------------------------------------------------------------------------------------

! subroutine netcdf_open_file_readonly( this, sFilename )

!   class (NETCDF4_FILE_T )             :: this
!   character (len=*), intent(in)       :: sFilename


!   call nf_open_file(this=this, sFilename=sFilename)

!   !> Similarly, the structure of the file may be slightly different from the
!   !> previous file
!   call nf_populate_dimension_struct( this )
!   call nf_populate_variable_struct( this )

!   !> CANNOT ASSUME THAT THIS WILL REMAIN CONSTANT ACROSS FILES FROM THE
!   !> SAME PROVIDER!! MUST UPDATE TO ENSURE THAT THE INDICES ARE STILL RELEVANT
!   call nf_get_variable_id_and_type( this )

!   ! OK. We only want to attempt to call functions that
!   ! process the time variable if a time variable actually exists!!
!   if ( associated( this%pNC_VAR_TIME) ) then

!     this%dpFirstAndLastTimeValues = nf_get_first_and_last(this=this, &
!         iVariableIndex=this%pNC_VAR_TIME%iVariableID )

!     !> retrieve the origin for the time units associated with this file
!     call nf_get_time_units( this=this )

!     !> retrieve the time value specific to this file
!     call nf_get_time_values( this )

!     call nf_calculate_time_range( this )

!   endif

!   !> establish scale_factor and add_offset values, if present
!   call nf_get_scale_and_offset( this=this )

! end subroutine netcdf_open_file_readonly

!--------------------------------------------------------------------------------------------------

subroutine nf_trap( iResultCode, sFilename, iLineNumber )

  integer (kind=c_int) :: iResultCode
  character (len=*), optional :: sFilename
  integer (kind=c_int), optional :: iLineNumber

  ! [ LOCALS ]
  type(c_ptr) :: cpResult
  character (len=256) :: sTextString
  character (len=256) :: sFile
  integer (kind=c_int) :: iLine

  if (iResultCode /= 0) then

    if (present(sFilename)) then
      sFile = trim(sFilename)
    else
      sFile = trim(__FILE__)
    endif

    if (present(iLinenumber)) then
      iLine = iLinenumber
    else
      iLine = __LINE__
    endif

    cpResult = nc_strerror(iResultCode)
    sTextString = char_ptr_to_fortran_string( cpResult )

    call LOGS%write("NetCDF ERROR: "//dquote( sTextString  )//" | error code was: " &
      //asCharacter(iResultCode) )

    call assert(lFALSE, "SWB is stopping due to a problem reading or writing" &
      //" a NetCDF file", trim(sFile), iLine)

  endif

end subroutine nf_trap

!--------------------------------------------------------------------------------------------------

subroutine nf_close_file( this)

  class (NETCDF4_FILE_T )             :: this

  call LOGS%write("Closing NetCDF file with name: "//dquote(this%sFilename))
  call nf_trap( nc_close(this%iNCID), __FILE__, __LINE__ )

!  call nf_deallocate_data_struct( this=this )

end subroutine nf_close_file

!--------------------------------------------------------------------------------------------------

subroutine nf_deallocate_data_struct( this )

  class (NETCDF4_FILE_T )             :: this

  ! [ LOCALS ]
  type (NETCDF4_VARIABLE_T), pointer :: pNC_VAR
  integer (kind=c_int) :: iIndex

  do iIndex=0, ubound(this%pNC_VAR,1)

    pNC_VAR => this%pNC_VAR(iIndex)

    if ( pNC_VAR%iNumberOfAttributes == 0 ) cycle

    if (associated( pNC_VAR%pNC_ATT ))  deallocate( pNC_VAR%pNC_ATT )
    pNC_VAR%pNC_ATT => null()

  enddo

  if (associated( this%pNC_VAR ))  deallocate( this%pNC_VAR )
  if (associated( this%pNC_ATT ))  deallocate( this%pNC_ATT )
  if (associated( this%pNC_DIM ))  deallocate( this%pNC_DIM )

  this%pNC_VAR => null()
  this%pNC_ATT => null()
  this%pNC_DIM => null()

end subroutine nf_deallocate_data_struct

!--------------------------------------------------------------------------------------------------

subroutine nf_nullify_data_struct( this )

  class (NETCDF4_FILE_T )             :: this

  ! [ LOCALS ]

  this%pNC_VAR => null()
  this%pNC_ATT => null()
  this%pNC_DIM => null()

end subroutine nf_nullify_data_struct

!--------------------------------------------------------------------------------------------------

!> Iterate over all dimension elements stored in a READONLY NetCDF file.
!!
!! The number of dimensions, the dimension names, and the dimension sizes
!! are determined by querying the NetCDF file and are stored in the NETCDF4_DIMENSION_T structure.
!!
!! @param[inout] this Object of class NETCDF4_FILE_T.
!! 
!! @note This subroutine is designed to be used when dealing with a READONLY file. 

subroutine nf_get_dimension_struct( this )

  class (NETCDF4_FILE_T) :: this

  ! [ LOCALS ]
  integer (kind=c_int) :: iStat
  integer (kind=c_int) :: iIndex
  character (len=256)  :: sDimensionName
  integer (kind=c_int) :: iNumberOfDimensions

  ! how many dimendions are defined in our NetCDF file?
  call nf_trap( nc_inq_ndims(ncid=this%iNCID, ndimsp=iNumberOfDimensions), &
                __FILE__, __LINE__ )

  if (associated(this%pNC_DIM) ) deallocate(this%pNC_DIM, stat=iStat)
  call assert(iStat == 0, "Could not deallocate memory for NC_DIM member in NC_FILE defined type", &
    trim(__FILE__), __LINE__)

  allocate(this%pNC_DIM( 0 : iNumberOfDimensions-1), stat=iStat )
  call assert(iStat == 0, "Could not allocate memory for NC_DIM member in NC_FILE defined type", &
    trim(__FILE__), __LINE__)

  ! NetCDF 3 function
  call nf_trap( nc_inq_unlimdim(ncid=this%iNCID, unlimdimidp=this%iNC3_UnlimitedDimensionNumber), &
               __FILE__, __LINE__ )

  do iIndex = 0, iNumberOfDimensions-1

    call nf_trap(nc_inq_dim(ncid=this%iNCID, dimid=iIndex, &
      name=sDimensionName, &
      lenp=this%pNC_DIM(iIndex)%iDimensionSize), __FILE__, __LINE__ )

    this%pNC_DIM(iIndex)%iDimensionID = iIndex
    this%pNC_DIM(iIndex)%sDimensionName = c_to_fortran_string(sDimensionName)

  enddo

end subroutine nf_get_dimension_struct

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_struct( this )

  class (NETCDF4_FILE_T) :: this

  ! [ LOCALS ]
  type (NETCDF4_ATTRIBUTE_T), pointer     :: pNC_ATT
  type (NETCDF4_VARIABLE_T), pointer      :: pNC_VAR
  integer (kind=c_int)                    :: iStat
  integer (kind=c_int)                    :: iIndex, iIndex2 , iIndex3
  character (len=256)                     :: sVariableName
  character (len=256)                     :: sAttributeName
  character (len=512)                     :: sAttributeValue
  integer (kind=c_int), dimension(0:25)   :: iAttValue
  integer (kind=c_short), dimension(0:25) :: i2AttValue
  real (kind=c_double), dimension(0:25)   :: cdAttValue
  integer (kind=c_int)                    :: iNumberOfVariables
  integer (kind=c_int)                    :: iNumberOfAttributes

  iStat = 0

  ! find out how many VARIABLES the file contains
  call nf_trap( nc_inq_nvars(ncid=this%iNCID, nvarsp=iNumberOfVariables), &
       __FILE__, __LINE__ )

  ! make space in the this%pNC_VAR data structure for all variables in file
  if (associated(this%pNC_VAR) ) deallocate(this%pNC_VAR, stat=iStat)
  call assert(iStat == 0, "Could not deallocate memory for NC_VAR member in NC_FILE defined type", &
    trim(__FILE__), __LINE__)

  allocate(this%pNC_VAR( 0 : iNumberOfVariables-1), stat=iStat )
  call assert(iStat == 0, "Could not allocate memory for NC_VAR member in NC_FILE defined type", &
    trim(__FILE__), __LINE__)

  ! iterate over all variables in file, collecting basic information about each
  do iIndex = lbound( this%pNC_VAR,1), ubound( this%pNC_VAR,1)

    pNC_VAR => this%pNC_VAR(iIndex)

    ! make actual call via C API to obtain variable information
    call nf_trap(nc_inq_var(ncid=this%iNCID, &
        varid=iIndex, &
        name=sVariableName, &
        xtypep=pNC_VAR%iVariableType, &
        ndimsp=pNC_VAR%iNumberOfDimensions, &
        dimidsp=pNC_VAR%iDimensionID, &
        nattsp=pNC_VAR%iNumberOfAttributes ), __FILE__, __LINE__ )

    pNC_VAR%iVariableID = iIndex
    pNC_VAR%sVariableName = c_to_fortran_string(sVariableName)

    ! each variable may have many attributes. if so, find them.
    if( pNC_VAR%iNumberOfAttributes > 0 ) then

      if (associated(pNC_VAR%pNC_ATT) ) deallocate(pNC_VAR%pNC_ATT, stat=iStat)
      call assert(iStat == 0, "Could not deallocate memory for NC_ATT member within NC_VAR in NC_FILE defined type", &
        trim(__FILE__), __LINE__)

      allocate( pNC_VAR%pNC_ATT( 0:pNC_VAR%iNumberOfAttributes - 1 ), stat = iStat)
      call assert(iStat == 0, "Could not allocate memory for NC_ATT member within NC_VAR in NC_FILE defined type", &
        trim(__FILE__), __LINE__)

      do iIndex2=lbound(pNC_VAR%pNC_ATT,1), ubound(pNC_VAR%pNC_ATT,1)

        pNC_ATT => pNC_VAR%pNC_ATT(iIndex2)

        call nf_populate_attribute_struct( this=this, pNC_ATT=pNC_ATT, &
          iVariableID=iIndex, iAttNum=iIndex2 )

      enddo

    endif

  enddo

  ! now find out how many GLOBAL attributes the file contains
  call nf_trap( nc_inq_natts(ncid=this%iNCID, ngattsp=iNumberOfAttributes), &
       __FILE__, __LINE__ )

  if ( associated(this%pNC_ATT) )  deallocate(this%pNC_ATT, stat=iStat)
  call assert(iStat == 0, "Could not deallocate memory for NC_ATT member within NC_FILE defined type", &
    trim(__FILE__), __LINE__)

  allocate(this%pNC_ATT(0:iNumberOfAttributes - 1), stat=iStat )
  call assert(iStat == 0, "Could not allocate memory for NC_ATT member within NC_FILE defined type", &
    trim(__FILE__), __LINE__)

  ! iterate over the GLOBAL attributes, recording basic information about each
  do iIndex=lbound(this%pNC_ATT,1), ubound(this%pNC_ATT,1)
    pNC_ATT => this%pNC_ATT(iIndex)

    call nf_populate_attribute_struct( this=this, pNC_ATT=pNC_ATT, &
      iVariableID=NC_GLOBAL, iAttNum=iIndex )

  enddo

end subroutine nf_get_variable_struct

!--------------------------------------------------------------------------------------------------

!> Retrieve attributes from NetCDF4 file.
!!
!! For a given variable ID and attribute, make inquiry of NetCDF file and populate the
!! data structure.
!!
!! @param[inout] this Object of class NETCDF4_FILE_T.
!! @param[inout] pNC_ATT Pointer to a NETCDF_ATTRIBUTE_T data structure.
!! @param[in] iVariableID Integer value associated with a particular variable stored in the NetCDF file.
!! @param[in] iAttNum Attribute index within pNC_ATT data structure.

subroutine nf_get_attribute_struct( this, pNC_ATT, iVariableID, iAttNum )

  class (NETCDF4_FILE_T), intent(inout) :: this
  type (NETCDF4_ATTRIBUTE_T), pointer :: pNC_ATT
  integer (kind=c_int) :: iVariableID
  integer (kind=c_int) :: iAttNum

  ![ LOCALS ]
  integer (kind=c_int)              :: iStat
  character (len=256)               :: sVariableName
  character (len=256)               :: sAttributeName
  character (len=256), allocatable  :: sAttributeValues(:)
  integer (kind=c_int)              :: iIndex
  integer (kind=c_int)              :: iLength

  ! obtain attribute NAME
  call nf_trap( nc_inq_attname(ncid=this%iNCID, &
    varid=iVariableID, &
    attnum=iAttNum, &
    name=sAttributeName), __FILE__, __LINE__ )

  pNC_ATT%sAttributeName = c_to_fortran_string(sAttributeName)

  ! obtain attribute SIZE and TYPE
  call nf_trap( nc_inq_att(ncid=this%iNCID, &
    varid=iVariableID, &
    name=sAttributeName, &
    xtypep=pNC_ATT%iAttributeType, &
    lenp=pNC_ATT%iAttributeSize), __FILE__, __LINE__ )

  iLength = pNC_ATT%iAttributeSize

  ! make room for local attribute VALUE strings
  iStat = 0
  allocate( sAttributeValues(0:iLength-1), stat=iStat )
  call assert(iStat==0, "INTERNAL PROGRAMMING ERROR--problem allocating memory", &
    trim(__FILE__), __LINE__)
  sAttributeValues = ""

  ! now get the actual attribute VALUE from the NetCDF4 file.
  select case(pNC_ATT%iAttributeType)

    case (NC_CHAR)

      call nf_trap( nc_get_att_text(ncid=this%iNCID, &
        varid=iVariableID, &
        name=sAttributeName, &
        ip=sAttributeValues ), __FILE__, __LINE__ )

      do iIndex=0, ubound(sAttributeValues,1)
        sAttributeValues(iIndex) = c_to_fortran_string(sAttributeValues(iIndex) )
        call pNC_ATT%slValues%append( sAttributeValues(iIndex) )
      enddo

    case (NC_SHORT)

      allocate(pNC_ATT%i2Values(0:iLength-1), stat=iStat )
      call assert(iStat==0, "INTERNAL PROGRAMMING ERROR--problem allocating memory", &
        trim(__FILE__), __LINE__)


      call nf_trap( nc_get_att_short(ncid=this%iNCID, &
        varid=iVariableID, &
        name=sAttributeName, &
        ip=pNC_ATT%i2Values), __FILE__, __LINE__ )

      do iIndex=0, ubound(pNC_ATT%i2Values,1)
        call pNC_ATT%slValues%append( int(pNC_ATT%i2Values(iIndex), kind=c_int) )
      enddo


    case (NC_INT)

      allocate(pNC_ATT%iValues(0:iLength-1), stat=iStat )
      call assert(iStat==0, "INTERNAL PROGRAMMING ERROR--problem allocating memory", &
        trim(__FILE__), __LINE__)

      call nf_trap( nc_get_att_int(ncid=this%iNCID, &
        varid=iVariableID, &
        name=sAttributeName, &
        ip=pNC_ATT%iValues), __FILE__, __LINE__ )

      do iIndex=0, ubound(pNC_ATT%iValues,1)
        call pNC_ATT%slValues%append( pNC_ATT%iValues(iIndex) )
      enddo

    case (NC_FLOAT)

      allocate(pNC_ATT%rValues(0:iLength-1), stat=iStat )
      call assert(iStat==0, "INTERNAL PROGRAMMING ERROR--problem allocating memory", &
        trim(__FILE__), __LINE__)


      call nf_trap( nc_get_att_float(ncid=this%iNCID, &
        varid=iVariableID, &
        name=sAttributeName, &
        ip=pNC_ATT%rValues), __FILE__, __LINE__ )

      do iIndex=0, ubound(pNC_ATT%rValues,1)
        call pNC_ATT%slValues%append( pNC_ATT%rValues(iIndex) )
      enddo

    case (NC_DOUBLE)

      allocate(pNC_ATT%dpValues(0:iLength-1), stat=iStat )
      call assert(iStat==0, "INTERNAL PROGRAMMING ERROR--problem allocating memory", &
        trim(__FILE__), __LINE__)

      call nf_trap( nc_get_att_double(ncid=this%iNCID, &
        varid=iVariableID, &
        name=sAttributeName, &
        ip=pNC_ATT%dpValues), __FILE__, __LINE__ )

      do iIndex=0, ubound(pNC_ATT%dpValues,1)
        call pNC_ATT%slValues%append( pNC_ATT%dpValues(iIndex) )
      enddo

    case default

  end select

end subroutine nf_get_attribute_struct

!--------------------------------------------------------------------------------------------------

function nf_update_time_starting_index(this, iJulianDay)  result(lDateTimeFound)

  class (NETCDF4_FILE_T)            :: this
  integer (kind=c_int), intent(in) :: iJulianDay
  logical (kind=c_bool)            :: lDateTimeFound

  ! [ LOCALS ]
  real (kind=c_double) :: rNC_DateTime

  call assert( associated( this%pNC_VAR_TIME ), &
    "INTERNAL PROGRAMMING ERROR--attempted use of null pointer", __FILE__, __LINE__ )

  this%pNC_VAR_TIME%iStart = nf_julian_day_to_index_adj( this=this, &
                                     rJulianDay=real(iJulianDay, kind=c_double ) )

  if ( this%pNC_VAR_TIME%iStart < 0) then
    this%pNC_VAR_TIME%iStart = 0
    lDateTimeFound = lFALSE
  else
    lDateTimeFound = lTRUE
  endif

end function nf_update_time_starting_index

!--------------------------------------------------------------------------------------------------

! subroutine netcdf_get_variable_slice(this, pNC_VAR, rValues, iValues)

!   class (NETCDF4_FILE_T)                          :: this
!   type (NETCDF4_VARIABLE_T), pointer             :: pNC_VAR
!   real (kind=c_float), dimension(:,:), optional  :: rValues
!   integer (kind=c_int), dimension(:,:), optional :: iValues

!   call assert( associated(pNC_VAR), & 
!     "INTERNAL PROGRAMMING ERROR--attempted use of null pointer", __FILE__, __LINE__ )

!   !> @todo expand this to cover more variable types 

!   select case ( pNC_VAR%iVariableType )

!     case ( NC_SHORT)

!       if (present(rValues) ) call nf_get_variable_slice_short(this, rValues)

!     case ( NC_INT )

!       if (present(rValues) ) call nf_get_variable_slice_int(this, rValues)

!     case ( NC_FLOAT )

!       if (present(rValues) ) call nf_get_variable_slice_float(this, rValues)

!     case default
  
!       call warn("Failed to find a method to retrieve data of the given type.", __FILE__, __LINE__)  

!   end select

! end subroutine netcdf_get_variable_slice

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_slice_short(this, pNC_VAR, rValues)

  class (NETCDF4_FILE_T)                          :: this
  type (NETCDF4_VARIABLE_T), pointer             :: pNC_VAR
  real (kind=c_float), dimension(:,:)            :: rValues

  ! [ LOCALS ]
  integer (kind=c_short), dimension(size(rValues,2) * size(rValues,1)) :: iTemp
  integer (kind=c_int) :: iStat
  integer (kind=c_int) :: iRow, iCol, iIndex
  integer (kind=c_int) :: iFromRow, iToRow, iByRow
  integer (kind=c_int) :: iFromCol, iToCol, iByCol

  iFromRow = this%iRowIter(NC_FIRST)
  iToRow = this%iRowIter(NC_LAST)
  iByRow = this%iRowIter(NC_BY)

  iFromCol = this%iColIter(NC_FIRST)
  iToCol = this%iColIter(NC_LAST)
  iByCol = this%iColIter(NC_BY)

  associate ( start_time => this%pNC_VAR_TIME%iStart, start_x => this%pNC_VAR_X%iStart,     &
              start_y => this%pNC_VAR_Y%iStart, start_val => pNC_VAR%iStart,     &
              count_time => this%pNC_VAR_TIME%iCount, count_x => this%pNC_VAR_X%iCount,     &
              count_y => this%pNC_VAR_Y%iCount, count_val => pNC_VAR%iCount,           &
              stride_time => this%pNC_VAR_TIME%iStride, stride_x => this%pNC_VAR_X%iStride, &
              stride_y => this%pNC_VAR_Y%iStride, stride_val => pNC_VAR%iStride,       &
              varid => pNC_VAR%iVariableID                                       )

    select case (this%sVariableOrder)

      case ("txy")    ! time, col, row

        call nf_get_variable_array_as_vector_short(this=this,  &
          iVariableID=varid,                                         &
          iNC_Start=[ start_time,   start_x,  start_y  ],          &
          iNC_Count=[ count_time,   count_x,  count_y  ],          &
          iNC_Stride=[ stride_time, stride_x, stride_y ],          &
          iNC_Vars=iTemp)

          iIndex = 0
          do iCol=iFromCol, iToCol, iByCol
            do iRow=iFromRow, iToRow, iByRow
              iIndex = iIndex + 1
              rValues(iCol,iRow) = real(iTemp(iIndex), kind=c_float)
            enddo
          enddo

      case ("tyx")    ! time, row, col


        call nf_get_variable_array_as_vector_short(this=this,  &
          iVariableID=varid,                                         &
          iNC_Start=[ start_time,   start_y,  start_x  ],          &
          iNC_Count=[ count_time,   count_y,  count_x  ],          &
          iNC_Stride=[ stride_time, stride_y, stride_x ],          &
          iNC_Vars=iTemp)

          iIndex = 0
          do iRow=iFromRow, iToRow, iByRow
            do iCol=iFromCol, iToCol, iByCol
              iIndex = iIndex + 1
              rValues(iCol,iRow) = real(iTemp(iIndex), kind=c_float)
            enddo
          enddo

      case default    

        call warn( "INTERNAL PROGRAMMING ERROR: Unhandled select case. Program will probably fail.", &
          __FILE__, __LINE__ )

    end select

  end associate

end subroutine nf_get_variable_slice_short

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_slice_int(this, pNC_VAR, rValues)

  class (NETCDF4_FILE_T)                          :: this
  type (NETCDF4_VARIABLE_T), pointer             :: pNC_VAR
  real (kind=c_float), dimension(:,:)            :: rValues

  ! [ LOCALS ]
  integer (kind=c_int), dimension(size(rValues,2) * size(rValues,1)) :: iTemp
  integer (kind=c_int) :: iStat
  integer (kind=c_int) :: iRow, iCol, iIndex
  integer (kind=c_int) :: iFromRow, iToRow, iByRow
  integer (kind=c_int) :: iFromCol, iToCol, iByCol

  iFromRow = this%iRowIter(NC_FIRST)
  iToRow = this%iRowIter(NC_LAST)
  iByRow = this%iRowIter(NC_BY)

  iFromCol = this%iColIter(NC_FIRST)
  iToCol = this%iColIter(NC_LAST)
  iByCol = this%iColIter(NC_BY)

  associate ( start_time => this%pNC_VAR_TIME%iStart, start_x => this%pNC_VAR_X%iStart,     &
              start_y => this%pNC_VAR_Y%iStart, start_val => pNC_VAR%iStart,                &
              count_time => this%pNC_VAR_TIME%iCount, count_x => this%pNC_VAR_X%iCount,     &
              count_y => this%pNC_VAR_Y%iCount, count_val => pNC_VAR%iCount,                &
              stride_time => this%pNC_VAR_TIME%iStride, stride_x => this%pNC_VAR_X%iStride, &
              stride_y => this%pNC_VAR_Y%iStride, stride_val => pNC_VAR%iStride,            &
              varid => pNC_VAR%iVariableID                                       )

    select case (this%sVariableOrder)

      case ("txy")    ! time, col, row

        call nf_get_variable_array_as_vector_int(this=this, &
          iVariableID=varid,                                         &
          iNC_Start=[ start_time,   start_x,  start_y  ],          &
          iNC_Count=[ count_time,   count_x,  count_y  ],          &
          iNC_Stride=[ stride_time, stride_x, stride_y ],          &
          iNC_Vars=iTemp)

          iIndex = 0
          do iCol=iFromCol, iToCol, iByCol
            do iRow=iFromRow, iToRow, iByRow
              iIndex = iIndex + 1
              rValues(iCol,iRow) = real(iTemp(iIndex), kind=c_float)
            enddo
          enddo

      case ("tyx")    ! time, row, col

        call nf_get_variable_array_as_vector_int(this=this, &
          iVariableID=varid,                                         &
          iNC_Start=[ start_time,   start_y,  start_x  ],          &
          iNC_Count=[ count_time,   count_y,  count_x  ],          &
          iNC_Stride=[ stride_time, stride_y, stride_x ],          &
          iNC_Vars=iTemp)

          iIndex = 0
          do iRow=iFromRow, iToRow, iByRow
            do iCol=iFromCol, iToCol, iByCol
              iIndex = iIndex + 1
              rValues(iCol,iRow) = real(iTemp(iIndex), kind=c_float)
            enddo
          enddo

      case default    

        call warn("INTERNAL PROGRAMMING ERROR: Unhandled select case. Program will probably fail.", &
         __FILE__, __LINE__)

    end select

  end associate

end subroutine nf_get_variable_slice_int

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_slice_float(this, pNC_VAR, rValues)

  class (NETCDF4_FILE_T)                       :: this
  type (NETCDF4_VARIABLE_T), pointer          :: pNC_VAR    
  real (kind=c_float), dimension(:,:)         :: rValues

  ! [ LOCALS ]
  real (kind=c_float), dimension(size(rValues,2) * size(rValues,1)) :: rTemp
  integer (kind=c_int) :: iStat
  integer (kind=c_int) :: iRow, iCol, iIndex
  integer (kind=c_int) :: iFromRow, iToRow, iByRow
  integer (kind=c_int) :: iFromCol, iToCol, iByCol

  iFromRow = this%iRowIter(NC_FIRST)
  iToRow = this%iRowIter(NC_LAST)
  iByRow = this%iRowIter(NC_BY)

  iFromCol = this%iColIter(NC_FIRST)
  iToCol = this%iColIter(NC_LAST)
  iByCol = this%iColIter(NC_BY)

  associate ( start_time => this%pNC_VAR_TIME%iStart, start_x => this%pNC_VAR_X%iStart,     &
              start_y => this%pNC_VAR_Y%iStart, start_val => pNC_VAR%iStart,     &
              count_time => this%pNC_VAR_TIME%iCount, count_x => this%pNC_VAR_X%iCount,     &
              count_y => this%pNC_VAR_Y%iCount, count_val => pNC_VAR%iCount,           &
              stride_time => this%pNC_VAR_TIME%iStride, stride_x => this%pNC_VAR_X%iStride, &
              stride_y => this%pNC_VAR_Y%iStride, stride_val => pNC_VAR%iStride,       &
              varid => pNC_VAR%iVariableID                                       )

    select case (this%sVariableOrder)

      case ("txy")    ! time, col, row

        call nf_get_variable_array_as_vector_float(this=this, &
          iVariableID=varid,                                         &
          iNC_Start=[ start_time,   start_x,  start_y  ],          &
          iNC_Count=[ count_time,   count_x,  count_y  ],          &
          iNC_Stride=[ stride_time, stride_x, stride_y ],          &
          iNC_Vars=iTemp)

        iIndex = 0
        do iCol=iFromCol, iToCol, iByCol
          do iRow=iFromRow, iToRow, iByRow
            iIndex = iIndex + 1
            rValues(iCol,iRow) = rTemp(iIndex)
          enddo
        enddo

      case ("tyx")    ! time, row, col

        call nf_get_variable_array_as_vector_float(this=this, &
          iVariableID=varid,                                         &
          iNC_Start=[ start_time,   start_y,  start_x  ],          &
          iNC_Count=[ count_time,   count_y,  count_x  ],          &
          iNC_Stride=[ stride_time, stride_y, stride_x ],          &
          iNC_Vars=iTemp)

        iIndex = 0
        do iRow=iFromRow, iToRow, iByRow
          do iCol=iFromCol, iToCol, iByCol
            iIndex = iIndex + 1
            rValues(iCol,iRow) = rTemp(iIndex)
          enddo
        enddo

      case default    

        call warn("INTERNAL PROGRAMMING ERROR: Unhandled select case. Program will probably fail.", &
          __FILE__, __LINE__)

    end select

  end associate  

end subroutine nf_get_variable_slice_float

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_vector_short(this, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, iNC_Vars)

  class (NETCDF4_FILE_T), intent(inout) :: this
  integer (kind=c_int) :: iVariableID
  integer (kind=c_size_t) :: iNC_Start
  integer (kind=c_size_t) :: iNC_Count
  integer (kind=c_ptrdiff_t) :: iNC_Stride
  integer (kind=c_short), dimension(:) :: iNC_Vars

  call nf_trap(nc_get_vars_short(ncid=this%iNCID, &
       varid=iVariableID, &
       startp=[iNC_Start], &
       countp=[iNC_Count], &
       stridep=[iNC_Stride], &
       vars=iNC_Vars), __FILE__, __LINE__ )

end subroutine nf_get_variable_vector_short

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_array_short(this, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, iNC_Vars)

  class (NETCDF4_FILE_T), intent(inout) :: this
  integer (kind=c_int) :: iVariableID
  integer (kind=c_size_t), dimension(:) :: iNC_Start
  integer (kind=c_size_t), dimension(:) :: iNC_Count
  integer (kind=c_size_t), dimension(:) :: iNC_Stride
  integer (kind=c_short), dimension(:,:) :: iNC_Vars

  call nf_trap(nc_get_vars_short(ncid=this%iNCID, &
       varid=iVariableID, &
       startp=[iNC_Start], &
       countp=[iNC_Count], &
       stridep=[iNC_Stride], &
       vars=iNC_Vars), __FILE__, __LINE__ )

end subroutine nf_get_variable_array_short

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_array_as_vector_short(this, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, iNC_Vars)

  class (NETCDF4_FILE_T), intent(inout) :: this
  integer (kind=c_int) :: iVariableID
  integer (kind=c_size_t), dimension(:) :: iNC_Start
  integer (kind=c_size_t), dimension(:) :: iNC_Count
  integer (kind=c_ptrdiff_t), dimension(:) :: iNC_Stride
  integer (kind=c_short), dimension(:) :: iNC_Vars

  call nf_trap(nc_get_vars_short(ncid=this%iNCID, &
       varid=iVariableID, &
       startp=[iNC_Start], &
       countp=[iNC_Count], &
       stridep=[iNC_Stride], &
       vars=iNC_Vars), __FILE__, __LINE__ )

end subroutine nf_get_variable_array_as_vector_short

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_array_as_vector_int(this, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, iNC_Vars)

  class (NETCDF4_FILE_T), intent(inout)     :: this
  integer (kind=c_int)                     :: iVariableID
  integer (kind=c_size_t), dimension(:)    :: iNC_Start
  integer (kind=c_size_t), dimension(:)    :: iNC_Count
  integer (kind=c_ptrdiff_t), dimension(:) :: iNC_Stride
  integer (kind=c_int), dimension(:)       :: iNC_Vars

  call nf_trap(nc_get_vars_int(ncid=this%iNCID, &
       varid=iVariableID, &
       startp=[iNC_Start], &
       countp=[iNC_Count], &
       stridep=[iNC_Stride], &
       vars=iNC_Vars), __FILE__, __LINE__ )

end subroutine nf_get_variable_array_as_vector_int

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_vector_int(this, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, iNC_Vars)

  class (NETCDF4_FILE_T), intent(inout) :: this
  integer (kind=c_int) :: iVariableID
  integer (kind=c_size_t) :: iNC_Start
  integer (kind=c_size_t) :: iNC_Count
  integer (kind=c_ptrdiff_t) :: iNC_Stride
  integer (kind=c_int), dimension(:) :: iNC_Vars

  call nf_trap(nc_get_vars_int(ncid=this%iNCID, &
       varid=iVariableID, &
       startp=[iNC_Start], &
       countp=[iNC_Count], &
       stridep=[iNC_Stride], &
       vars=iNC_Vars), __FILE__, __LINE__ )

end subroutine nf_get_variable_vector_int

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_vector_double(this, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, dpNC_Vars)

  class (NETCDF4_FILE_T), intent(inout) :: this
  integer (kind=c_int) :: iVariableID
  integer (kind=c_size_t) :: iNC_Start
  integer (kind=c_size_t) :: iNC_Count
  integer (kind=c_size_t) :: iNC_Stride
  real (kind=c_double), dimension(:) :: dpNC_Vars

  call nf_trap(nc_get_vars_double(ncid=this%iNCID, &
       varid=iVariableID, &
       startp=[iNC_Start], &
       countp=[iNC_Count], &
       stridep=[iNC_Stride], &
       vars=dpNC_Vars), __FILE__, __LINE__ )

end subroutine nf_get_variable_vector_double

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_array_double(this, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, dpNC_Vars)

  class (NETCDF4_FILE_T), intent(inout) :: this
  integer (kind=c_int) :: iVariableID
  integer (kind=c_size_t), dimension(:) :: iNC_Start
  integer (kind=c_size_t), dimension(:) :: iNC_Count
  integer (kind=c_size_t), dimension(:) :: iNC_Stride
  real (kind=c_double), dimension(:,:) :: dpNC_Vars

  call nf_trap(nc_get_vars_double(ncid=this%iNCID, &
       varid=iVariableID, &
       startp=[iNC_Start], &
       countp=[iNC_Count], &
       stridep=[iNC_Stride], &
       vars=dpNC_Vars), __FILE__, __LINE__ )

end subroutine nf_get_variable_array_double

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_array_as_vector_double(this, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, dpNC_Vars)

  class (NETCDF4_FILE_T), intent(inout) :: this
  integer (kind=c_int) :: iVariableID
  integer (kind=c_size_t), dimension(:) :: iNC_Start
  integer (kind=c_size_t), dimension(:) :: iNC_Count
  integer (kind=c_ptrdiff_t), dimension(:) :: iNC_Stride
  real (kind=c_double), dimension(:) :: dpNC_Vars

  call nf_trap(nc_get_vars_double(ncid=this%iNCID, &
       varid=iVariableID, &
       startp=[iNC_Start], &
       countp=[iNC_Count], &
       stridep=[iNC_Stride], &
       vars=dpNC_Vars), __FILE__, __LINE__ )

end subroutine nf_get_variable_array_as_vector_double

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_vector_float(this, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, rNC_Vars )

  class (NETCDF4_FILE_T), intent(inout) :: this
  integer (kind=c_int) :: iVariableID
  integer (kind=c_size_t) :: iNC_Start
  integer (kind=c_size_t) :: iNC_Count
  integer (kind=c_ptrdiff_t) :: iNC_Stride
  real (kind=c_float), dimension(:) :: rNC_Vars

  call nf_trap(nc_get_vars_float(ncid=this%iNCID, &
       varid=iVariableID, &
       startp=[iNC_Start], &
       countp=[iNC_Count], &
       stridep=[iNC_Stride], &
       vars=rNC_Vars), __FILE__, __LINE__ )

end subroutine nf_get_variable_vector_float

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_array_float(this, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, rNC_Vars )

  class (NETCDF4_FILE_T), intent(inout) :: this
  integer (kind=c_int) :: iVariableID
  integer (kind=c_size_t), dimension(:) :: iNC_Start
  integer (kind=c_size_t), dimension(:) :: iNC_Count
  integer (kind=c_ptrdiff_t), dimension(:) :: iNC_Stride
  real (kind=c_float), dimension(:,:) :: rNC_Vars

  call nf_trap(nc_get_vars_float(ncid=this%iNCID, &
       varid=iVariableID, &
       startp=[iNC_Start], &
       countp=[iNC_Count], &
       stridep=[iNC_Stride], &
       vars=rNC_Vars), __FILE__, __LINE__ )

end subroutine nf_get_variable_array_float

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_array_as_vector_float(this, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, rNC_Vars )

  class (NETCDF4_FILE_T), intent(inout) :: this
  integer (kind=c_int) :: iVariableID
  integer (kind=c_size_t), dimension(:) :: iNC_Start
  integer (kind=c_size_t), dimension(:) :: iNC_Count
  integer (kind=c_ptrdiff_t), dimension(:) :: iNC_Stride
  real (kind=c_float), dimension(:) :: rNC_Vars

  call nf_trap(nc_get_vars_float(ncid=this%iNCID, &
       varid=iVariableID, &
       startp=[iNC_Start], &
       countp=[iNC_Count], &
       stridep=[iNC_Stride], &
       vars=rNC_Vars), __FILE__, __LINE__ )

end subroutine nf_get_variable_array_as_vector_float

!--------------------------------------------------------------------------------------------------

subroutine nf_dump_cdl(this, iLU)

  class (NETCDF4_FILE_T )             :: this
  integer                             :: iLU

  ! [ LOCALS ]
  type (NETCDF4_ATTRIBUTE_T), pointer :: pNC_ATT
  type (NETCDF4_VARIABLE_T), pointer  :: pNC_VAR
  type (NETCDF4_DIMENSION_T), pointer :: pNC_DIM
  character (len=256)                 :: sBuf, sBuf2
  character (len=256)                 :: sDimensionName
  integer (kind=c_int)                :: iDimensionID
  integer (kind=c_int)                :: iUbound

  integer :: iResult, iIndex, iIndex2, iIndex3, iIndex4
  integer (kind=c_int) :: iNumberOfDimensions

  sBuf=""; sBuf2=""

  write(unit=iLU, fmt="(a)") "netcdf "//trim(this%sFilename)//" {"
  write(unit=iLU, fmt="(a)") "  dimensions:"

  do iIndex = 0, ubound( this%pNC_DIM, 1)
    write(unit=iLU, fmt="(4x,a, ' = ', i0, ';')") trim(this%pNC_DIM(iIndex)%sDimensionName), &
      this%pNC_DIM(iIndex)%iDimensionSize
  enddo

  do iIndex = 0, ubound( this%pNC_VAR, 1)

    pNC_VAR => this%pNC_VAR(iIndex)

    if( iNumberOfDimensions > 0) then

      sBuf = ' ('

      iUbound = pNC_VAR%iNumberOfDimensions - 1
      do iIndex3 = 0, iUbound

        iDimensionID = pNC_VAR%iDimensionID(iIndex3)

        call assert(iDimensionID >=0 .and. &
          iDimensionID <= ubound( this%pNC_DIM, 1 ), &
          "INTERNAL PROGRAMMING ERROR -- iDimensionID out of bounds", &
          trim(__FILE__), __LINE__)

        pNC_DIM => this%pNC_DIM(iDimensionID)
        sDimensionName = pNC_DIM%sDimensionName

        write(sBuf2, fmt="(i12)") pNC_DIM%iDimensionSize
        sBuf = trim(sBuf)//trim(pNC_DIM%sDimensionName)//"=" &
           //trim(adjustl(sBuf2))

        if (iIndex3 /= iUbound) sBuf = trim(sBuf)//", "

      enddo

      sBuf = trim(sBuf)//')'

    else

      sBuf = ""

    endif

    sBuf = trim(NETCDF_DATA_TYPE(pNC_VAR%iVariableType)) &
       //" "//trim(pNC_VAR%sVariableName)//sBuf//";"

    write(unit=iLU, fmt="(2x,a)") trim(sBuf)

    iUbound = pNC_VAR%iNumberOfAttributes - 1
    do iIndex3 = 0, iUbound

      pNC_ATT => this%pNC_VAR(iIndex)%pNC_ATT(iIndex3)

      sBuf = trim(pNC_VAR%sVariableName)//":"//trim(pNC_ATT%sAttributeName )//" ="

      sBuf = trim(sBuf)//" "//trim(pNC_ATT%slValues%cat() )

      sBuf=trim(sBuf)//"; // "//trim(NETCDF_DATA_TYPE(pNC_ATT%iAttributeType) )

      write(unit=iLU, fmt="(4x,a)") trim(sBuf)

    enddo

  enddo

  do iIndex = 0, ubound( this%pNC_ATT, 1)

    pNC_ATT => this%pNC_ATT(iIndex)

    sBuf = ":"//trim(pNC_ATT%sAttributeName )//" ="

    sBuf = trim(sBuf)//" "//trim(pNC_ATT%slValues%cat() )

    sBuf=trim(sBuf)//"; // "//trim(NETCDF_DATA_TYPE(pNC_ATT%iAttributeType) )

    write(unit=iLU, fmt="(a)") trim(sBuf)

  enddo

  write(unit=iLU, fmt="(a,/,/)") "}"


end subroutine nf_dump_cdl

!--------------------------------------------------------------------------------------------------

function nf_get_first_and_last(this, pNC_VAR)  result(dpValues)

  type (NETCDF4_FILE_T )             :: this 
  type (NETCDF4_VARIABLE_T), pointer :: pNC_VAR
  real (kind=c_double), dimension(0:1) :: dpValues

  ! [ LOCALS ]
  type (NETCDF4_DIMENSION_T), pointer :: pNC_DIM
  integer (kind=c_int) :: iDimensionSize
  integer (kind=c_int) :: iDimensionIndex
  integer (kind=c_size_t) :: iStride
  integer (kind=c_size_t) :: iCount
  integer (kind=c_short), dimension(0:1) :: spValues
  integer (kind=c_int), dimension(0:1) :: ipValues
  real (kind=c_float), dimension(0:1) :: rpValues

  iDimensionSize = nf_return_dimension_size(this, pNC_VAR%iDimensionID(0) )

  if (iDimensionSize > 1) then
    iCount = 2_c_size_t
    iStride = int(iDimensionSize, kind=c_size_t) - 1_c_size_t
  else
    iCount = 1_c_size_t
    iStride = 1_c_size_t
  endif

  select case (pNC_VAR%iVariableType )

    case (NC_SHORT)

      call nf_get_variable_vector_short(this=this, &
        iVariableID=pNC_VAR%iVariableID, &
        iNC_Start=0_c_size_t, &
        iNC_Count=iCount, &
        iNC_Stride=iStride, &
        iNC_Vars=spValues)

      dpValues = real(spValues, kind=c_double)

    case (NC_INT)

      call nf_get_variable_vector_int(this=this, &
        iVariableID=pNC_VAR%iVariableID, &
        iNC_Start=0_c_size_t, &
        iNC_Count=iCount, &
        iNC_Stride=iStride, &
        iNC_Vars=ipValues)

      dpValues = real(ipValues, kind=c_double)

    case (NC_FLOAT)

      call nf_get_variable_vector_float(this=this, &
        iVariableID=pNC_VAR%iVariableID, &
        iNC_Start=0_c_size_t, &
        iNC_Count=iCount, &
        iNC_Stride=iStride, &
        rNC_Vars=rpValues)

      dpValues = real(rpValues, kind=c_double)

    case (NC_DOUBLE)

      call nf_get_variable_vector_double(this=this, &
        iVariableID=pNC_VAR%iVariableID, &
        iNC_Start=0_c_size_t, &
        iNC_Count=iCount, &
        iNC_Stride=iStride, &
        dpNC_Vars=dpValues)

    case default    

      call warn("INTERNAL PROGRAMMING ERROR: Unhandled select case. Program will probably fail.", &
        __FILE__, __LINE__)

  end select

  !> if there is only one day of data in this NetCDF file, the
  !> first day equals the last day
  if (iCount == 1) dpValues(NC_LAST) = dpValues(NC_FIRST)

end function nf_get_first_and_last

!--------------------------------------------------------------------------------------------------

subroutine nf_calculate_time_range(this)

  class (NETCDF4_FILE_T), intent(inout) :: this

  this%iOriginJD = julian_day(this%iOriginYear, &
    this%iOriginMonth, this%iOriginDay)

  this%iFirstDayJD = this%iOriginJD + this%dpFirstAndLastTimeValues(NC_FIRST)
  this%iLastDayJD = this%iOriginJD + this%dpFirstAndLastTimeValues(NC_LAST)

end subroutine nf_calculate_time_range

!--------------------------------------------------------------------------------------------------

subroutine nf_get_time_units(this)

  class (NETCDF4_FILE_T), intent(inout) :: this

  ! [ LOCALS ]
  character (len=256)   :: sDateTime
  character (len=256)   :: sItem
  integer (kind=c_int)  :: iIndex
  logical (kind=c_bool) :: lFound
  integer (kind=c_int)  :: iStat


  call assert( associated(this%pNC_VAR_TIME), "INTERNAL PROGRAMMING ERROR--attempted to use null pointer", &
    __FILE__, __LINE__ )


  lFound = lFALSE

  do iIndex=0, this%pNC_VAR_TIME%iNumberOfAttributes - 1

    if ( this%pNC_VAR_TIME%pNC_ATT(iIndex)%sAttributeName .strequal. "units"   ) then
      lFound = lTRUE
      exit
    endif

  enddo

  call assert (lFound, "Failed to find the 'units' attribute associated " &
    //"with time variable "//dquote( this%pNC_VAR_TIME%sVariableName ), &
    trim(__FILE__), __LINE__)

  sDateTime = this%pNC_VAR_TIME%pNC_ATT(iIndex)%slValues%get(1)

  call chomp(sDateTime, sItem)    !> should be "days"
  call chomp(sDateTime, sItem)    !> should be "since"

  call chomp(sDateTime, sItem, "/-")
  read(sItem, *) this%iOriginYear

  call chomp(sDateTime, sItem, "/-")
  read(sItem, *) this%iOriginMonth

  !> @todo this does not appear to have the fix that was applied to the master swb branch to 
  !!       deal with cases where no time values are given at all

  read(sDateTime, *) this%iOriginDay

  call chomp(sDateTime, sItem, ":")
  read(sItem, *) this%iOriginHH

  call chomp(sDateTime, sItem, ":")
  read(sItem, *) this%iOriginMM

  read(sDateTime, *) this%iOriginSS

end subroutine nf_get_time_units

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_units(this)

  class (NETCDF4_FILE_T), intent(inout) :: this

  ! [ LOCALS ]
  type (NETCDF4_VARIABLE_T), pointer :: pNC_VAR
  integer (kind=c_int) :: iIndex, iIndex2
  logical (kind=c_bool) :: lFound
  integer (kind=c_int) :: iStat

  do iIndex = 0, ubound(this%pNC_VAR, 1)

    call warn(this%pNC_VAR(iIndex)%iVariableID >= 0, "INTERNAL PROGRAMMING ERROR -- " &
    //"nc_get_XYZ_units must be called only after a call is made to ~" &
    //"netcdf_get_variable_ids", trim(__FILE__), __LINE__)

    lFound = lFALSE

    do iIndex2=0, this%pNC_VAR(iIndex)%iNumberOfAttributes - 1

      if ( this%pNC_VAR(iIndex)%pNC_ATT(iIndex2)%sAttributeName .strequal. "units" ) then
        lFound = lTRUE
        exit
      endif

    enddo

    if (lFound) then
      this%pNC_VAR(iIndex)%sVariableUnits = trim( this%pNC_VAR(iIndex)%pNC_ATT(iIndex2)%slValues%get(1))
    endif

  enddo

end subroutine nf_get_variable_units

!--------------------------------------------------------------------------------------------------

subroutine nf_get_scale_and_offset(this, pNC_VAR)

  class (NETCDF4_FILE_T), intent(inout) :: this
  type (NETCDF4_VARIABLE_T), pointer :: pNC_VAR

  ! [ LOCALS ]
  integer (kind=c_int)    :: iIndex
  logical (kind=c_bool)   :: lFound
  integer (kind=c_int)    :: iStat
  character (len=32)      :: sBuf

  call assert( associated(this%pNC_VAR), "INTERNAL PROGRAMMING ERROR--attempted use of null pointer", &
    __FILE__, __LINE__ )

  call assert( pNC_VAR(iIndex)%iVariableID >= 0, "INTERNAL PROGRAMMING ERROR -- " &
    //"nc_get_scale_and_offset must be called only after a call is made to ~" &
    //"netcdf_get_variable_ids", trim(__FILE__), __LINE__ )

  associate ( var => this%pNC_VAR )

    lFound = lFALSE
    
    !> loop over all attributes of the selected variable; attemmpt to find "scale_factor" attribute
    do iIndex2=0, var%iNumberOfAttributes - 1

      if ( var%pNC_ATT(iIndex2)%sAttributeName .strequal. "scale_factor" ) then
        lFound = lTRUE
        exit
      endif

    enddo

    if (lFound) then
      sBuf = trim( var%pNC_ATT(iIndex2)%slValues%get(1) )
      read(sBuf,*) var%rScaleFactor
    endif

    !> Now repeat the process for "add_offset" attribute
    lFound = lFALSE

    !> loop over all attributes of the selected variable; attemmpt to find "add_offset" attribute
    do iIndex2=0, var%iNumberOfAttributes - 1

      if ( var%pNC_ATT(iIndex2)%sAttributeName .strequal. "add_offset" ) then
        lFound = lTRUE
        exit
      endif

    enddo

    if (lFound) then
      sBuf = trim(var%pNC_ATT(iIndex2)%slValues%get(1) )
      read(sBuf,*) var%rAddOffset
    endif

  end associate


end subroutine nf_get_scale_and_offset

!--------------------------------------------------------------------------------------------------

function nf_find_variable( this, sVariableName )    result( pNC_VAR )

  class (NETCDF4_FILE_T), intent(inout) :: this
  character (len=*), intent(in)        :: sVariableName
  type (NETCDF4_VARIABLE_T), pointer   :: pNC_VAR

  ! [ LOCALS ]
  integer (kind=c_int)  :: iIndex
  logical (kind=c_bool) :: lFound

  call assert( associated(this%pNC_VAR), "INTERNAL PROGRAMMING ERROR--attempted use of null pointer", &
    __FILE__, __LINE__ )

  pNC_VAR => null()

  do iIndex = 0, ubound(this%pNC_VAR, 1)

    lFound = lFALSE

    if ( this%pNC_VAR(iIndex)%sVariableName .strequal. sVariableName ) then
      lFound = lTRUE
      exit
    endif

  enddo

  if (lFound) then
    pNC_VAR => this%pNC_VAR(iIndex)
  endif

end function nf_find_variable
  
!--------------------------------------------------------------------------------------------------

function nf_return_index_double(rValues, rTargetValue)  result(iIndex)

  real (kind=c_double), dimension(:) :: rValues
  real (kind=c_double) :: rTargetValue
  integer (kind=c_int) :: iIndex

  ! [ LOCALS ]
  integer (kind=c_int) :: iCount
  real (kind=c_double) :: rDiff, rDiffMin

  if ( .not. (rTargetValue >= minval(rValues) .and. rTargetValue <= maxval(rValues)) ) then
    call LOGS%write("rTargetValue (" &
    //asCharacter(rTargetValue)//") is not within the range " &
    //asCharacter(minval(rValues))//" to "//asCharacter(maxval(rValues)), lEcho=lTRUE )

    call assert(lFALSE, "INTERNAL PROGRAMMING ERROR", trim(__FILE__), __LINE__)
  endif

  rDiffMin = 1.e+20

  do iCount=lbound(rValues,1), ubound(rValues,1)

    rDiff = abs(rValues(iCount) - rTargetValue)

    if ( rDiff < rDiffMin ) then
      iIndex = iCount
      rDiffMin =rDiff
    endif

  enddo

end function nf_return_index_double

!--------------------------------------------------------------------------------------------------

function nf_coord_to_col_row(this, rX, rY)  result(iColRow)

  class (NETCDF4_FILE_T )             :: this
  real (kind=c_double) :: rX
  real (kind=c_double) :: rY
  integer (kind=c_size_t), dimension(2) :: iColRow


  ! [ LOCALS ]
  integer (kind=c_int) :: iColNum, iRowNum

  if (rX < minval(this%rX_Coords) ) &
    call die( "X coordinate value "//asCharacter(rX)//" is less than the minimum X coordinate " &
      //"value ("//asCharacter(minval(this%rX_Coords))//") contained in the NetCDF file " &
      //dquote(this%sFilename) )

  if (rX > maxval(this%rX_Coords) ) &
    call die( "X coordinate value "//asCharacter(rX)//" is greater than the maximum X coordinate " &
      //"value ("//asCharacter(minval(this%rX_Coords))//") contained in the NetCDF file " &
      //dquote(this%sFilename) )

  if (rY < minval(this%rY_Coords) ) &
    call die( "Y coordinate value "//asCharacter(rY)//" is less than the minimum Y coordinate " &
      //"value ("//asCharacter(minval(this%rY_Coords))//") contained in the NetCDF file " &
      //dquote(this%sFilename) )

  if (rY > maxval(this%rY_Coords) ) &
    call die( "Y coordinate value "//asCharacter(rY)//" is greater than the maximum Y coordinate " &
      //"value ("//asCharacter(minval(this%rY_Coords))//") contained in the NetCDF file " &
      //dquote(this%sFilename) )

  iColNum = nf_return_index_double(this%rX_Coords, rX)
  iRowNum = nf_return_index_double(this%rY_Coords, rY)

  iColRow(COLUMN) = iColNum
  iColRow(ROW) = iRowNum

end function nf_coord_to_col_row

!--------------------------------------------------------------------------------------------------

function nf_lookup_varid(this, sVariableName)  result(iVariableID)

  class (NETCDF4_FILE_T )             :: this
  character (len=*) :: sVariableName
  integer (kind=c_int) :: iVariableID

  ! [ LOCALS ]
  integer (kind=c_int) :: iIndex
  type (NETCDF4_VARIABLE_T), pointer :: pNC_VAR

  iVariableID = -9999

  do iIndex=0, ubound(this%pNC_VAR,1)

    pNC_VAR => this%pNC_VAR(iIndex)

    if( sVariableName .strequal. pNC_VAR%sVariableName ) then

      iVariableID = iIndex
      exit

    endif

  enddo

end function nf_lookup_varid

!--------------------------------------------------------------------------------------------------

subroutine nf_create(this, sFilename, iLU)

  class (NETCDF4_FILE_T )             :: this
  character (len=*) :: sFilename
  integer (kind=c_int), optional :: iLU

  character (len=256) :: sBuf
  call getcwd(sBuf)

  call nf_trap(nc_create(path=trim(fortran_to_c_string(sFilename)), &
                 cmode=NC_NETCDF4, &
                 ncidp=this%iNCID), &
                 __FILE__, __LINE__)

  this%sFilename = trim(sFilename)
  this%iFileFormat = NC_FORMAT_NETCDF4

  if (present(iLU) ) then
    call LOGS%write("Created NetCDF file for output. Filename: " &
      //dquote(this%sFilename)//"; NCID="//asCharacter(this%iNCID) )
  endif

end subroutine nf_create

!--------------------------------------------------------------------------------------------------

subroutine nf_put_deflate(this, iVariableID, iShuffle, iDeflate, iDeflate_level)

  class (NETCDF4_FILE_T )             :: this
  integer (kind=c_int) :: iVariableID
  integer (kind=c_int) :: iShuffle
  integer (kind=c_int) :: iDeflate
  integer (kind=c_int) :: iDeflate_level

  call nf_trap(nc_def_var_deflate(ncid=this%iNCID, &
          varid=iVariableID, &
          shuffle=iShuffle, &
          deflate=iDeflate, &
          deflate_level=iDeflate_level), &
          __FILE__, __LINE__)

end subroutine nf_put_deflate

!--------------------------------------------------------------------------------------------------

subroutine nf_enddef(this)

  class (NETCDF4_FILE_T )             :: this

  call nf_trap(nc_enddef(ncid=this%iNCID), &
       __FILE__, __LINE__)

end subroutine nf_enddef

!--------------------------------------------------------------------------------------------------

function nf_put_dimension(this, sDimensionName, iDimensionSize) &
      result(iDimensionID)

  class (NETCDF4_FILE_T )             :: this
  character (len=*)                   :: sDimensionName
  integer (kind=c_int)                :: iDimensionSize

  ! [ LOCALS ]
  integer (kind=c_int)                :: iDimensionID
  integer (kind=c_size_t)             :: iDimensionSize_

  iDimensionSize_ = int(iDimensionSize, kind=c_size_t)

  call nf_trap(nc_def_dim(ncid=this%iNCID, &
                          name=trim(sDimensionName)//c_null_char, &
                          lenv=iDimensionSize_, &
                          dimidp=iDimensionID), &
                          __FILE__, __LINE__)

end function nf_put_dimension

!--------------------------------------------------------------------------------------------------

subroutine nf_put_dimensions( this )

  class (NETCDF4_FILE_T) :: this

  ! [ LOCALS ]
  integer (kind=c_int) :: iIndex
  type (NETCDF4_DIMENSION_T), pointer :: pNC_DIM

  do iIndex = 0, ubound( this%pNC_DIM, 1 )

    pNC_DIM => this%pNC_DIM(iIndex)

    call nf_trap(nc_def_dim(ncid=this%iNCID, &
      name=trim(pNC_DIM%sDimensionName)//c_null_char, &
      lenv=pNC_DIM%iDimensionSize, &
      dimidp=pNC_DIM%iDimensionID), &
      __FILE__, __LINE__ )

  enddo

end subroutine nf_put_dimensions

!--------------------------------------------------------------------------------------------------

! subroutine nf_define_standard_dimensions(this, iNX, iNY, lIncludeTime)

!   type (NETCDF4_FILE_T ), intent(inout)           :: this
!   integer (kind=c_int), intent(in)                :: iNX
!   integer (kind=c_int), intent(in)                :: iNY
!   logical (kind=c_bool), intent(in), optional     :: lIncludeTime

!   ! [ LOCALS ]
!   integer (kind=c_int)  :: iStat
!   logical (kind=c_bool) :: lIncludeTime_
!   integer (kind=c_int)  :: iNumberOfDimensions

!   if (present( lIncludeTime) ) then
!     lIncludeTime_ = lIncludeTime
!   else
!     lIncludeTime_ = lTRUE
!   endif    

!   if ( lIncludeTime_ ) then
!     iNumberOfDimensions = 3
!   else
!     iNumberOfDimensions = 2
!   endif  

!   iStat = 0

!   if (associated(this%pNC_DIM) ) deallocate(this%pNC_DIM, stat=iStat)
!   call assert(iStat == 0, "Could not deallocate memory for NC_DIM member in NC_FILE defined type", &
!     trim(__FILE__), __LINE__)

!   allocate(this%pNC_DIM( 0 : ubound( this%pNC_DIM, 1 ) ), stat=iStat )
!   call assert(iStat == 0, "Could not allocate memory for NC_DIM member in NC_FILE defined type", &
!     trim(__FILE__), __LINE__)

!   if ( lIncludeTime_ ) then
!     this%pNC_DIM_TIME => this%pNC_DIM(2)
!     !> define the time dimension;
!     this%pNC_DIM_TIME%sDimensionName = "time"
!     this%pNC_DIM_TIME%iDimensionSize = NC_UNLIMITED
!   endif

!   !> define the y dimension;
!   this%pNC_DIM_Y => this%pNC_DIM(0)
!   this%pNC_DIM_Y%sDimensionName = "y"
!   this%pNC_DIM_Y%iDimensionSize = iNY

!   !> define the x dimension;
!   this%pNC_DIM_X => this%pNC_DIM(1)
!   this%pNC_DIM_X%sDimensionName = "x"
!   this%pNC_DIM_X%iDimensionSize = iNX

! end subroutine nf_define_standard_dimensions

! !--------------------------------------------------------------------------------------------------

! subroutine nf_define_standard_variables(this, slVariableNames, iVariableTypes, iNumberOfDimensions, &
!     lLatLon, lTime )

!   type (NETCDF4_FILE_T )               :: this
!   type (STRING_LIST_T)                 :: slVariableNames
!   integer (kind=c_int)                 :: iVariableTypes(:)
!   integer (kind=c_int)                 :: iNumberOfDimensions(:)
!   logical (kind=c_bool), optional      :: lLatLon
!   logical (kind=c_bool), optional      :: lTime

!   ! [ LOCALS ]
!   type ( NETCDF4_VARIABLE_T), pointer   :: pNC_VAR
!   integer (kind=c_int)                  :: iStat
!   integer (kind=c_int)                  :: iIndex
!   integer (kind=c_int)                  :: iNumberOfVariables
!   logical (kind=c_bool)                 :: lLatLon_
!   logical (kind=c_bool)                 :: lTime_

!   iStat = 0
!   iIndex = 0
!   iNumberOfVariables = 2 

!   if ( present( lLatLon) ) then
!     lLatLon_ = lLatLon
!   else
!     lLatLon_ = lFALSE  
!   endif  

!   if ( present( lTime ) ) then
!     lTime_ = lTime
!   else 
!     lTime = lTRUE
!   endif  

!   if ( lLatLon_ ) iNumberOfVariables = iNumberOfVariables + 2
!   if ( lTime_ ) iNumberOfVariables = iNumberOfVariables + 1

!   iNumberOfVariables = iNumberOfVariables + slVariableNames%count

!   if (associated(this%pNC_VAR) ) deallocate(this%pNC_VAR, stat=iStat)
!   call assert(iStat == 0, "Could not deallocate memory for NC_VAR member in NC_FILE defined type", &
!     trim(__FILE__), __LINE__)

!   allocate(this%pNC_VAR( 0 : iNumberOfVariables-1), stat=iStat )
!   call assert(iStat == 0, "Could not allocate memory for NC_VAR member in NC_FILE defined type", &
!     trim(__FILE__), __LINE__)

!   this%pNC_VAR_TIME => this%pNC_VAR( iIndex )
!   this%pNC_VAR_TIME%sVariableName = "time"
!   this%pNC_VAR_TIME%iVariableType = NC_DOUBLE
!   this%pNC_VAR_TIME%iNumberOfDimensions = 1
! !  pNC_VAR_TIME%iDimensionID(0) = this%pNC_DIM(NC_TIME)%iDimensionID

!   iIndex = iIndex + 1
!   this%pNC_VAR_Y => this%pNC_VAR( iIndex )
!   this%pNC_VAR_Y%sVariableName = "y"
!   this%pNC_VAR_Y%iVariableType = NC_DOUBLE
!   this%pNC_VAR_Y%iNumberOfDimensions = 1
! !  pNC_VAR_Y%iDimensionID = this%pNC_DIM(NC_Y)%iDimensionID

!   iIndex = iIndex + 1
!   this%pNC_VAR_X => this%pNC_VAR( iIndex )
!   this%pNC_VAR_X%sVariableName = "x"
!   this%pNC_VAR_X%iVariableType = NC_DOUBLE
!   this%pNC_VAR_X%iNumberOfDimensions = 1
! !  pNC_VAR_X%iDimensionID = this%pNC_DIM(NC_X)%iDimensionID  

!   if ( lLatLon_ ) then

!     iIndex = iIndex + 1
!     this%pNC_VAR_LAT => this%pNC_VAR( iIndex )
!     this%pNC_VAR_LAT%sVariableName = "lat"
!     this%pNC_VAR_LAT%iVariableType = NC_FLOAT
!     this%pNC_VAR_LAT%iNumberOfDimensions = 2
! !    pNC_VAR_LAT%iDimensionID = [this%pNC_DIM(NC_X)%iDimensionID, &
! !                                        this%pNC_DIM(NC_Y)%iDimensionID,0,0]

!     iIndex = iIndex + 1
!     this%pNC_VAR_LON => this%pNC_VAR( iIndex )
!     this%pNC_VAR_LON%sVariableName = "lon"
!     this%pNC_VAR_LON%iVariableType = NC_FLOAT
!     this%pNC_VAR_LON%iNumberOfDimensions = 2
! !    this%pNC_VAR(NC_LON)%iDimensionID = [this%pNC_DIM(NC_X)%iDimensionID, &
! !                                        this%pNC_DIM(NC_Y)%iDimensionID,0,0]
!   endif

!   iIndex = iIndex 

!   do iIndex2 = 1, slVariableNames%count

!     pNC_VAR => this%pNC_VAR( iIndex + iIndex2 )
!     pNC_VAR%sVariableName = slVariableNames%get( iIndex2 )
!     pNC_VAR%iVariableType = iVariableTypes( iIndex2 )
!     pNC_VAR%iNumberOfDimensions = iNumberOfDimensions( iIndex2 )
!   !  pNC_VAR_VALUE%iDimensionID = [this%pNC_DIM(NC_TIME)%iDimensionID, &
!   !                                 this%pNC_DIM(NC_Y)%iDimensionID, &
!   !                                 this%pNC_DIM(NC_X)%iDimensionID,0]

!   enddo

! end subroutine nf_define_standard_variables

! !--------------------------------------------------------------------------------------------------

! subroutine nf_define_global_attributes(this, sDataType, sSourceFile )

!   class (NETCDF4_FILE_T )             :: this
!   character (len=*), intent(in) :: sDataType
!   character (len=*), intent(in), optional :: sSourceFile

!   ! [ LOCALS ]
!   integer (kind=c_int) :: iStat
!   type (DATETIME_T)    :: DT
!   character (len=20)   :: sDateTime
!   integer (kind=c_int) :: iNumberOfAttributes

!   call DT%systime()
!   sDateTime = DT%prettydatetime()


!   iNumberOfAttributes = 3

!   allocate( this%pNC_ATT(0:iNumberOfAttributes-1), stat=iStat)
!   call assert(iStat == 0, "Could not allocate memory for NC_ATT member of NC_FILE", &
!     trim(__FILE__), __LINE__)

!   block

!     if (present(sSourceFile) ) then

!       this%pNC_ATT(0)%sAttributeName = "source"
!       this%pNC_ATT(0)%slValues       = trim(sDataType)//" data from file "//dquote(sSourceFile)
!       this%pNC_ATT(0)%iAttributeType = NC_CHAR
!       this%pNC_ATT(0)%iAttributeSize = 1_c_size_t

!     else

!       this%pNC_ATT(0)%sAttributeName = "source"
!       this%pNC_ATT(0)%slValues       = trim(sDataType)//" data from SWB2 run (version "//SWB_VERSION &
!         //", compiled on: "//COMPILATION_TIMESTAMP//")"
!       this%pNC_ATT(0)%iAttributeType = NC_CHAR
!       this%pNC_ATT(0)%iAttributeSize = 1_c_size_t

!     endif  

!     this%pNC_ATT(1)%sAttributeName = "conventions"
!     this%pNC_ATT(1)%slValues       = "CF-1.6"
!     this%pNC_ATT(1)%iAttributeType = NC_CHAR
!     this%pNC_ATT(1)%iAttributeSize = 1_c_size_t

!     this%pNC_ATT(2)%sAttributeName = "history"
!     this%pNC_ATT(2)%slValues       = trim(sDateTime)//": Soil-Water-Balance model run started."
!     this%pNC_ATT(2)%iAttributeType = NC_CHAR
!     this%pNC_ATT(2)%iAttributeSize = 1_c_size_t

!   end block

! end subroutine nf_define_global_attributes

! !--------------------------------------------------------------------------------------------------

! subroutine nf_define_standard_attributes(this, sOriginText, lLatLon, fValidMin, fValidMax )

!   type (NETCDF4_FILE_T )             :: this
!   character (len=*), optional        :: sOriginText
!   logical (kind=c_bool), optional    :: lLatLon
!   real (kind=c_float), optional      :: fValidMin
!   real (kind=c_float), optional      :: fValidMax

!   ! [ LOCALS ]
!   integer (kind=c_int)                             :: iStat
!   integer (kind=c_int)                             :: iNumAttributes
!   type (NETCDF4_ATTRIBUTE_T), dimension(:), pointer :: pNC_ATT
!   logical (kind=c_bool)                            :: lLatLon_

!   if (present( lLatLon ) ) then
!     lLatLon_ = lLatLon
!   else
!     lLatLon_ = lFALSE
!   endif  

!   if (present( sOriginText ) ) then

!     iNumAttributes = 3
!     allocate( this%pNC_VAR_TIME%pNC_ATT(0:iNumAttributes-1), stat=iStat)
!     call assert(iStat == 0, "Could not allocate memory for NC_ATT member in NC_VAR struct of NC_FILE", &
!       trim(__FILE__), __LINE__)
!     this%pNC_VAR_TIME%iNumberOfAttributes = iNumAttributes

!     pNC_ATT => this%pNC_VAR_TIME%pNC_ATT

!     !! define attributes associated with TIME variable
!     block

!       pNC_ATT(0)%sAttributeName     = "units"
!       pNC_ATT(0)%slValues           = "days since "//trim(sOriginText)//" 00:00:00"
!       pNC_ATT(0)%iAttributeType     = NC_CHAR
!       pNC_ATT(0)%iAttributeSize     = 1_c_size_t

!       pNC_ATT(1)%sAttributeName     = "calendar"
!       pNC_ATT(1)%slValues           = "standard"
!       pNC_ATT(1)%iAttributeType     = NC_CHAR
!       pNC_ATT(1)%iAttributeSize     = 1_c_size_t

!       pNC_ATT(2)%sAttributeName     = "long_name"
!       pNC_ATT(2)%slValues           = "time"
!       pNC_ATT(2)%iAttributeType     = NC_CHAR
!       pNC_ATT(2)%iAttributeSize     = 1_c_size_t


!     end block

!   endif

!   if (present( fValidMin ) .and. present( fValidMax) ) then

!     iNumAttributes = 4
!     allocate( this%pNC_VAR_VALUE%pNC_ATT(0:iNumAttributes-1), stat=iStat)
!     call assert(iStat == 0, "Could not allocate memory for NC_ATT member in NC_VAR struct of NC_FILE", &
!       trim(__FILE__), __LINE__)
!     this%pNC_VAR_VALUE%iNumberOfAttributes = iNumAttributes

!     pNC_ATT => this%pNC_VAR_VALUE%pNC_ATT

!     pNC_ATT(0)%sAttributeName = "units"
!     pNC_ATT(0)%slValues       = this%pNC_VAR_VALUE%sVariableUnits
!     pNC_ATT(0)%iAttributeType = NC_CHAR
!     pNC_ATT(0)%iAttributeSize = 1_c_size_t

!     pNC_ATT(1)%sAttributeName = "valid_min"
!     allocate(pNC_ATT(1)%rAttValue(0:0))
!     pNC_ATT(1)%rAttValue(0) = fValidMin
!     pNC_ATT(1)%iAttributeType = NC_FLOAT
!     pNC_ATT(1)%iAttributeSize = 1_c_size_t

!     pNC_ATT(2)%sAttributeName = "valid_max"
!     allocate(pNC_ATT(2)%rAttValue(0:0))
!     pNC_ATT(2)%rAttValue(0) = fValidMax
!     pNC_ATT(2)%iAttributeType = NC_FLOAT
!     pNC_ATT(2)%iAttributeSize = 1_c_size_t

!     pNC_ATT(3)%sAttributeName = "valid_range"
!     allocate(pNC_ATT(3)%rAttValue(0:1))
!     pNC_ATT(3)%rAttValue(0) = fValidMin
!     pNC_ATT(3)%rAttValue(1) = fValidMax
!     pNC_ATT(3)%iAttributeType = NC_FLOAT
!     pNC_ATT(3)%iAttributeSize = 2_c_size_t

!   else  

!     iNumAttributes = 1
!     allocate( this%pNC_VAR_VALUE%pNC_ATT(0:iNumAttributes-1), stat=iStat)
!     call assert(iStat == 0, "Could not allocate memory for NC_ATT member in NC_VAR struct of NC_FILE", &
!       trim(__FILE__), __LINE__)
!     this%pNC_VAR_VALUE%iNumberOfAttributes = iNumAttributes

!     pNC_ATT => this%pNC_VAR_VALUE%pNC_ATT

!     pNC_ATT(0)%sAttributeName = "units"
!     pNC_ATT(0)%slValues       = this%sVariableUnits(NC_Z)
!     pNC_ATT(0)%iAttributeType = NC_CHAR
!     pNC_ATT(0)%iAttributeSize = 1_c_size_t

!   endif

!   iNumAttributes = 3
!   allocate( this%pNC_VAR_Y%pNC_ATT(0:iNumAttributes-1), stat=iStat)
!   call assert(iStat == 0, "Could not allocate memory for NC_ATT member in NC_VAR struct of NC_FILE", &
!     trim(__FILE__), __LINE__)
!   this%pNC_VAR_Y%iNumberOfAttributes = iNumAttributes

!   pNC_ATT => this%pNC_VAR_Y%pNC_ATT

!   block

!     pNC_ATT(0)%sAttributeName = "units"
!     pNC_ATT(0)%slValues       = this%pNC_VAR_Y%sVariableUnits
!     pNC_ATT(0)%iAttributeType = NC_CHAR
!     pNC_ATT(0)%iAttributeSize = 1_c_size_t

!     pNC_ATT(1)%sAttributeName = "long_name"
!     pNC_ATT(1)%slValues       = "y coordinate of projection"
!     pNC_ATT(1)%iAttributeType = NC_CHAR
!     pNC_ATT(1)%iAttributeSize = 1_c_size_t

!     pNC_ATT(2)%sAttributeName = "standard_name"
!     pNC_ATT(2)%slValues       = "projection_y_coordinate"
!     pNC_ATT(2)%iAttributeType = NC_CHAR
!     pNC_ATT(2)%iAttributeSize = 1_c_size_t


!   end block

!   iNumAttributes = 3
!   allocate( this%pNC_VAR(NC_X)%pNC_ATT(0:iNumAttributes-1), stat=iStat)
!   call assert(iStat == 0, "Could not allocate memory for NC_ATT member in NC_VAR struct of NC_FILE", &
!     trim(__FILE__), __LINE__)
!   this%pNC_VAR(NC_X)%iNumberOfAttributes = iNumAttributes

!   pNC_ATT => this%pNC_VAR(NC_X)%pNC_ATT

!   block

!     pNC_ATT(0)%sAttributeName = "units"
!     pNC_ATT(0)%slValues       = this%sVariableUnits(NC_X)
!     pNC_ATT(0)%iAttributeType = NC_CHAR
!     pNC_ATT(0)%iAttributeSize = 1_c_size_t

!     pNC_ATT(1)%sAttributeName = "long_name"
!     pNC_ATT(1)%slValues       = "x coordinate of projection"
!     pNC_ATT(1)%iAttributeType = NC_CHAR
!     pNC_ATT(1)%iAttributeSize = 1_c_size_t

!     pNC_ATT(2)%sAttributeName = "standard_name"
!     pNC_ATT(2)%slValues       = "projection_x_coordinate"
!     pNC_ATT(2)%iAttributeType = NC_CHAR
!     pNC_ATT(2)%iAttributeSize = 1_c_size_t


!   end block

!   if ( lLatLon_) then

!     iNumAttributes = 3
!     allocate( this%pNC_VAR_LAT%pNC_ATT(0:iNumAttributes-1), stat=iStat)
!     call assert(iStat == 0, "Could not allocate memory for NC_ATT member in NC_VAR struct of NC_FILE", &
!       trim(__FILE__), __LINE__)
!     this%pNC_VAR_LAT%iNumberOfAttributes = iNumAttributes

!     pNC_ATT => this%pNC_VAR_LAT%pNC_ATT

!     block

!       pNC_ATT(0)%sAttributeName = "units"
!       pNC_ATT(0)%slValues       = "degrees_north"
!       pNC_ATT(0)%iAttributeType = NC_CHAR
!       pNC_ATT(0)%iAttributeSize = 1_c_size_t

!       pNC_ATT(1)%sAttributeName = "long_name"
!       pNC_ATT(1)%slValues       = "latitude"
!       pNC_ATT(1)%iAttributeType = NC_CHAR
!       pNC_ATT(1)%iAttributeSize = 1_c_size_t

!       pNC_ATT(2)%sAttributeName = "standard_name"
!       pNC_ATT(2)%slValues       = "latitude"
!       pNC_ATT(2)%iAttributeType = NC_CHAR
!       pNC_ATT(2)%iAttributeSize = 1_c_size_t


!     end block


!     iNumAttributes = 3
!     allocate( this%pNC_VAR_LON%pNC_ATT(0:iNumAttributes-1), stat=iStat)
!     call assert(iStat == 0, "Could not allocate memory for NC_ATT member in NC_VAR struct of NC_FILE", &
!       trim(__FILE__), __LINE__)
!     this%pNC_VAR_LON%iNumberOfAttributes = iNumAttributes

!     pNC_ATT => this%pNC_VAR_LON%pNC_ATT

!     block

!       pNC_ATT(0)%sAttributeName = "units"
!       pNC_ATT(0)%slValues       = "degrees_east"
!       pNC_ATT(0)%iAttributeType = NC_CHAR
!       pNC_ATT(0)%iAttributeSize = 1_c_size_t

!       pNC_ATT(1)%sAttributeName = "long_name"
!       pNC_ATT(1)%slValues       = "longitude"
!       pNC_ATT(1)%iAttributeType = NC_CHAR
!       pNC_ATT(1)%iAttributeSize = 1_c_size_t

!       pNC_ATT(2)%sAttributeName = "standard_name"
!       pNC_ATT(2)%slValues       = "longitude"
!       pNC_ATT(2)%iAttributeType = NC_CHAR
!       pNC_ATT(2)%iAttributeSize = 1_c_size_t


!     end block

!   endif

! end subroutine nf_define_standard_attributes

!--------------------------------------------------------------------------------------------------

subroutine nf_put_x_and_y(this, dpX, dpY)

  class (NETCDF4_FILE_T) :: this
  real (kind=c_double), dimension(:) :: dpX
  real (kind=c_double), dimension(:) :: dpY

  ! [ LOCALS ]
  integer (kind=c_size_t) :: iLength
  real (kind=c_double), dimension(:), allocatable :: rX, rY

  iLength = int(size(dpX, 1), kind=c_size_t)

  call netcdf_put_variable_vector(this=this, &
                   iVariableID=this%pNC_VAR_X%iVariableID, &
                   iStart=[0_c_size_t], &
                   iCount=[iLength], &
                   iStride=[1_c_ptrdiff_t], &
                   dpValues=dpX)

  iLength = int(size(dpY, 1), kind=c_size_t)

  call netcdf_put_variable_vector(this=this, &
                   iVariableID=this%pNC_VAR_Y%iVariableID, &
                   iStart=[0_c_size_t], &
                   iCount=[iLength], &
                   iStride=[1_c_ptrdiff_t], &
                   dpValues=dpY)

end subroutine nf_put_x_and_y

!--------------------------------------------------------------------------------------------------

subroutine nf_put_lat_and_lon(this, dpLat, dpLon)

  class (NETCDF4_FILE_T) :: this
  real (kind=c_double), dimension(:,:) :: dpLat
  real (kind=c_double), dimension(:,:) :: dpLon

  ! [ LOCALS ]
  integer (kind=c_size_t) :: iNX, iNY

  iNX = int( size(dpLat, 2), kind=c_size_t)
  iNY = int( size(dpLat, 1), kind=c_size_t)

  call netcdf_put_variable_array(this=this, &
                   iVariableID=this%pNC_VAR_LAT%iVariableID, &
                   iStart=[0_c_size_t, 0_c_size_t], &
                   iCount=[ iNY, iNX ], &
                   iStride=[1_c_ptrdiff_t,1_c_ptrdiff_t], &
                   dpValues=dpLat)


  call netcdf_put_variable_array(this=this, &
                   iVariableID=this%pNC_VAR_LON%iVariableID, &
                   iStart=[0_c_size_t, 0_c_size_t], &
                   iCount=[ iNY, iNX ],&
                   iStride=[1_c_ptrdiff_t,1_c_ptrdiff_t], &
                   dpValues=dpLon)

end subroutine nf_put_lat_and_lon

!--------------------------------------------------------------------------------------------------

function nf_put_variable(this, sVariableName, iVariableType, &
   iNumberOfDimensions, iDimensionIDs)    result(iVariableID)

  class (NETCDF4_FILE_T )             :: this
  character (len=*) :: sVariableName
  integer (kind=c_int) :: iVariableType
  integer (kind=c_int) :: iNumberOfDimensions
  integer (kind=c_int), dimension(:) :: iDimensionIDs
  integer (kind=c_int) :: iVariableID

  call nf_trap( nc_def_var(ncid=this%iNCID,&
                           name=trim(fortran_to_c_string(sVariableName)), &
                           xtype=iVariableType, &
                           ndims=iNumberOfDimensions, &
                           dimidsp=iDimensionIDs, &
                           varidp=iVariableID), &
                           __FILE__, __LINE__)

end function nf_put_variable

!--------------------------------------------------------------------------------------------------

!! before this function is called, the values associated with this must be defined.

subroutine nf_put_variables( this )

  class (NETCDF4_FILE_T) :: this

  ! [ LOCALS ]
  integer (kind=c_int) :: iStat
  integer (kind=c_int) :: iIndex
  character (len=256)  :: sDimensionName
  type (NETCDF4_VARIABLE_T), pointer :: pNC_VAR

  !! note: the default number of variables for a simple archive file is 4:
  !! 0) time, 1) Y, 2) X, 3) variable of interest

  do iIndex = lbound(this%pNC_VAR,1), ubound(this%pNC_VAR,1)

    pNC_VAR => this%pNC_VAR(iIndex)

    print *, __FILE__, ": ", __LINE__, "     varname=", dquote(pNC_VAR%sVariableName)

    if ( len_trim( pNC_VAR%sVariableName ) > 0 ) then

      call nf_trap( nc_def_var(ncid=this%iNCID,&
                               name=trim(fortran_to_c_string(pNC_VAR%sVariableName)), &
                               xtype=pNC_VAR%iVariableType, &
                               ndims=pNC_VAR%iNumberOfDimensions, &
                               dimidsp=pNC_VAR%iDimensionID, &
                               varidp=pNC_VAR%iVariableID), &
                               __FILE__, __LINE__)

    endif

  enddo

end subroutine nf_put_variables

!--------------------------------------------------------------------------------------------------

subroutine nf_put_attribute(this, iVariableID, sAttributeName, &
  sAttributeValue, iAttributeValue, rAttributeValue, dpAttributeValue)

  class (NETCDF4_FILE_T )             :: this
  integer (kind=c_int)                :: iVariableID
  character (len=*)                   :: sAttributeName
  character (len=*), optional         :: sAttributeValue
  integer (kind=c_int), optional      :: iAttributeValue(:)
  real (kind=c_float), optional       :: rAttributeValue(:)
  real (kind=c_double), optional     :: dpAttributeValue(:)

  ! [ LOCALS ]
  integer (kind=c_size_t) :: iNumberOfAttributes

  if (present(sAttributeValue) ) then

    iNumberOfAttributes = size( sAttributeValue, 1)
    iNumberOfAttributes = int(len_trim(sAttributeValue(1)), kind=c_size_t)

    call nf_trap( nc_put_att_text(ncid=this%iNCID, &
                    varid=iVariableID, &
                    name=trim(sAttributeName), &
                    nlen=iNumberOfAttributes, &
                    tp=trim( sAttributeValue )), &
                    __FILE__, __LINE__)

  elseif (present(iAttributeValue) ) then

    iNumberOfAttributes = size( iAttributeValue, 1)

    call nf_trap( nc_put_att_int(ncid=this%iNCID, &
                    varid=iVariableID, &
                    name=trim(sAttributeName), &
                    xtype=NC_INT, &
                    nlen=iNumberOfAttributes, &
                     ip=iAttributeValue), &
                     __FILE__, __LINE__)

  elseif (present(rAttributeValue) ) then

    iNumberOfAttributes = size( rAttributeValue, 1)

    call nf_trap( nc_put_att_float(ncid=this%iNCID, &
                    varid=iVariableID, &
                    name=trim(sAttributeName), &
                    xtype=NC_FLOAT, &
                    nlen=iNumberOfAttributes, &
                    fp=rAttributeValue), &
                    __FILE__, __LINE__)

  elseif (present(dpAttributeValue) ) then

    iNumberOfAttributes = size( dpAttributeValue, 1)

    call nf_trap( nc_put_att_double(ncid=this%iNCID, &
                       varid=iVariableID, &
                       name=trim(sAttributeName), &
                       xtype=NC_DOUBLE, &
                       nlen=iNumberOfAttributes, &
                       dp=dpAttributeValue), &
                       __FILE__, __LINE__)

  endif


end subroutine nf_put_attribute

!--------------------------------------------------------------------------------------------------

subroutine nf_put_attributes(this)

  class (NETCDF4_FILE_T )             :: this

  ! [ LOCALS ]
  integer (kind=c_size_t)             :: iNumberOfAttributes
  type (NETCDF4_VARIABLE_T), pointer  :: pNC_VAR
  type (NETCDF4_ATTRIBUTE_T), pointer :: pNC_ATT
  integer (kind=c_int)                :: iIndex
  integer (kind=c_int)                :: iIndex2
  integer (kind=c_int)                :: iStat
  character (len=256)                 :: sBuf

  ! loop over variables
  do iIndex = 0, ubound(this%pNC_VAR,1)

    pNC_VAR => this%pNC_VAR(iIndex)

    ! for each variable, loop over the associated attributes
    do iIndex2 = 0, ubound(pNC_VAR%pNC_ATT,1)

      pNC_ATT => pNC_VAR%pNC_ATT(iIndex2)

        select case (pNC_ATT%iAttributeType)

          case (NC_DOUBLE)

            if (.not. allocated(pNC_ATT%dpValues) ) &
              call die("INTERNAL PROGRAMMING ERROR--attempt to use unallocated variable; " &
              //"attribute name: "//dquote(pNC_ATT%sAttributeName), &
              trim(__FILE__), __LINE__)

            call nf_put_attribute(this=this, &
                iVariableID=pNC_VAR%iVariableID, &
                sAttributeName=trim(pNC_ATT%sAttributeName)//c_null_char, &
                dpAttributeValue=pNC_ATT%dpValues)

          case (NC_INT)

            if (.not. allocated(pNC_ATT%iValues) ) &
              call die("INTERNAL PROGRAMMING ERROR--attempt to use unallocated variable; " &
              //"attribute name: "//dquote(pNC_ATT%sAttributeName), &
              trim(__FILE__), __LINE__)

            call nf_put_attribute(this=this, &
                iVariableID=pNC_VAR%iVariableID, &
                sAttributeName=trim(pNC_ATT%sAttributeName)//c_null_char, &
                iAttributeValue=pNC_ATT%iValues)

          case (NC_FLOAT)

            if (.not. allocated(pNC_ATT%rValues) ) &
              call die("INTERNAL PROGRAMMING ERROR--attempt to use unallocated variable; " &
              //"attribute name: "//dquote(pNC_ATT%sAttributeName), &
              trim(__FILE__), __LINE__)

            call nf_put_attribute(this=this, &
                iVariableID=pNC_VAR%iVariableID, &
                sAttributeName=trim(pNC_ATT%sAttributeName)//c_null_char, &
                rAttributeValue=pNC_ATT%rValues)

          case (NC_CHAR)

            sBuf = trim(pNC_ATT%slValues%cat() )

            call nf_put_attribute(this=this, &
                iVariableID=pNC_VAR%iVariableID, &
                sAttributeName=trim(pNC_ATT%sAttributeName)//c_null_char, &
                sAttributeValue= [ sBuf //c_null_char ] )

        end select

    enddo

  enddo

  ! now loop over global attributes
  do iIndex2 = lbound(this%pNC_ATT,1), ubound(this%pNC_ATT,1)

    pNC_ATT => this%pNC_ATT(iIndex2)

    select case (pNC_ATT%iAttributeType)

      case (NC_DOUBLE)

        call nf_put_attribute(this=this, &
            iVariableID=NC_GLOBAL, &
            sAttributeName=trim(pNC_ATT%sAttributeName)//c_null_char, &
            dpAttributeValue=pNC_ATT%dpValues )

      case (NC_INT)

        call nf_put_attribute(this=this, &
            iVariableID=NC_GLOBAL, &
            sAttributeName=trim(pNC_ATT%sAttributeName)//c_null_char, &
            iAttributeValue=pNC_ATT%iValues )

      case (NC_FLOAT)

        call nf_put_attribute(this=this, &
            iVariableID=NC_GLOBAL, &
            sAttributeName=trim(pNC_ATT%sAttributeName)//c_null_char, &
            rAttributeValue=pNC_ATT%rValues )

      case (NC_CHAR)

        call nf_put_attribute(this=this, &
            iVariableID=NC_GLOBAL, &
            sAttributeName=trim(pNC_ATT%sAttributeName)//c_null_char, &
            sAttributeValue=[trim(pNC_ATT%slValues%cat() )//c_null_char] )

    end select

  enddo

end subroutine nf_put_attributes

!--------------------------------------------------------------------------------------------------

subroutine netcdf_put_variable_array(this, iVariableID, iStart, iCount, iStride, &
   iValues, i2Values, rValues, dpValues)

  class (NETCDF4_FILE_T )             :: this
  integer (kind=c_int) :: iVariableID
  integer (kind=c_size_t), dimension(:) :: iStart
  integer (kind=c_size_t), dimension(:) :: iCount
  integer (kind=c_ptrdiff_t), dimension(:) :: iStride
  integer (kind=c_int), dimension(:,:), optional :: iValues
  integer (kind=c_short), dimension(:,:), optional :: i2Values
  real (kind=c_float), dimension(:,:), optional :: rValues
  real (kind=c_double), dimension(:,:), optional :: dpValues

  if (present(iValues) ) then

    call nf_trap(nc_put_vars_int(ncid=this%iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=iValues), &
                       __FILE__, __LINE__)

  elseif (present(i2Values) ) then

    call nf_trap(nc_put_vars_short(ncid=this%iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=i2Values), &
                       __FILE__, __LINE__)

  elseif (present(rValues) ) then

   call nf_trap(nc_put_vars_float(ncid=this%iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=rValues), &
                       __FILE__, __LINE__)

  elseif (present(dpValues) ) then

    call nf_trap(nc_put_vars_double(ncid=this%iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=dpValues), &
                       __FILE__, __LINE__)

  endif

end subroutine netcdf_put_variable_array

!--------------------------------------------------------------------------------------------------

subroutine netcdf_put_packed_variable_array(this, iVariableID, iStart, iCount, iStride, &
   lMask, iValues, iField, i2Values, i2Field, rValues, rField, dpValues, dpField)

  class (NETCDF4_FILE_T )             :: this
  integer (kind=c_int) :: iVariableID
  integer (kind=c_size_t), dimension(:)            :: iStart
  integer (kind=c_size_t), dimension(:)            :: iCount
  integer (kind=c_ptrdiff_t), dimension(:)         :: iStride
  logical (kind=c_bool), dimension(:,:)            :: lMask
  integer (kind=c_int), dimension(:), optional     :: iValues
  integer (kind=c_int), dimension(:,:), optional   :: iField
  integer (kind=c_short), dimension(:), optional   :: i2Values
  integer (kind=c_short), dimension(:,:), optional :: i2Field
  real (kind=c_float), dimension(:), optional      :: rValues
  real (kind=c_float), dimension(:,:), optional    :: rField
  real (kind=c_double), dimension(:), optional     :: dpValues
  real (kind=c_double), dimension(:,:), optional   :: dpField

  if (present(iValues) ) then

    call nf_trap(nc_put_vars_int(ncid=this%iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=unpack(iValues, lMask, iField)), &
                       __FILE__, __LINE__)

  elseif (present(i2Values) ) then

    call nf_trap(nc_put_vars_short(ncid=this%iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=unpack(i2Values, lMask, i2Field)), &
                       __FILE__, __LINE__)

  elseif (present(rValues) ) then

   call nf_trap(nc_put_vars_float(ncid=this%iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=unpack(rValues, lMask, rField)), &
                       __FILE__, __LINE__)

  elseif (present(dpValues) ) then

    call nf_trap(nc_put_vars_double(ncid=this%iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=unpack(dpValues, lMask, dpField)), &
                       __FILE__, __LINE__)

  endif

end subroutine netcdf_put_packed_variable_array

!--------------------------------------------------------------------------------------------------

subroutine netcdf_put_variable_vector(this, iVariableID, iStart, iCount, iStride, &
   iValues, i2Values, rValues, dpValues)

  class (NETCDF4_FILE_T )             :: this
  integer (kind=c_int) :: iVariableID
  integer (kind=c_size_t), dimension(:) :: iStart
  integer (kind=c_size_t), dimension(:) :: iCount
  integer (kind=c_ptrdiff_t), dimension(:) :: iStride
  integer (kind=c_int), dimension(:), optional :: iValues
  integer (kind=c_short), dimension(:), optional :: i2Values
  real (kind=c_float), dimension(:), optional :: rValues
  real (kind=c_double), dimension(:), optional :: dpValues

  if (present(iValues) ) then

    call nf_trap(nc_put_vars_int(ncid=this%iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=iValues), &
                       __FILE__, __LINE__)

  elseif (present(i2Values) ) then

    call nf_trap(nc_put_vars_short(ncid=this%iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=i2Values), &
                       __FILE__, __LINE__)

  elseif (present(rValues) ) then

    call nf_trap(nc_put_vars_float(ncid=this%iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=rValues), &
                       __FILE__, __LINE__)

  elseif (present(dpValues) ) then

    call nf_trap(nc_put_vars_double(ncid=this%iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=dpValues), &
                       __FILE__, __LINE__)

  endif

end subroutine netcdf_put_variable_vector

!--------------------------------------------------------------------------------------------------

end module netcdf4_support
