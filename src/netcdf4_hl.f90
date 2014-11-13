module netcdf4_hl

  use constants_and_conversions
  use exceptions
  use netcdf4_support
  implicit none

  private

  public :: NETCDF4_DIMENSION_T, NETCDF4_VARIABLE_T, NETCDF4_ATTRIBUTE_T, NETCDF4_FILE_T

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
    integer (kind=c_size_t)                  :: iStart                = 1_c_size_t
    integer (kind=c_size_t)                  :: iCount                = 1_c_size_t
    integer (kind=c_size_t)                  :: iStride               = 1_c_size_t
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

    procedure :: date_within_range
    generic   :: is_date_valid => date_within_range

    procedure :: deallocate_data_struct
    generic   :: deallocate => deallocate_data_struct

    procedure :: nullify_data_struct
    generic   :: nullify => nullify_data_struct

    procedure :: dump_cdl

    procedure :: define_iteration_bounds
    procedure :: define_start_count_stride

    procedure :: get_time_units
    procedure :: get_time_values

    procedure :: get_dimension_struct
    procedure :: get_variable_struct
    procedure :: get_variable_units 

    procedure :: dimension_index_to_dimension_id
    procedure :: dimension_id_to_dimension_index

    procedure :: variable_name_to_variable_id
    procedure :: variable_index_to_variable_id
    procedure :: variable_id_to_variable_index

    procedure :: return_dimension_index
    procedure :: return_variable_index

    procedure :: index_to_dayvalue
    procedure :: dayvalue_to_julian_day
    procedure :: julian_day_to_index
    procedure :: julian_day_to_index_adj

    procedure :: open_and_prepare_as_input
    procedure :: open_and_prepare_as_output

  end type NETCDF4_FILE_T

  integer (kind=c_int), public, parameter :: NC_LEFT  = 0
  integer (kind=c_int), public, parameter :: NC_RIGHT = 1
  integer (kind=c_int), public, parameter :: NC_TOP    = 0
  integer (kind=c_int), public, parameter :: NC_BOTTOM = 1

  integer (kind=c_int), parameter :: NC_FIRST = 0
  integer (kind=c_int), parameter :: NC_LAST  = 1
  integer (kind=c_int), parameter :: NC_BY    = 2

  integer (kind=c_int), parameter :: COLUMN = 1
  integer (kind=c_int), parameter :: ROW = 2

!
! A note on verbs:
!
! ==> The following *only* manipulate the module data structure
! if "to" is in the name: subroutine or function converts from one unit to another
! if "set" is in the name: subroutine alters the values of variables within the NCFILE data structures
! if "return" is in the name: subroutine queries the NCFILE data structure and returns the values found therein

! ==> The following read or write to/from an open NetCDF file
! if "put" is in the name: subroutine calls routines that WRITE to NetCDF
! if "get" is in the name: subroutine calls routines that READ from NetCDF
! if "define" is in the name: subroutine calls routines to DEFINE variables, dimensions, or attributes in the NetCDF file

contains

  subroutine open_and_prepare_as_input(this, sFilename, &
      lFlipHorizontal, lFlipVertical, &
      rX_Coord_AddOffset, rY_Coord_AddOffset, &
      sVariableOrder, sVarName_x, &
      sVarName_y, sVarName_value, sVarName_time, &
      tGridBounds, iLU)

    class (T_NETCDF4_FILE ), intent(inout)         :: this
    character (len=*), intent(in)                  :: sFilename
    logical (kind=c_bool), intent(in), optional    :: lFlipHorizontal
    logical (kind=c_bool), intent(in), optional    :: lFlipVertical
    character (len=*), intent(in), optional        :: sVariableOrder
    character (len=*)intent(in), optional          :: sVarName_x
    character (len=*), intent(in), optional        :: sVarName_y
    character (len=*), intent(in), optional        :: sVarName_value
    character (len=*), intent(in), optional        :: sVarName_time
    type (T_GRID_BOUNDS), intent(in), optional     :: tGridBounds
    integer (kind=c_int), intent(in), optional     :: iLU

    ! [ LOCALS ]
    type (T_NETCDF_VARIABLE), pointer  :: pNC_VAR => null()
    type (T_NETCDF_DIMENSION), pointer :: pNC_DIM => null()
    logical (kind=c_bool)              :: lFileOpen
    integer (kind=c_int), dimension(2) :: iColRow_ll, iColRow_ur, iColRow_lr, iColRow_ul
    integer (kind=c_int)               :: iColmin, iColmax, iRowmin, iRowmax
    integer (kind=c_int)               :: iIndex


    call nf_open_file( iNCID=this%iNCID, sFilename=sFilename, iFileformat=NC_FORMAT_NETCDF4 )

    call this%get_dimension_struct()
    call this%get_variable_struct()

    if (present(lFlipHorizontal) ) this%lFlipHorizontal = lFlipHorizontal
    if (present(lFlipVertical) ) this%lFlipVertical = lFlipVertical

    if (present(sVariableOrder) )  this%sVariableOrder = sVariableOrder

    if( present(iLU) ) then
      inquire (unit=iLU, opened=lFileOpen)
      if ( lFileOpen )  call nf_dump_cdl( this%iNCID, iLU)
    endif

    if (present(sVarName_x) ) then
      NCFILE%sVarName(NC_X) = sVarName_x
    else
      NCFILE%sVarName(NC_X) = "x"
    endif

    if (present(sVarName_y) ) then
      NCFILE%sVarName(NC_Y) = sVarName_y
    else
      NCFILE%sVarName(NC_Y) = "y"
    endif

    if (present(sVarName_z) ) then
      NCFILE%sVarName(NC_Z) = sVarName_z
    else
      NCFILE%sVarName(NC_Z) = "prcp"
    endif

    if (present(sVarName_time) ) then
      NCFILE%sVarName(NC_TIME) = sVarName_time
    else
      NCFILE%sVarName(NC_TIME) = "time"
    endif

    call nf_get_variable_id_and_type( NCFILE )

    NCFILE%dpFirstAndLastTimeValues = nf_get_first_and_last(NCFILE=NCFILE, &
        iVarIndex=NCFILE%iVarIndex(NC_TIME) )

    call nf_get_time_units(NCFILE=NCFILE)
    call nf_get_xyz_units(NCFILE=NCFILE)

    !> establish scale_factor and add_offset values, if present
    call nf_get_scale_and_offset(NCFILE=NCFILE)

    call nf_calculate_time_range(NCFILE)

    !> retrieve the X and Y coordinates from the NetCDF file...
    call nf_get_x_and_y(NCFILE)

    !> retrieve the time values as included in the NetCDF file
    call nf_get_time_vals(NCFILE)

    if (present(tGridBounds) ) then

      !> define a subset of the grid as the AOI
      !> need all four corner points since it is likely that
      !> the AOI rectangle is rotated relative to the base
      !> projection
      iColRow_ll = nf_coord_to_col_row(NCFILE=NCFILE, &
                                       rX=tGridBounds%rXll, &
                                       rY=tGridBounds%rYll)

      iColRow_lr = nf_coord_to_col_row(NCFILE=NCFILE, &
                                       rX=tGridBounds%rXlr, &
                                       rY=tGridBounds%rYlr)

      iColRow_ul = nf_coord_to_col_row(NCFILE=NCFILE, &
                                       rX=tGridBounds%rXul, &
                                       rY=tGridBounds%rYul)

      iColRow_ur = nf_coord_to_col_row(NCFILE=NCFILE, &
                                       rX=tGridBounds%rXur, &
                                       rY=tGridBounds%rYur)
  #ifdef DEBUG_PRINT
      write(*, fmt="(a,a,i6)") "Find correspondence between project bounds (in native projection) and row, col of dataset |", &
        trim(__FILE__), __LINE__
      write(*, fmt="(a)") "      column     row              X              Y"
      write(*, fmt="(a,i6,i6,a,f14.3,f14.3)") "LL: ", iColRow_ll(COLUMN), iColRow_ll(ROW), " <==> ", tGridBounds%rXll, tGridBounds%rYll
      write(*, fmt="(a,i6,i6,a,f14.3,f14.3)") "LR: ", iColRow_lr(COLUMN), iColRow_lr(ROW), " <==> ", tGridBounds%rXlr, tGridBounds%rYlr
      write(*, fmt="(a,i6,i6,a,f14.3,f14.3)") "UL: ", iColRow_ul(COLUMN), iColRow_ul(ROW), " <==> ", tGridBounds%rXul, tGridBounds%rYul
      write(*, fmt="(a,i6,i6,a,f14.3,f14.3)") "UR: ", iColRow_ur(COLUMN), iColRow_ur(ROW), " <==> ", tGridBounds%rXur, tGridBounds%rYur
  #endif

      NCFILE%iColBounds(NC_LEFT) = &
        max( min( iColRow_ul(COLUMN), iColRow_ur(COLUMN), iColRow_ll(COLUMN), iColRow_lr(COLUMN) ) - 4, &
                  lbound(NCFILE%rX_Coords,1) )

      NCFILE%iColBounds(NC_RIGHT) = &
        min( max( iColRow_ul(COLUMN), iColRow_ur(COLUMN), iColRow_ll(COLUMN), iColRow_lr(COLUMN) ) + 4, &
                  ubound(NCFILE%rX_Coords,1) )


        NCFILE%iRowBounds(NC_TOP) = &
          max( min( iColRow_ul(ROW), iColRow_ur(ROW), iColRow_ll(ROW), iColRow_lr(ROW) ) - 4, &
                    lbound(NCFILE%rY_Coords,1) )

        NCFILE%iRowBounds(NC_BOTTOM) = &
          min( max( iColRow_ul(ROW), iColRow_ur(ROW), iColRow_ll(ROW), iColRow_lr(ROW) ) + 4, &
                    ubound(NCFILE%rY_Coords,1) )

    else

      !> define the entire grid area as the AOI
      NCFILE%iColBounds(NC_LEFT) = lbound(NCFILE%rX_Coords,1)
      NCFILE%iColBounds(NC_RIGHT) = ubound(NCFILE%rX_Coords,1)

      NCFILE%iRowBounds(NC_TOP) = lbound(NCFILE%rY_Coords,1)
      NCFILE%iRowBounds(NC_BOTTOM) = ubound(NCFILE%rY_Coords,1)

    endif

    !> based on the subset of the NetCDF file as determined above, set the
    !> start, count, and stride parameters for use in all further data
    !> retrievals
    call nf_set_start_count_stride(NCFILE)

    !> establish the bounds to iterate over; this can enable horiz or vert flipping
    call nf_set_iteration_bounds(NCFILE)

    !> now that we have (possibly) created a subset, need to get the
    !> **NATIVE** coordinate bounds so that the intermediate grid file
    !> can be created
    call nf_return_native_coord_bounds(NCFILE)

  end subroutine open_and_prepare_as_input

  !----------------------------------------------------------------------

  !> Open a NetCDF file in order to save a local copy of the source NetCDF
  !! file.
  subroutine open_and_prepare_as_output(NCFILE, NCFILE_ARCHIVE, &
     iOriginMonth, iOriginDay, iOriginYear, iStartYear, iEndYear, &
     rX, rY)

    type (T_NETCDF4_FILE ) :: NCFILE
    type (T_NETCDF4_FILE ) :: NCFILE_ARCHIVE
    integer (kind=c_int) :: iOriginMonth
    integer (kind=c_int) :: iOriginDay
    integer (kind=c_int) :: iOriginYear
    integer (kind=c_int) :: iStartYear
    integer (kind=c_int) :: iEndYear
    real (kind=c_double), optional :: rX(:,:)
    real (kind=c_double), optional :: rY(:,:)
    
    ! [ LOCALS ]
    type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
    type (T_NETCDF_DIMENSION), pointer :: pNC_DIM
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iNumCols, iNumRows
    integer (kind=c_int) :: iMinCol, iMaxCol
    integer (kind=c_int) :: iMinRow, iMaxRow
    real (kind=c_double), dimension(:), allocatable :: rX_vec, rY_vec
    character (len=10) :: sOriginText
    character (len=256) :: sFilename

    write(sOriginText, fmt="(i4.4,'-',i2.2,'-',i2.2)") iOriginYear, &
      iOriginMonth, iOriginDay

    iMaxRow = maxval(NCFILE%iRowBounds)
    iMinRow = minval(NCFILE%iRowBounds)
    iMaxCol = maxval(NCFILE%iColBounds)
    iMinCol = minval(NCFILE%iColBounds)

    iNumRows = iMaxRow - iMinRow + 1
    iNumCols = iMaxCol - iMinCol + 1

    allocate(rX_vec(iNumCols))
    allocate(rY_vec(iNumRows))
    rX_vec = NCFILE%rX_Coords(iMinCol:iMaxCol)
    rY_vec = NCFILE%rY_Coords(iMinRow:iMaxRow)

    sFilename = trim(NCFILE%sVarName(NC_Z))//"_"//trim(asCharacter(iStartYear)) &
      //"_"//trim(asCharacter(iEndYear))//"__" &
      //trim(asCharacter(iNumRows)) &
       //"_by_"//trim(asCharacter(iNumCols))//".nc"

    call nf_create(NCFILE=NCFILE_ARCHIVE, sFilename=trim(sFilename) )

    !> set dimension values in the NCFILE struct
    call nf_set_standard_dimensions(NCFILE=NCFILE_ARCHIVE, &
                         iNX=iNumCols, &
                         iNY=iNumRows)

    NCFILE_ARCHIVE%sVarUnits(NC_X) =   NCFILE%sVarUnits(NC_X)
    NCFILE_ARCHIVE%sVarUnits(NC_Y) =   NCFILE%sVarUnits(NC_Y)
    NCFILE_ARCHIVE%sVarUnits(NC_Z) =   NCFILE%sVarUnits(NC_Z)

    !> transfer dimension values to NetCDF file
    call nf_define_dimensions( NCFILE=NCFILE_ARCHIVE )

    !> set variable values in the NCFILE struct
    call nf_set_standard_variables(NCFILE=NCFILE_ARCHIVE, &
         sVarName_z = trim(NCFILE%sVarName(NC_Z)) )

    !> transfer variable values to NetCDF file
    call nf_define_variables(NCFILE=NCFILE_ARCHIVE)

    call nf_get_variable_id_and_type( NCFILE=NCFILE_ARCHIVE )

    call nf_set_standard_attributes(NCFILE=NCFILE_ARCHIVE, &
      sOriginText=sOriginText)

    call nf_set_global_attributes(NCFILE=NCFILE_ARCHIVE, &
       sDataType=trim(NCFILE%sVarName(NC_Z)), &
       sSourceFile=trim(NCFILE%sFilename))

    call nf_put_attributes(NCFILE=NCFILE_ARCHIVE)

    !> enable a low level of data compression for the
    !> variable of interest
    call nf_define_deflate(iNCID=NCFILE_ARCHIVE%iNCID, &
       iVarID=NCFILE_ARCHIVE%iVarID(NC_Z), &
       iShuffle=NC_SHUFFLE_YES, &
       iDeflate=NC_DEFLATE_YES, &
       iDeflate_level=2 )

    call nf_enddef(NCFILE=NCFILE_ARCHIVE)

    call nf_put_x_and_y(NCFILE=NCFILE_ARCHIVE, &
         dpX=NCFILE%rX_Coords(iMinCol:iMaxCol), &
         dpY=NCFILE%rY_Coords(iMinRow:iMaxRow) )
  !       dpX=rX, &
  !       dpY=rY )

  !  call netcdf_close_file(NCFILE_ARCHIVE)

  end subroutine open_and_prepare_as_output  

!--------------------------------------------------------------------------------------------------

!> Test whether a given Julian Day is within the range provided in the NetCDF file.
!!
!! @param[inout] this Object of class NETCDF4_FILE_T).
!! @param[in]    iJulianDay Julian Day for the date of interest.
!! @retval       lWithinRange TRUE if iJulianDate is within the date range of dates provided in the 
!!               NetCDF file.

  function date_within_range( this, iJulianDay )  result( lWithinRange )

    class (NETCDF4_FILE_T )             :: this
    integer (kind=c_int), intent(in)    :: iJulianDay
    logical (kind=c_bool)               :: lWithinRange

    if ( iJulianDay >= this%iFirstDayJD &
        .and. iJulianDay <= this%iLastDayJD ) then

      lWithinRange = lTRUE

    else

      lWithinRange = lFALSE

    endif

  end function date_within_range

!--------------------------------------------------------------------------------------------------

!> This was so clear the other day. Basically, I think we need
!> two functions to convert from index to timeval, and timeval to JD;
!> note that timeval refers to the number of days from the origin
!> of the NetCDF file

!> return the day value (number of days since origin

  function julian_day_to_index(this, rJulianDay)  result (iIndex)

    class (NETCDF4_FILE_T )             :: this
    real (kind=c_double), intent(in)    :: rJulianDay
    integer (kind=c_int)                :: iIndex

    iIndex = aint(rJulianDay) - this%iFirstDayJD

  end function julian_day_to_index

!--------------------------------------------------------------------------------------------------

  function index_to_dayvalue(this, iIndex)   result(rDayValue)

    class (NETCDF4_FILE_T )             :: this
    integer (kind=c_int), intent(in)    :: iIndex
    real (kind=c_double)                :: rDayValue

    if (iIndex < lbound(this%rDateTimeValues, 1) .or. iIndex > ubound(this%rDateTimeValues, 1) ) &
      call die( "Dimension out of bounds", trim(__FILE__), __LINE__)
    
    rDayValue = this%rDateTimeValues(iIndex)

  end function index_to_dayvalue

!--------------------------------------------------------------------------------------------------

  function dayvalue_to_julian_day(this, rDayValue)   result(rJulianDay)

    class (NETCDF4_FILE_T )             :: this
    real (kind=c_double), intent(in)    :: rDayValue
    real (kind=c_double)                :: rJulianDay

    rJulianDahly = real(this%iOriginJD, kind=c_double) &
      + real(this%iOriginHH, kind=c_double) / 24_c_double &
      + real(this%iOriginMM, kind=c_double) / 1440_c_double &
      + real(this%iOriginSS, kind=c_double) / 86400_c_double &
      + rDayValue

  end function dayvalue_to_julian_day

!--------------------------------------------------------------------------------------------------

  function julian_day_to_index_adj( this, rJulianDay )  result(iStart)

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

  end function julian_day_to_index_adj

!--------------------------------------------------------------------------------------------------

  function variable_name_to_variable_id( this, sVariableName )    result( iVariableID )

    class (NETCDF4_FILE_T )                   :: this
    character (len=*), intent(in)             :: sVariableName
    integer (kind=c_int)                      :: iVariableID

    ! [ LOCALS ]
    integer (kind=c_int)               :: iIndex
    type (NETCDF4_VARIABLE_T), pointer :: pNC_VAR

    iVariableID = -9999

    if ( .not. associated( this%pNC_VAR ) )  &
      call die("INTERNAL PROGRAMMING ERROR--attempted use of null pointer", &
        __FILE__, __LINE__ )

    do iIndex=lbound(this%pNC_VAR,1), ubound(this%pNC_VAR,1)

      pNC_VAR => this%pNC_VAR(iIndex)

      if( sVariableName .strequal. pNC_VAR%sVariableName ) then

        iVariableID = iIndex
        exit

      endif

    enddo

  end function variable_name_to_variable_id

!--------------------------------------------------------------------------------------------------

  function variable_index_to_variable_id( this, iVariableIndex)   result(iVariableID)

    class (NETCDF4_FILE_T )             :: this
    integer (kind=c_int), intent(in)    :: iVariableIndex
    integer (kind=c_int)                :: iVariableID

    ! [ LOCALS ]
    type (NETCDF4_VARIABLE_T), pointer :: pNC_VAR

    if ( .not. associated( this%pNC_VAR ) )  &
      call die("INTERNAL PROGRAMMING ERROR--attempted use of null pointer", &
        __FILE__, __LINE__ )

    if ( iVariableIndex < lbound( this%pNC_VAR, 1) .or. iVariableIndex > ubound( this%pNC_VAR, 1) )  &
      call die( "INTERNAL PROGRAMMING ERROR--index out of bounds: "//asCharacter(iVariableIndex), &
        __FILE__, __LINE__ )

    pNC_VAR => this%pNC_VAR(iVariableIndex)

    iVariableID = pNC_VAR%iVariableID

  end function variable_index_to_variable_id

!--------------------------------------------------------------------------------------------------

  function dimension_index_to_dimension_id( this, iDimensionIndex)   result(iDimensionID)

    class (NETCDF4_FILE_T )             :: this
    integer (kind=c_int), intent(in)    :: iDimensionIndex
    integer (kind=c_int)                :: iDimensionID

    ! [ LOCALS ]
    type (NETCDF4_DIMENSION_T), pointer :: pNC_DIM

    if ( .not. associated( this%pNC_DIM ) )  &
      call die("INTERNAL PROGRAMMING ERROR--attempted use of null pointer", &
        __FILE__, __LINE__ )

    if ( iDimensionIndex < lbound( this%pNC_DIM, 1) .or. iDimensionIndex > ubound( this%pNC_DIM, 1) )  &
      call die( "INTERNAL PROGRAMMING ERROR--index out of bounds: "//asCharacter(iDimensionIndex), &
        __FILE__, __LINE__ )

    pNC_DIM => this%pNC_DIM(iDimensionIndex)

    iDimensionID = pNC_DIM%iDimensionID

  end function dimension_index_to_dimension_id

!--------------------------------------------------------------------------------------------------

  function variable_id_to_variable_index( this, iVariableID)   result(iVariableIndex)

    class (NETCDF4_FILE_T )             :: this
     integer (kind=c_int), intent(in)   :: iVariableID
     integer (kind=c_int)               :: iVariableIndex

     ! [ LOCALS ]
     type (NETCDF4_VARIABLE_T), pointer :: pNC_VAR
     integer (kind=c_int)               :: iIndex
     logical (kind=c_bool)              :: lFound

    lFound = lFALSE

    if ( .not. associated( this%pNC_VAR ) )  &
      call die("INTERNAL PROGRAMMING ERROR--attempted use of null pointer", &
        __FILE__, __LINE__ )

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

  end function variable_id_to_variable_index

!--------------------------------------------------------------------------------------------------

  function return_attribute_value_string( this, iVariableIndex, sAttributeName)   result(sAttributeValue)

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

  end function return_attribute_value_string

!--------------------------------------------------------------------------------------------------

  function dimension_id_to_dimension_index( this, iDimensionID)   result(iDimensionIndex)

    class (NETCDF4_FILE_T )             :: this
     integer (kind=c_int), intent(in)   :: iDimensionID
     integer (kind=c_int)               :: iDimensionIndex

     ! [ LOCALS ]
     type (NETCDF4_DIMENSION_T), pointer  :: pNC_DIM
     integer (kind=c_int)                 :: iIndex
     logical (kind=c_bool)                :: lFound

    lFound = lFALSE

    if ( .not. associated( this%pNC_DIM ) )  &
      call die("INTERNAL PROGRAMMING ERROR--attempted use of null pointer", &
        __FILE__, __LINE__ )

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

  end function dimension_id_to_dimension_index

!--------------------------------------------------------------------------------------------------

  function return_dimension_size( this, iDimensionID)   result(iDimensionSize)

    class (NETCDF4_FILE_T )             :: this
     integer (kind=c_int), intent(in)   :: iDimensionID
     integer (kind=c_size_t)            :: iDimensionSize

    ! [ LOCALS ]
    type (NETCDF4_DIMENSION_T), pointer   :: pNC_DIM
    integer (kind=c_int)                  :: iIndex
    logical (kind=c_bool)                 :: lFound

    lFound = lFALSE

    if ( .not. associated( this%pNC_DIM ) )  &
      call die("INTERNAL PROGRAMMING ERROR--attempted use of null pointer", &
        __FILE__, __LINE__ )

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

  end function return_dimension_size

!--------------------------------------------------------------------------------------------------

  subroutine set_iteration_bounds(this)

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

  end subroutine set_iteration_bounds

!--------------------------------------------------------------------------------------------------

  subroutine set_start_count_stride(this, pNC_VAR)

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


  end subroutine set_start_count_stride

!--------------------------------------------------------------------------------------------------

  function coord_to_col_row(this, rX, rY)   result(iColRow)

    class (NETCDF4_FILE_T ), intent(inout)         :: this
    real (kind=c_double), intent(in)               :: rX
    real (kind=c_double), intent(in)               :: rY
    integer (kind=c_size_t)                        :: iColRow(2)


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

  end function coord_to_col_row

!--------------------------------------------------------------------------------------------------

  subroutine put_x_and_y(this, dX, dY)

    class (NETCDF4_FILE_T) :: this
    real (kind=c_double), dimension(:) :: dX
    real (kind=c_double), dimension(:) :: dY

    ! [ LOCALS ]
    integer (kind=c_size_t) :: iLength
    real (kind=c_double), dimension(:), allocatable :: rX, rY

    iLength = int(size(dpX, 1), kind=c_size_t)

    call nf_put_variable_vector(this=this%iNCID, &
                     iVariableID=this%pNC_VAR_X%iVariableID, &
                     iStart=[0_c_size_t], &
                     iCount=[iLength], &
                     iStride=[1_c_ptrdiff_t], &
                     dpValues=dX)

    iLength = int(size(dpY, 1), kind=c_size_t)

    call nf_put_variable_vector(this=this%iNCID, &
                     iVariableID=this%pNC_VAR_Y%iVariableID, &
                     iStart=[0_c_size_t], &
                     iCount=[iLength], &
                     iStride=[1_c_ptrdiff_t], &
                     dpValues=dY)

  end subroutine put_x_and_y

!--------------------------------------------------------------------------------------------------

  subroutine put_lat_and_lon(this, dLat, dLon)

    class (NETCDF4_FILE_T) :: this
    real (kind=c_double), dimension(:,:) :: dLat
    real (kind=c_double), dimension(:,:) :: dLon

    ! [ LOCALS ]
    integer (kind=c_size_t) :: iNX, iNY

    iNX = int( size(dLat, 2), kind=c_size_t)
    iNY = int( size(dLat, 1), kind=c_size_t)

    call nf_put_variable_array(this=this%iNCID, &
                     iVariableID=this%pNC_VAR_LAT%iVariableID, &
                     iStart=[0_c_size_t, 0_c_size_t], &
                     iCount=[ iNY, iNX ], &
                     iStride=[1_c_ptrdiff_t,1_c_ptrdiff_t], &
                     dValues=dLat)


    call nf_put_variable_array(this=this%iNCID, &
                     iVariableID=this%pNC_VAR_LON%iVariableID, &
                     iStart=[0_c_size_t, 0_c_size_t], &
                     iCount=[ iNY, iNX ],&
                     iStride=[1_c_ptrdiff_t,1_c_ptrdiff_t], &
                     dValues=dLon)

  end subroutine put_lat_and_lon

!--------------------------------------------------------------------------------------------------

  subroutine calculate_native_coord_bounds(this)

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

  end subroutine calculate_native_coord_bounds

!--------------------------------------------------------------------------------------------------

!> Get time values from NetCDF file.
!!
!! This routine obtains a vector of all time values from the NetCDF file associated with NCID.
!!
!! @param[inout] this Object of class NETCDF4_FILE_T

  subroutine get_time_values(this)

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

    call nf_get_variable_vector_double(this=this,          &
         iVariableID=this%pNC_VAR_TIME%iVariableID,        &
         iNC_Start=0_c_size_t,                             &
         iNC_Count=this%pNC_DIM_TIME%iDimensionSize,       &
         iNC_Stride=1_c_size_t,                            &
         dNC_Vars=this%rDateTimeValues)

  end subroutine get_time_values

!--------------------------------------------------------------------------------------------------

!> Read X and Y variable vectors from the NetCDF file.
!!
!! Only 1-D arrays (vectors) of X and Y variables are supported at this time.
!!
!! @param[inout] this Object of class NETCDF4_FILE_T.
!!
!! @pre Before calling this routine, pointers to the X and Y variables must be targeted at
!!      valid variables.

  subroutine get_x_and_y(this)

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
         dNC_Vars=this%rX_Coords)

    call nf_get_variable_vector_double(this=this, &
         iVariableID=this%pNC_VAR_Y%iVariableID, &
         iNC_Start=0_c_size_t, &
         iNC_Count=this%pNC_DIM_Y%iDimensionSize, &
         iNC_Stride=1_c_size_t, &
         dNC_Vars=this%rY_Coords)

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

  end subroutine get_x_and_y

!--------------------------------------------------------------------------------------------------

  subroutine nullify_data_structs( this )

    class (NETCDF4_FILE_T ), intent(inout)       :: this

    ! [ LOCALS ]

    this%pNC_VAR => null()
    this%pNC_ATT => null()
    this%pNC_DIM => null()

  end subroutine nullify_data_structs

!--------------------------------------------------------------------------------------------------

  subroutine deallocate_data_structs( this )

    class (NETCDF4_FILE_T ), intent(inout)       :: this

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

  end subroutine deallocate_data_structs

!--------------------------------------------------------------------------------------------------

!> Iterate over all dimension elements stored in a READONLY NetCDF file.
!!
!! The number of dimensions, the dimension names, and the dimension sizes
!! are determined by querying the NetCDF file and are stored in the NETCDF4_DIMENSION_T structure.
!!
!! @param[inout] this Object of class NETCDF4_FILE_T.
!! 
!! @note This subroutine is designed to be used when dealing with a READONLY file. 

  subroutine get_dimensions( this )

    class (NETCDF4_FILE_T), intent(inout)        :: this

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

  end subroutine get_dimensions

!--------------------------------------------------------------------------------------------------

!> Iterate over all variable elements stored in a READONLY NetCDF file.
!!
!! The number of variables, variable names, and variable sizes
!! are determined by querying the NetCDF file and are stored in the NETCDF4_VARIABLE_T structure.
!!
!! @param[inout] this Object of class NETCDF4_FILE_T.
!! 
!! @note This subroutine is designed to be used when dealing with a READONLY file. 

  subroutine get_variables( this )

    class (NETCDF4_FILE_T), intent(inout)        :: this

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

    ! find out how many VARIABLES the NetCDF file contains
    call nf_trap( nc_inq_nvars( ncid=this%iNCID,         &
           nvarsp=iNumberOfVariables),                   &
           __FILE__, __LINE__ )

    ! deallocate this%pNC_VAR data if already in use
    if (associated(this%pNC_VAR) ) deallocate(this%pNC_VAR, stat=iStat)
    call assert(iStat == 0, "Could not deallocate memory for NC_VAR member in NC_FILE defined type", &
      trim(__FILE__), __LINE__)

    ! make space in the this%pNC_VAR data structure for all variables in file
    allocate(this%pNC_VAR( 0 : iNumberOfVariables-1), stat=iStat )
    call assert(iStat == 0, "Could not allocate memory for NC_VAR member in NC_FILE defined type", &
      trim(__FILE__), __LINE__)

    ! iterate over all variables in file, collecting basic information about each
    do iIndex = lbound( this%pNC_VAR,1), ubound( this%pNC_VAR,1)

      pNC_VAR => this%pNC_VAR(iIndex)

      ! make actual call via C API to obtain variable information
      call nf_trap( nc_inq_var(ncid=this%iNCID,          &
             varid=iIndex,                               &
             name=sVariableName,                         &
             xtypep=pNC_VAR%iVariableType,               &
             ndimsp=pNC_VAR%iNumberOfDimensions,         &
             dimidsp=pNC_VAR%iDimensionID,               &
             nattsp=pNC_VAR%iNumberOfAttributes ),       & 
             __FILE__, __LINE__ )

      pNC_VAR%iVariableID = iIndex
      pNC_VAR%sVariableName = c_to_fortran_string(sVariableName)

      ! each variable may have many attributes. if so, find them.
      if( pNC_VAR%iNumberOfAttributes > 0 ) then

        if ( associated( pNC_VAR%pNC_ATT ) )    deallocate(pNC_VAR%pNC_ATT, stat=iStat)

        call assert(iStat == 0, "Could not deallocate memory for NC_ATT member within NC_VAR in NC_FILE defined type", &
          trim(__FILE__), __LINE__)

        allocate( pNC_VAR%pNC_ATT( 0:pNC_VAR%iNumberOfAttributes - 1 ), stat = iStat)

        call assert(iStat == 0, "Could not allocate memory for NC_ATT member within NC_VAR in NC_FILE defined type", &
          trim(__FILE__), __LINE__)

        do iIndex2=lbound(pNC_VAR%pNC_ATT,1), ubound(pNC_VAR%pNC_ATT,1)

          pNC_ATT => pNC_VAR%pNC_ATT(iIndex2)

          call this%get_attribute( pNC_ATT=pNC_ATT, iVariableID=iIndex, iAttNum=iIndex2 )

        enddo

      endif

    enddo

    ! now find out how many GLOBAL attributes the file contains
    call nf_trap( nc_inq_natts(ncid=this%iNCID,       &
           ngattsp=iNumberOfAttributes),              &
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

      call nf_get_attribute_struct( this=this, pNC_ATT=pNC_ATT, &
        iVariableID=NC_GLOBAL, iAttNum=iIndex )

    enddo

  end subroutine get_variables

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

  subroutine get_attribute( this, pNC_ATT, iVariableID, iAttNum )

    class (NETCDF4_FILE_T), intent(inout)        :: this
    type (NETCDF4_ATTRIBUTE_T), pointer          :: pNC_ATT
    integer (kind=c_int), intent(in)             :: iVariableID
    integer (kind=c_int), intent(in)             :: iAttNum

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

    pNC_ATT%sAttributeName = c_to_fortran_string( sAttributeName )

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

        do iIndex=lbound(sAttributeValues,1), ubound(sAttributeValues,1)
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

        do iIndex=lbound(pNC_ATT%i2Values,1), ubound(pNC_ATT%i2Values,1)
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

        do iIndex=lbound(pNC_ATT%iValues,1), ubound(pNC_ATT%iValues,1)
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

        do iIndex=lbound(pNC_ATT%dpValues,1), ubound(pNC_ATT%dpValues,1)
          call pNC_ATT%slValues%append( pNC_ATT%dpValues(iIndex) )
        enddo

      case default

    end select

  end subroutine get_attribute

!--------------------------------------------------------------------------------------------------

  function update_time_starting_index(this, iJulianDay)  result(lDateTimeFound)

    class (NETCDF4_FILE_T), intent(inout)        :: this
    integer (kind=c_int), intent(in)             :: iJulianDay
    logical (kind=c_bool)                        :: lDateTimeFound

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

  end function update_time_starting_index

!--------------------------------------------------------------------------------------------------

  subroutine put_attributes(this)

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
                  dpAttributeValues=pNC_ATT%dpValues)

            case (NC_INT)

              if (.not. allocated(pNC_ATT%iValues) ) &
                call die("INTERNAL PROGRAMMING ERROR--attempt to use unallocated variable; " &
                //"attribute name: "//dquote(pNC_ATT%sAttributeName), &
                trim(__FILE__), __LINE__)

              call nf_put_attribute(this=this, &
                  iVariableID=pNC_VAR%iVariableID, &
                  sAttributeName=trim(pNC_ATT%sAttributeName)//c_null_char, &
                  iAttributeValues=pNC_ATT%iValues)

            case (NC_FLOAT)

              if (.not. allocated(pNC_ATT%rValues) ) &
                call die("INTERNAL PROGRAMMING ERROR--attempt to use unallocated variable; " &
                //"attribute name: "//dquote(pNC_ATT%sAttributeName), &
                trim(__FILE__), __LINE__)

              call nf_put_attribute(this=this, &
                  iVariableID=pNC_VAR%iVariableID, &
                  sAttributeName=trim(pNC_ATT%sAttributeName)//c_null_char, &
                  rAttributeValues=pNC_ATT%rValues)

            case (NC_CHAR)

              call nf_put_attribute(this=this, &
                  iVariableID=pNC_VAR%iVariableID, &
                  sAttributeName=trim(pNC_ATT%sAttributeName)//c_null_char, &
                  slAttributeValues= pNC_ATT%slValues )

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
              dpAttributeValues=pNC_ATT%dpValues )

        case (NC_INT)

          call nf_put_attribute(this=this, &
              iVariableID=NC_GLOBAL, &
              sAttributeName=trim(pNC_ATT%sAttributeName)//c_null_char, &
              iAttributeValues=pNC_ATT%iValues )

        case (NC_FLOAT)

          call nf_put_attribute(this=this, &
              iVariableID=NC_GLOBAL, &
              sAttributeName=trim(pNC_ATT%sAttributeName)//c_null_char, &
              rAttributeValues=pNC_ATT%fValues )

        case (NC_CHAR)

          call nf_put_attribute(this=this, &
              iVariableID=NC_GLOBAL, &
              sAttributeName=trim(pNC_ATT%sAttributeName)//c_null_char, &
              slAttributeValues=pNC_ATT%slValues )

      end select

    enddo

  end subroutine put_attributes

!--------------------------------------------------------------------------------------------------

  subroutine put_dimensions( this )

    class (NETCDF4_FILE_T) :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    type (NETCDF4_DIMENSION_T), pointer :: pNC_DIM

    do iIndex = lbound( this%pNC_DIM, 1 ), ubound( this%pNC_DIM, 1 )

      pNC_DIM => this%pNC_DIM( iIndex )

      call nf_trap( nc_def_dim( ncid=this%iNCID,                       &
             name=fortran_to_c_string( pNC_DIM%sDimensionName ) ),     &
             lenv=pNC_DIM%iDimensionSize,                              &
             dimidp=pNC_DIM%iDimensionID),                             &
             __FILE__, __LINE__ )

    enddo

  end subroutine put_dimensions

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

        call nf_trap( nc_def_var(ncid=this%iNCID,                          &
               name=fortran_to_c_string(pNC_VAR%sVariableName),            &
               xtype=pNC_VAR%iVariableType,                                &
               ndims=pNC_VAR%iNumberOfDimensions,                          &
               dimidsp=pNC_VAR%iDimensionID,                               &
               varidp=pNC_VAR%iVariableID),                                &
               __FILE__, __LINE__)

      endif

    enddo

  end subroutine nf_put_variables

!--------------------------------------------------------------------------------------------------

  subroutine get_variable_slice_short(this, pNC_VAR, rValues)

    class (NETCDF4_FILE_T)                         :: this
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

  end subroutine get_variable_slice_short

!--------------------------------------------------------------------------------------------------

  subroutine get_variable_slice_int(this, pNC_VAR, rValues)

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

  end subroutine get_variable_slice_int

!--------------------------------------------------------------------------------------------------

  subroutine get_variable_slice_float(this, pNC_VAR, rValues)

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
            rNC_Vars=rTemp)

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
            rNC_Vars=rTemp)

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

  end subroutine get_variable_slice_float

!--------------------------------------------------------------------------------------------------

  subroutine dump_cdl(this, iLU)

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


  end subroutine dump_cdl

!--------------------------------------------------------------------------------------------------

  function get_first_and_last(this, pNC_VAR)  result(dpValues)

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

  end function get_first_and_last

!--------------------------------------------------------------------------------------------------

  subroutine calculate_time_range(this)

    class (NETCDF4_FILE_T), intent(inout) :: this

    this%iOriginJD = julian_day(this%iOriginYear, &
      this%iOriginMonth, this%iOriginDay)

    this%iFirstDayJD = this%iOriginJD + this%dpFirstAndLastTimeValues(NC_FIRST)
    this%iLastDayJD = this%iOriginJD + this%dpFirstAndLastTimeValues(NC_LAST)

  end subroutine calculate_time_range

!--------------------------------------------------------------------------------------------------

  subroutine get_time_units(this)

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

  end subroutine get_time_units

!--------------------------------------------------------------------------------------------------

  subroutine get_variable_units(this)

    class (NETCDF4_FILE_T), intent(inout) :: this

    ! [ LOCALS ]
    type (NETCDF4_VARIABLE_T), pointer :: pNC_VAR
    integer (kind=c_int)               :: iIndex, iIndex2
    logical (kind=c_bool)              :: lFound
    integer (kind=c_int)               :: iStat

    do iIndex=lbound(this%pNC_VAR, 1), ubound(this%pNC_VAR, 1)

      call assert(this%pNC_VAR(iIndex)%iVariableID >= 0, "INTERNAL PROGRAMMING ERROR -- " &
      //"nc_get_XYZ_units must be called only after a call is made to ~" &
      //"nf_get_variable_ids", trim(__FILE__), __LINE__)

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

  end subroutine get_variable_units

!--------------------------------------------------------------------------------------------------

  subroutine get_scale_and_offset(this, pNC_VAR)

    class (NETCDF4_FILE_T), intent(inout) :: this
    type (NETCDF4_VARIABLE_T), pointer :: pNC_VAR

    ! [ LOCALS ]
    integer (kind=c_int)    :: iIndex
    logical (kind=c_bool)   :: lFound
    integer (kind=c_int)    :: iStat
    character (len=32)      :: sBuf

    call assert( associated( pNC_VAR ), "INTERNAL PROGRAMMING ERROR--attempted use of null pointer", &
      __FILE__, __LINE__ )

    call assert( pNC_VAR%iVariableID >= 0, "INTERNAL PROGRAMMING ERROR -- " &
      //"nc_get_scale_and_offset must be called only after a call is made to ~" &
      //"nf_get_variable_ids", trim(__FILE__), __LINE__ )

    associate ( var => pNC_VAR )

      lFound = lFALSE
      
      !> loop over all attributes of the selected variable; attempt to find "scale_factor" attribute
      do iIndex=0, var%iNumberOfAttributes - 1

        if ( var%pNC_ATT(iIndex)%sAttributeName .strequal. "scale_factor" ) then
          lFound = lTRUE
          exit
        endif

      enddo

      if (lFound) then
        sBuf = trim( var%pNC_ATT(iIndex)%slValues%get(1) )
        read(sBuf,*) var%rScaleFactor
      endif

      !> Now repeat the process for "add_offset" attribute
      lFound = lFALSE

      !> loop over all attributes of the selected variable; attemmpt to find "add_offset" attribute
      do iIndex=0, var%iNumberOfAttributes - 1

        if ( var%pNC_ATT(iIndex)%sAttributeName .strequal. "add_offset" ) then
          lFound = lTRUE
          exit
        endif

      enddo

      if (lFound) then
        sBuf = trim(var%pNC_ATT(iIndex)%slValues%get(1) )
        read(sBuf,*) var%rAddOffset
      endif

    end associate


  end subroutine get_scale_and_offset

!--------------------------------------------------------------------------------------------------

  function return_variable_pointer( this, sVariableName )    result( pNC_VAR )

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

  end function return_variable_pointer
  
!--------------------------------------------------------------------------------------------------

  function return_index_double(rValues, rTargetValue)  result(iIndex)

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

  end function return_index_double

!--------------------------------------------------------------------------------------------------

end module netcdf4_hl