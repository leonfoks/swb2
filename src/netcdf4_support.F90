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

  integer(kind=c_int), parameter, public :: NC_FILL_CHAR    = 0
  integer(kind=c_int), parameter, public :: NC_FILL_BYTE    = -127
  integer(kind=c_int), parameter, public :: NC_FILL_SHORT   = -32767
  integer(kind=c_int), parameter, public :: NC_FILL_INT     = -2147483647
  real(kind=c_float),  parameter, public :: NC_FILL_FLOAT   = 9.9692099683868690e+36
  real(kind=c_double), parameter, public :: NC_FILL_DOUBLE  = 9.9692099683868690d+36

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
  
contains


!--------------------------------------------------------------------------------------------------
!> Open a NetCDF file in READONLY mode and determine the format.
!!
!! @param[inout] this Object of class NETCDF4_FILE_T.
!! @param[in] sFilename URL, relative, or absolute path and filename of NetCDF file.

subroutine nf_open_file_readonly( iNCID, sFilename )

  integer (kind=c_int), intent(inout)  :: iNCID
  character (len=*), intent(in)        :: sFilename

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

subroutine nf_trap( iResultCode, sFilename, iLineNumber )

  integer (kind=c_int), intent(in)            :: iResultCode
  character (len=*), intent(in), optional     :: sFilename
  integer (kind=c_int), intent(in), optional  :: iLineNumber

  ! [ LOCALS ]
  type (c_ptr)          :: cpResult
  character (len=256)   :: sTextString
  character (len=256)   :: sFile
  integer (kind=c_int)  :: iLine

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
      //asCharacter(iResultCode), lEcho=lTRUE )

    call assert(lFALSE, "SWB is stopping due to a problem reading or writing" &
      //" a NetCDF file", trim(sFile), iLine)

  endif

end subroutine nf_trap

!--------------------------------------------------------------------------------------------------

subroutine nf_close_file( iNCID, sFilename )

  integer (kind=c_int), intent(in)   :: iNCID
  character (len=*), intent(in)      :: sFilename

  call LOGS%write("Closing NetCDF file with name: "//dquote( sFilename ) )
  call nf_trap( nc_close( iNCID ), __FILE__, __LINE__ )

end subroutine nf_close_file

!--------------------------------------------------------------------------------------------------




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
  integer (kind=c_int)               :: iIndex, iIndex2
  logical (kind=c_bool)              :: lFound
  integer (kind=c_int)               :: iStat

  do iIndex=lbound(this%pNC_VAR, 1), ubound(this%pNC_VAR, 1)

    call assert(this%pNC_VAR(iIndex)%iVariableID >= 0, "INTERNAL PROGRAMMING ERROR -- " &
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

  call assert( associated( pNC_VAR ), "INTERNAL PROGRAMMING ERROR--attempted use of null pointer", &
    __FILE__, __LINE__ )

  call assert( pNC_VAR%iVariableID >= 0, "INTERNAL PROGRAMMING ERROR -- " &
    //"nc_get_scale_and_offset must be called only after a call is made to ~" &
    //"netcdf_get_variable_ids", trim(__FILE__), __LINE__ )

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
  slAttributeValues, iAttributeValues, rAttributeValues, dpAttributeValues)

  class (NETCDF4_FILE_T )             :: this
  integer (kind=c_int)                :: iVariableID
  character (len=*)                   :: sAttributeName
  type (STRING_LIST_T), optional      :: slAttributeValues
  integer (kind=c_int), optional      :: iAttributeValues(:)
  real (kind=c_float), optional       :: rAttributeValues(:)
  real (kind=c_double), optional      :: dpAttributeValues(:)

  ! [ LOCALS ]
  integer (kind=c_size_t)         :: iNumberOfAttributes
  character (len=:), allocatable  :: sAttributeText

  if (present(slAttributeValues) ) then

    ! this function takes the string list and returns one long newline character delimited text string
    sAttributeText = slAttributeValues%cat( sDelimiter=sNEWLINE )//c_null_char

    iNumberOfAttributes = int(len_trim(sAttributeText), kind=c_size_t)

    call nf_trap( nc_put_att_text(ncid=this%iNCID, &
                    varid=iVariableID, &
                    name=trim(sAttributeName), &
                    nlen=iNumberOfAttributes, &
                    tp=sAttributeText), &
                    __FILE__, __LINE__)

  elseif (present(iAttributeValues) ) then

    iNumberOfAttributes = size( iAttributeValues, 1)

    call nf_trap( nc_put_att_int(ncid=this%iNCID, &
                    varid=iVariableID, &
                    name=trim(sAttributeName), &
                    xtype=NC_INT, &
                    nlen=iNumberOfAttributes, &
                     ip=iAttributeValues), &
                     __FILE__, __LINE__)

  elseif (present(rAttributeValues) ) then

    iNumberOfAttributes = size( rAttributeValues, 1)

    call nf_trap( nc_put_att_float(ncid=this%iNCID, &
                    varid=iVariableID, &
                    name=trim(sAttributeName), &
                    xtype=NC_FLOAT, &
                    nlen=iNumberOfAttributes, &
                    fp=rAttributeValues), &
                    __FILE__, __LINE__)

  elseif (present(dpAttributeValues) ) then

    iNumberOfAttributes = size( dpAttributeValues, 1)

    call nf_trap( nc_put_att_double(ncid=this%iNCID, &
                       varid=iVariableID, &
                       name=trim(sAttributeName), &
                       xtype=NC_DOUBLE, &
                       nlen=iNumberOfAttributes, &
                       dp=dpAttributeValues), &
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

end subroutine nf_put_attributes

!--------------------------------------------------------------------------------------------------

subroutine netcdf_put_variable_array(this, iVariableID, iStart, iCount, iStride, &
   iValues, i2Values, rValues, dpValues)

  integer (kind=c_int), intent(in)               :: iNCID
  integer (kind=c_int), intent(in)               :: iVariableID
  integer (kind=c_size_t), intent(in)            :: iStart(:)
  integer (kind=c_size_t), intent(in)            :: iCount(:)
  integer (kind=c_ptrdiff_t), intent(in)         :: iStride(:)
  integer (kind=c_int), intent(in), optional     :: iValues(:,:)
  integer (kind=c_short), intent(in), optional   :: i2Values(:,:)
  real (kind=c_float), intent(in), optional      :: rValues(:,:)
  real (kind=c_double), intent(it), optional     :: dValues(:,:)

  if (present(iValues) ) then

    call nf_trap(nc_put_vars_int(ncid=iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=iValues), &
                       __FILE__, __LINE__)

  elseif (present(i2Values) ) then

    call nf_trap(nc_put_vars_short(ncid=iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=i2Values), &
                       __FILE__, __LINE__)

  elseif (present(rValues) ) then

   call nf_trap(nc_put_vars_float(ncid=iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=fValues), &
                       __FILE__, __LINE__)

  elseif (present(dpValues) ) then

    call nf_trap(nc_put_vars_double(ncid=iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=dp=Values), &
                       __FILE__, __LINE__)

  endif

end subroutine netcdf_put_variable_array

!--------------------------------------------------------------------------------------------------

subroutine netcdf_put_packed_variable_array(iNCID, iVariableID, iStart, iCount, iStride, &
   lMask, iValues, iField, i2Values, i2Field, fValues, fField, dValues, dField)

  integer (kind=c_int), intent(in)               :: iNCID
  integer (kind=c_int), intent(in)               :: iVariableID
  integer (kind=c_size_t), intent(in)            :: iStart(:)
  integer (kind=c_size_t), intent(in)            :: iCount(:)
  integer (kind=c_ptrdiff_t), intent(in)         :: iStride(:)
  logical (kind=c_bool), intent(in)              :: lMask(:,:)
  integer (kind=c_int), intent(in), optional     :: iValues(:)
  integer (kind=c_int), intent(in), optional     :: iField(:,:)
  integer (kind=c_short), intent(in), optional   :: i2Values(:)
  integer (kind=c_short), intent(in), optional   :: i2Field(:,:)
  real (kind=c_float), intent(in), optional      :: fValues(:)
  real (kind=c_float), intent(in), optional      :: fField(:,:)
  real (kind=c_double), intent(in), optional     :: dValues(:)
  real (kind=c_double), intent(in), optional     :: dField(:,:)

  if (present(iValues) ) then

    call nf_trap(nc_put_vars_int(ncid=iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=unpack(iValues, lMask, iField)), &
                       __FILE__, __LINE__)

  elseif (present(i2Values) ) then

    call nf_trap(nc_put_vars_short(ncid=iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=unpack(i2Values, lMask, i2Field)), &
                       __FILE__, __LINE__)

  elseif (present(rValues) ) then

   call nf_trap(nc_put_vars_float(ncid=iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=unpack(fValues, lMask, fField)), &
                       __FILE__, __LINE__)

  elseif (present(dValues) ) then

    call nf_trap(nc_put_vars_double(ncid=iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=unpack(dValues, lMask, dField)), &
                       __FILE__, __LINE__)

  endif

end subroutine netcdf_put_packed_variable_array

!--------------------------------------------------------------------------------------------------

subroutine netcdf_put_variable_vector(iNCID, iVariableID, iStart, iCount, iStride, &
   iValues, i2Values, rValues, dValues)

  integer (kind=c_int), intent(in)                     :: iNCID
  integer (kind=c_int), intent(in)                     :: iVariableID
  integer (kind=c_size_t), intent(in)                  :: iStart(:)
  integer (kind=c_size_t), intent(in)                  :: iCount(:)
  integer (kind=c_ptrdiff_t), intent(in)               :: iStride(:)
  integer (kind=c_int), intent(inout), optional        :: iValues(:)
  integer (kind=c_short), intent(inout), optional      :: i2Values(:)
  real (kind=c_float), intent(inout), optional         :: fValues(:)
  real (kind=c_double), intent(inout), optional        :: dValues(:)

  if ( present(iValues) ) then

    call nf_trap(nc_put_vars_int(ncid=iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=iValues), &
                       __FILE__, __LINE__)

  elseif ( present(i2Values) ) then

    call nf_trap(nc_put_vars_short(ncid=iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=i2Values), &
                       __FILE__, __LINE__)

  elseif ( present(fValues) ) then

    call nf_trap(nc_put_vars_float(ncid=iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=fValues), &
                       __FILE__, __LINE__)

  elseif ( present(dpValues) ) then

    call nf_trap(nc_put_vars_double(ncid=iNCID, &
                       varid=iVariableID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=dValues), &
                       __FILE__, __LINE__)

  endif

end subroutine netcdf_put_variable_vector

!--------------------------------------------------------------------------------------------------

end module netcdf4_support
