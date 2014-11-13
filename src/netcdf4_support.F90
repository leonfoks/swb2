!> @file
!!  Contains a single module, @ref netcdf4_support, which
!!  provides support for use of NetCDF files as input or output.
!!
!! Supports use of NetCDF files as input for time-varying,
!! gridded meteorologic data, or output for any SWB-generated variable.
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

subroutine nf_open_file_readonly( iNCID, sFilename, iFileformat )

  integer (kind=c_int), intent(inout)  :: iNCID
  character (len=*), intent(in)        :: sFilename
  integer (kind=c_int), intent(out)    :: iFileformat

  ! [ LOCALS ]
  logical (kind=c_bool) :: lFileOpen

  call LOGS%write("Attempting to open READONLY NetCDF file: " &
    //dquote(sFilename))

  call nf_trap( nc_open(trim(sFilename)//c_null_char, &
                NC_READONLY, iNCID), __FILE__, __LINE__ )

  call nf_trap( nc_inq_format(ncid=iNCID, formatp=iFileFormat), __FILE__, __LINE__ )

  call LOGS%write("   Succeeded.  ncid: "//asCharacter( iNCID ) &
         //"  format: "//trim( NETCDF_FORMAT_STRING( iFileFormat ) ) )

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

! subroutine nf_get_variable_slice(this, pNC_VAR, rValues, iValues)

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

! end subroutine nf_get_variable_slice

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_vector_short( iNCID, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, i2NC_Vars )

  integer (kind=c_int), intent(in)               :: iNCID
  integer (kind=c_int), intent(in)               :: iVariableID
  integer (kind=c_size_t), intent(in)            :: iNC_Start
  integer (kind=c_size_t), intent(in)            :: iNC_Count
  integer (kind=c_ptrdiff_t), intent(in)         :: iNC_Stride
  integer (kind=c_short), intent(inout)          :: i2NC_Vars(:)

  call nf_trap( nc_get_vars_short( ncid=iNCID,        &
       varid=iVariableID,                             &
       startp=[iNC_Start],                            &
       countp=[iNC_Count],                            &
       stridep=[iNC_Stride],                          &
       vars=i2NC_Vars ), __FILE__, __LINE__ )

end subroutine nf_get_variable_vector_short

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_array_short( iNCID, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, i2NC_Vars )

  integer (kind=c_int), intent(in)               :: iNCID
  integer (kind=c_int), intent(in)               :: iVariableID
  integer (kind=c_size_t), intent(in)            :: iNC_Start(:)
  integer (kind=c_size_t), intent(in)            :: iNC_Count(:)
  integer (kind=c_size_t), intent(in)            :: iNC_Stride(:)
  integer (kind=c_short), intent(inout)          :: i2NC_Vars(:,:)

  call nf_trap( nc_get_vars_short( ncid=iNCID,          &
       varid=iVariableID,                               &
       startp=[iNC_Start],                              &
       countp=[iNC_Count],                              &
       stridep=[iNC_Stride],                            &
       vars=i2NC_Vars ), __FILE__, __LINE__ )

end subroutine nf_get_variable_array_short

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_array_as_vector_short( iNCID, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, i2NC_Vars )

  integer (kind=c_int), intent(in)               :: iNCID
  integer (kind=c_int), intent(in)               :: iVariableID
  integer (kind=c_size_t), intent(in)            :: iNC_Start(:)
  integer (kind=c_size_t), intent(in)            :: iNC_Count(:)
  integer (kind=c_ptrdiff_t),intent(in)          :: iNC_Stride(:)
  integer (kind=c_short), intent(inout)          :: i2NC_Vars(:)

  call nf_trap( nc_get_vars_short( ncid=this%iNCID,     &
       varid=iVariableID,                               &
       startp=[iNC_Start],                              &
       countp=[iNC_Count],                              &
       stridep=[iNC_Stride],                            &
       vars=i2NC_Vars ), __FILE__, __LINE__ )

end subroutine nf_get_variable_array_as_vector_short

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_array_as_vector_int( iNCID, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, iNC_Vars )

  integer (kind=c_int), intent(in)               :: iNCID
  integer (kind=c_int), intent(in)               :: iVariableID
  integer (kind=c_size_t), intent(in)            :: iNC_Start(:)
  integer (kind=c_size_t), intent(in)            :: iNC_Count(:)
  integer (kind=c_ptrdiff_t), intent(in)         :: iNC_Stride(:)
  integer (kind=c_int), intent(inout)            :: iNC_Vars(:)

  call nf_trap( nc_get_vars_int( ncid=iNCID,         &
       varid=iVariableID,                            &
       startp=[iNC_Start],                           &
       countp=[iNC_Count],                           &
       stridep=[iNC_Stride],                         &
       vars=iNC_Vars ), __FILE__, __LINE__ )

end subroutine nf_get_variable_array_as_vector_int

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_array_int( iNCID, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, iNC_Vars )

  integer (kind=c_int), intent(in)               :: iNCID
  integer (kind=c_int), intent(in)               :: iVariableID
  integer (kind=c_size_t), intent(in)            :: iNC_Start(:)
  integer (kind=c_size_t), intent(in)            :: iNC_Count(:)
  integer (kind=c_ptrdiff_t), intent(in)         :: iNC_Stride(:)
  integer (kind=c_int), intent(inout)            :: iNC_Vars(:,:)

  call nf_trap( nc_get_vars_int( ncid=iNCID,         &
       varid=iVariableID,                            &
       startp=[iNC_Start],                           &
       countp=[iNC_Count],                           &
       stridep=[iNC_Stride],                         &
       vars=iNC_Vars ), __FILE__, __LINE__ )

end subroutine nf_get_variable_array_int

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_vector_int( iNCID, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, iNC_Vars )

  integer (kind=c_int), intent(in)               :: iNCID
  integer (kind=c_int), intent(in)               :: iVariableID
  integer (kind=c_size_t), intent(in)            :: iNC_Start
  integer (kind=c_size_t), intent(in)            :: iNC_Count
  integer (kind=c_ptrdiff_t), intent(in)         :: iNC_Stride
  integer (kind=c_int), intent(in)               :: iNC_Vars(:)

  call nf_trap( nc_get_vars_int( ncid=this%iNCID,       &
       varid=iVariableID,                               &
       startp=[iNC_Start],                              &
       countp=[iNC_Count],                              &
       stridep=[iNC_Stride],                            &
       vars=iNC_Vars ), __FILE__, __LINE__ )

end subroutine nf_get_variable_vector_int

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_vector_double( iNCID, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, dNC_Vars )

  integer (kind=c_int), intent(in)               :: iNCID
  integer (kind=c_int), intent(in)               :: iVariableID
  integer (kind=c_size_t), intent(in)            :: iNC_Start
  integer (kind=c_size_t), intent(in)            :: iNC_Count
  integer (kind=c_size_t), intent(in)            :: iNC_Stride
  real (kind=c_double), intent(inout)            :: dNC_Vars(:)

  call nf_trap( nc_get_vars_double( ncid=iNCID,         &
       varid=iVariableID,                               &
       startp=[iNC_Start],                              &
       countp=[iNC_Count],                              &
       stridep=[iNC_Stride],                            &
       vars=dNC_Vars ), __FILE__, __LINE__ )

end subroutine nf_get_variable_vector_double

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_array_double( iNCID, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, dNC_Vars )

  integer (kind=c_int), intent(in)               :: iNCID
  integer (kind=c_int), intent(in)               :: iVariableID
  integer (kind=c_size_t), intent(in)            :: iNC_Start(:)
  integer (kind=c_size_t), intent(in)            :: iNC_Count(:)
  integer (kind=c_size_t), intent(in)            :: iNC_Stride(:)
  real (kind=c_double), intent(in)               :: dNC_Vars(:,:)

  call nf_trap(nc_get_vars_double(ncid=iNCID,       &
       varid=iVariableID,                           &
       startp=[iNC_Start],                          &
       countp=[iNC_Count],                          &
       stridep=[iNC_Stride],                        &
       vars=dNC_Vars ), __FILE__, __LINE__ )

end subroutine nf_get_variable_array_double

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_array_as_vector_double( iNCID, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, dNC_Vars )

  integer (kind=c_int), intent(in)               :: iNCID
  integer (kind=c_int), intent(in)               :: iVariableID
  integer (kind=c_size_t), intent(in)            :: iNC_Start(:)
  integer (kind=c_size_t), intent(in)            :: iNC_Count(:)
  integer (kind=c_ptrdiff_t), intent(in)         :: iNC_Stride(:)
  real (kind=c_double), intent(inout)            :: dNC_Vars(:)

  call nf_trap( nc_get_vars_double( ncid=iNCID,       &
       varid=iVariableID,                             &
       startp=[iNC_Start],                            &
       countp=[iNC_Count],                            &
       stridep=[iNC_Stride],                          &
       vars=dNC_Vars ), __FILE__, __LINE__ )

end subroutine nf_get_variable_array_as_vector_double

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_vector_float( iNCID, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, fNC_Vars )

  integer (kind=c_int), intent(in)               :: iNCID
  integer (kind=c_int), intent(in)               :: iVariableID
  integer (kind=c_size_t), intent(in)            :: iNC_Start
  integer (kind=c_size_t), intent(in)            :: iNC_Count
  integer (kind=c_ptrdiff_t), intent(in)         :: iNC_Stride
  real (kind=c_float), intent(inout)             :: fNC_Vars(:)

  call nf_trap( nc_get_vars_float( ncid=this%iNCID,   &
       varid=iVariableID,                             &
       startp=[iNC_Start],                            &
       countp=[iNC_Count],                            &
       stridep=[iNC_Stride],                          &
       vars=fNC_Vars ), __FILE__, __LINE__ )

end subroutine nf_get_variable_vector_float

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_array_float( iNCID, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, fNC_Vars )

  integer (kind=c_int), intent(in)               :: iNCID
  integer (kind=c_int), intent(in)               :: iVariableID
  integer (kind=c_size_t), intent(in)            :: iNC_Start(:)
  integer (kind=c_size_t), intent(in)            :: iNC_Count(:)
  integer (kind=c_ptrdiff_t), intent(in)         :: iNC_Stride(:)
  real (kind=c_float), intent(inout)             :: fNC_Vars(:,:)

  call nf_trap( nc_get_vars_float( ncid=iNCID,         &
       varid=iVariableID,                              &
       startp=[iNC_Start],                             &
       countp=[iNC_Count],                             &
       stridep=[iNC_Stride],                           &
       vars=fNC_Vars ), __FILE__, __LINE__ )

end subroutine nf_get_variable_array_float

!--------------------------------------------------------------------------------------------------

subroutine nf_get_variable_array_as_vector_float( iNCID, iVariableID, iNC_Start, iNC_Count, &
   iNC_Stride, fNC_Vars )

  integer (kind=c_int), intent(in)               :: iNCID
  integer (kind=c_int), intent(in)               :: iVariableID
  integer (kind=c_size_t), intent(in)            :: iNC_Start(:)
  integer (kind=c_size_t), intent(in)            :: iNC_Count(:)
  integer (kind=c_ptrdiff_t), intent(in)         :: iNC_Stride(:)
  real (kind=c_float), intent(inout)             :: rNC_Vars(:)

  call nf_trap( nc_get_vars_float( ncid=this%iNCID,   &
       varid=iVariableID,                             &
       startp=[iNC_Start],                            &
       countp=[iNC_Count],                            &
       stridep=[iNC_Stride],                          &
       vars=fNC_Vars), __FILE__, __LINE__ )

end subroutine nf_get_variable_array_as_vector_float



!--------------------------------------------------------------------------------------------------

subroutine nf_create( iNCID, sFilename, iFileFormat, iLU )

  integer (kind=c_int), intent(in)             :: iNCID
  character (len=*), intent(in)                :: sFilename
  integer (kind=c_int), intent(in)             :: iFileFormat
  integer (kind=c_int), intent(in), optional   :: iLU

!  character (len=256) :: sBuf
!  call getcwd(sBuf)

  call nf_trap(nc_create(path=trim( fortran_to_c_string( sFilename ) ),    &
                 cmode=iFileFormat,                                        &
                 ncidp=iNCID ),                                            &
                 __FILE__, __LINE__)

  if (present( iLU ) ) then
    call LOGS%write("Created NetCDF file for output. Filename: " &
      //dquote( sFilename )//"; NCID="//asCharacter( iNCID ) )
  endif

end subroutine nf_create

!--------------------------------------------------------------------------------------------------

subroutine nf_define_deflate(iNCID, iVariableID, iShuffle, iDeflate, iDeflate_level)

  integer (kind=c_int), intent(in)      :: iNCID
  integer (kind=c_int), intent(in)      :: iVariableID
  integer (kind=c_int), intent(in)      :: iShuffle
  integer (kind=c_int), intent(in)      :: iDeflate
  integer (kind=c_int), intent(in)      :: iDeflate_level

  call nf_trap(nc_def_var_deflate(ncid=this%iNCID,      &
          varid=iVariableID,                            &
          shuffle=iShuffle,                             &
          deflate=iDeflate,                             & 
          deflate_level=iDeflate_level),                &
          __FILE__, __LINE__)

end subroutine nf_define_deflate

!--------------------------------------------------------------------------------------------------

subroutine nf_enddef( iNCID )

  integer (kind=c_int), intent(in)             :: iNCID

  call nf_trap(nc_enddef(ncid=iNCID), __FILE__, __LINE__ )

end subroutine nf_enddef

!--------------------------------------------------------------------------------------------------

function nf_define_dimension( iNCID, sDimensionName, iDimensionSize )    result( iDimensionID )

  integer (kind=c_int), intent(in)      :: iNCID
  character (len=*), intent(in)         :: sDimensionName
  integer (kind=c_int), intent(in)      :: iDimensionSize
  integer (kind=c_int)                  :: iDimensionID

  ! [ LOCALS ]
  integer (kind=c_size_t)               :: iDimensionSize_

  ! need to convert size to c_size_t to make call to library
  iDimensionSize_ = int(iDimensionSize, kind=c_size_t)

  call nf_trap(nc_def_dim(ncid=iNCID,                                  &
                          name=trim(sDimensionName)//c_null_char,      &
                          lenv=iDimensionSize_,                        &
                          dimidp=iDimensionID),                        &
                          __FILE__, __LINE__)

end function nf_define_dimension

!--------------------------------------------------------------------------------------------------

function nf_define_variable( iNCID, sVariableName, iVariableType, &
  iNumberOfDimensions, iDimensionIDs)                                 result(iVariableID)

  integer (kind=c_int), intent(in)      :: iNCID
  character (len=*), intent(in)         :: sVariableName
  integer (kind=c_int), intent(in)      :: iVariableType
  integer (kind=c_int), intent(in)      :: iNumberOfDimensions
  integer (kind=c_int), intent(in)      :: iDimensionIDs(:)
  integer (kind=c_int)                  :: iVariableID

  call nf_trap( nc_def_var(ncid=iNCID,                                        &
                           name=trim(fortran_to_c_string(sVariableName)),     &
                           xtype=iVariableType,                               &
                           ndims=iNumberOfDimensions,                         &
                           dimidsp=iDimensionIDs,                             &
                           varidp=iVariableID),                               &
                           __FILE__, __LINE__)

end function nf_define_variable

!--------------------------------------------------------------------------------------------------

subroutine nf_put_attribute( iNCID, iVariableID, sAttributeName, &
  slAttributeValues, iAttributeValues, rAttributeValues, dpAttributeValues)
 
  integer (kind=c_int), intent(in)                :: iNCID
  integer (kind=c_int), intent(in)                :: iVariableID
  character (len=*), intent(in)                   :: sAttributeName
  type (STRING_LIST_T), intent(in), optional      :: slAttributeValues
  integer (kind=c_int), intent(in), optional      :: iAttributeValues(:)
  real (kind=c_float), intent(in), optional       :: rAttributeValues(:)
  real (kind=c_double), intent(in), optional      :: dpAttributeValues(:)

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

subroutine nf_put_variable_array(iNCID, iVariableID, iStart, iCount, iStride, &
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

end subroutine nf_put_variable_array

!--------------------------------------------------------------------------------------------------

subroutine nf_put_packed_variable_array(iNCID, iVariableID, iStart, iCount, iStride, &
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

end subroutine nf_put_packed_variable_array

!--------------------------------------------------------------------------------------------------

subroutine nf_put_variable_vector(iNCID, iVariableID, iStart, iCount, iStride, &
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

end subroutine nf_put_variable_vector

!--------------------------------------------------------------------------------------------------

end module netcdf4_support
