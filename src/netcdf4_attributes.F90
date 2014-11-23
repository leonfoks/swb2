module netcdf4_attributes

  use strings
  use string_list
  use netcdf_support
  use iso_c_binding
  implicit none

  type NETCDF4_ATTRIBUTE_T
    character (len=64)                         :: sAttributeName
    type (STRING_LIST_T)                       :: slValues
    integer (kind=c_short), allocatable        :: i2Values(:)
    integer (kind=c_int), allocatable          :: iValues(:)
    real (kind=c_float), allocatable           :: fValues(:)
    real (kind=c_double), allocatable          :: dValues(:)
    integer (kind=c_int)                       :: iAttributeType
    integer (kind=c_size_t)                    :: iAttributeSize
    type (NETCDF_ATTRIBUTE_T), pointer         :: next             => null()

  contains

    procedure :: new_attribute_sub
    generic   :: new => new_attribute_sub 



  end type NETCDF4_ATTRIBUTE_T

contains

  subroutine new_attribute_sub(this, sAttributeName, slValues, i2Values, iValues, fValues, dValues )

    class (NETCDF4_ATTRIBUTE_T), intent(inout)    :: this
    character (len=*), intent(in)                 :: sAttributeName
    type (STRING_LIST_T), intent(in), optional    :: slValues
    integer (kind=c_short), intent(in), optional  :: i2Values(:)
    integer (kind=c_int), intent(in), optional    :: iValues(:)
    real (kind=c_float), intent(in), optional     :: fValues(:)
    real (kind=c_double), intent(in), optional    :: dValues(:)

    this%sAttributeName = sAttributeName
    this%iAttributeType = iAttributeType

    if ( present( slValues ) ) then     

      this%slValues = slValues
      this%iAttributeType = NC_CHAR

    elseif (present ( i2Values ) ) then
    
      this%i2Values = i2Values
      this%iAttributeType = NC_SHORT 

    elseif ( present ( iValues ) ) then
    
      this%iValues = iValues
      this%iAttributeType = NC_INT

    elseif ( present ( fValues ) ) then

      this%fValues = fValues
      this%iAttributeType = NC_FLOAT

    elseif ( present ( dValues ) ) then

      this%dValues = dValues
      this%iAttributeType = NC_DOUBLE

    else

      call die("INTERNAL ERROR--must provide at least one of the optional arguments to this subroutine", &
        __FILE__, __LINE__ )

    endif   

  end subroutine new_attribute_sub



end module netcdf4_attributes