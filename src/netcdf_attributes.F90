module netcdf4_attribute

  use strings
  use string_list
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
    type (NETCDF_ATTRIBUTE_T), pointer         :: pNext           => null()
  end type NETCDSF_ATTRIBUTE_T

  type NETCDF4_ATTRIBUTE_COLLECTION_T
  	type (NETCDF4_ATTRIBUTE_T), pointer        :: pFirst          => null()
  	type (NETCDF4_ATTRIBUTE_T), pointer        :: pLast           => null()
    integer (kind=c_int)                       :: count

  contains

  	!> get: obtain from a NetCDF file
  	!! add: populate based on input
  
    procedure :: add_attribute_sub
    procedure :: get_attribute_sub
    procedure :: delete_attribute_sub
    procedure :: return_attribute_pointer_fn


    generic :: add             => add_attribute_sub
    generic :: get             => get_attribute_sub
    generic :: delete          => delete_attribute_sub
    generic :: return_pointer  => return_attribute_pointer_fn



  end type NETCDF4_ATTRIBUTE_COLLECTION_T

end module netcdf4_attribute