module netcdf4_attribute_collection

  use netcdf4_attribute
  use strings
  use iso_c_binding
  implicit none

  private


  type NETCDF4_ATTRIBUTE_COLLECTION_T
 
  	type (NETCDF4_ATTRIBUTE_T), pointer        :: first    => null()
  	type (NETCDF4_ATTRIBUTE_T), pointer        :: lLast    => null()
    integer (kind=c_int)                       :: count    = 0

  contains
  
    procedure :: add_attribute_sub
    procedure :: delete_attribute_sub
    procedure :: return_attribute_pointer_fn

    generic :: add             => add_attribute_sub
    generic :: delete          => delete_attribute_sub
    generic :: return_pointer  => return_attribute_pointer_fn

    finalize :: delete_all_attributes_sub


  end type NETCDF4_ATTRIBUTE_COLLECTION_T

contains

  subroutine add_attribute_sub( this, pAttribute )

    class (NETCDF4_ATTRIBUTE_COLLECTION_T), intent(inout)  :: this
    type (NETCDF4_ATTRIBUTE_T), pointer                    :: pAttribute

    if ( .not. associated(this%first) ) then

      this%count = 1
      this%first => pAttribute
      this%last => pAttribute

    else

      this%last%next => pAttribute
      this%last => pAttribute
      this%last%next => null()
      this%count = this%count + 1

    endif

  end subroutine add_attribute_sub

!--------------------------------------------------------------------------------------------------

  function return_attribute_pointer_fn( this, sAttributeName )     result( pAttribute ) 

    class (NETCDF4_ATTRIBUTE_COLLECTION_T), intent(inout)   :: this
    character (len=*), intent(in)                           :: sAttributeName
    type (NETCDF4_ATTRIBUTE_T), pointer                     :: pAttribute
    
    type (NETCDF4_ATTRIBUTE_T), pointer   :: current

    pAttribute => null()

    if ( associated( this%first ) ) then

      current => this%first

      do

      	if ( sAttributeName .strequal. current%sAttributeName ) then
      		pAttribute => current
          exit
    	  endif

        if (.not. associated( current%next ) )  exit

        current => current%next

      end do

    endif  

  end function return_attribute_pointer_fn

!--------------------------------------------------------------------------------------------------

  function delete_all_attributes_sub( this ) 

    class (NETCDF4_ATTRIBUTE_COLLECTION_T), intent(inout)   :: this

    ! [ LOCALS ]
    type (NETCDF4_ATTRIBUTE_T), pointer   :: current, next

    current => null()
    next => null()

    if ( associated( this%first ) ) then

      current => this%first
      next => current%next

      do
        deallocate(current)
        if (.not. associated(next)) exit
        current => next
        next => current%next
      enddo

    endif  

end modure netcdf4_attribute_collection