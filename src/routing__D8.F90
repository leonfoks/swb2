module routing__D8

  use iso_c_binding, only : c_short, c_int, c_float, c_double, c_bool
  use data_catalog
  use data_catalog_entry
  use netcdf4_support
  use simulation_datetime
  use exceptions
  implicit none

  integer (kind=c_int), allocatable :: DOWNSTREAM_CELL_INDEX(:)

  private

  public :: routing_D8_initialize, routing_D8_calculate, D8_UNDETERMINED, TARGET_INDEX, ORDER_INDEX

  type (DATA_CATALOG_ENTRY_T), pointer :: pD8_FLOWDIR    ! data catalog object => D8 flow direction grid

  integer (kind=c_int), allocatable     :: iTargetRow(:,:)
  integer (kind=c_int), allocatable     :: iTargetCol(:,:)
  logical (kind=c_bool), allocatable    :: lDownhillMarked(:,:)
  integer (kind=c_int), allocatable     :: iSumOfUpslopeCells(:,:)
  integer (kind=c_int), allocatable     :: iNumberOfConnections(:,:)
  integer (kind=c_int), allocatable     :: ROW2D(:,:)
  integer (kind=c_int), allocatable     :: COL2D(:,:) 
  integer (kind=c_int), allocatable     :: ROW1D(:)
  integer (kind=c_int), allocatable     :: COL1D(:) 

  integer (kind=c_int), allocatable     :: ROW_INDEX(:)
  integer (kind=c_int), allocatable     :: COLUMN_INDEX(:)
  integer (kind=c_int), allocatable     :: ORDER_INDEX(:)
  integer (kind=c_int), allocatable     :: TARGET_INDEX(:)

  integer (kind=c_int), parameter       :: D8_EAST         = 1
  integer (kind=c_int), parameter       :: D8_SOUTHEAST    = 2
  integer (kind=c_int), parameter       :: D8_SOUTH        = 4
  integer (kind=c_int), parameter       :: D8_SOUTHWEST    = 8
  integer (kind=c_int), parameter       :: D8_WEST         = 16
  integer (kind=c_int), parameter       :: D8_NORTHWEST    = 32
  integer (kind=c_int), parameter       :: D8_NORTH        = 64
  integer (kind=c_int), parameter       :: D8_NORTHEAST    = 128

  integer (kind=c_int), parameter       :: D8_UNDETERMINED = -999

  type (T_NETCDF4_FILE), pointer       :: pNCFILE ! pointer to OUTPUT NetCDF file

contains

  subroutine routing_D8_initialize( lActive, dX, dY, dX_lon, dY_lat )

    logical (kind=c_bool), intent(in)     :: lActive(:,:)
    real (kind=c_double), intent(in)      :: dX(:)
    real (kind=c_double), intent(in)      :: dY(:)
    real (kind=c_double), intent(in)      :: dX_lon(:,:)
    real (kind=c_double), intent(in)      :: dY_lat(:,:)

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iNX
    integer (kind=c_int)                 :: iNY
    integer (kind=c_int)                 :: iStat
    integer (kind=c_int)                 :: iColnum
    integer (kind=c_int)                 :: iRownum  
    integer (kind=c_int)                 :: iIndex 
    integer (kind=c_int)                 :: iCount

    integer (kind=c_int) :: iCol_lbound, iCol_ubound
    integer (kind=c_int) :: iRow_lbound, iRow_ubound

    iCol_lbound = lbound(lActive, 1)
    iCol_ubound = ubound(lActive, 1)

    iRow_lbound = lbound(lActive, 2)
    iRow_ubound = ubound(lActive, 2)

    iCount = 0

    iNX = ubound(lActive, 1)
    iNY = ubound(lActive, 2)

    allocate( iTargetRow( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( iTargetCol( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )


    allocate( lDownhillMarked( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( COL2D( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( ROW2D( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( COL1D( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( ROW1D( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( iSumOfUpslopeCells( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( iNumberOfConnections( iNX, iNY ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( ROW_INDEX( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( COLUMN_INDEX( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( TARGET_INDEX( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( ORDER_INDEX( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( DOWNSTREAM_CELL_INDEX( count( lActive) ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory", __FILE__, __LINE__ )

    ! locate the data structure associated with the gridded flow direction entries
    pD8_FLOWDIR => DAT%find("FLOW_DIRECTION")
    if ( .not. associated(pD8_FLOWDIR) ) &
        call die("A FLOW_DIRECTION grid must be supplied in order to make use of this option.", __FILE__, __LINE__)

    call pD8_FLOWDIR%getvalues()

    call routing_D8_assign_downstream_row_col( lActive )

    do iRownum=iRow_lbound, iRow_ubound    
      do iColnum=iCol_lbound, iCol_ubound  
 
        COL2D( iColnum, iRownum ) = iColNum   ! right-most index should vary slowest
        ROW2D( iColnum, iRownum ) = iRownum

        if ( lActive( iColnum, iRownum) ) then
        	iCount = iCount + 1
        	ORDER_INDEX(iCount) = iCount
        endif

      enddo
    enddo

    COL1D = pack( COL2D, lActive )
    ROW1D = pack( ROW2D, lActive )	

    call routing_D8_determine_solution_order( lActive )    

    allocate ( pNCFILE, stat=iStat )
      call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    call netcdf_open_and_prepare_as_output( NCFILE=pNCFILE, sVariableName="D8_flow_accumulation", &
      sVariableUnits="number of upslope contributing cells", iNX=iNX, iNY=iNY, &
      fX=dX, fY=dY, StartDate=SIM_DT%start, EndDate=SIM_DT%end, dpLat=dY_lat, dpLon=dX_lon, &
      iVarType = NC_INT )

    ! write timestamp to NetCDF file
    call netcdf_put_variable_vector(NCFILE=pNCFILE, &
      iVarID=pNCFILE%iVarID(NC_TIME), &
      iStart=[0_c_size_t], &
      iCount=[1_c_size_t], &
      iStride=[1_c_ptrdiff_t], &
      dpValues=[0.0_c_double])

    call netcdf_put_variable_array(NCFILE=pNCFILE, &
    	           iVarID=pNCFILE%iVarID(NC_Z), &
                 iStart=[0_c_size_t,0_c_size_t, 0_c_size_t], &
                 iCount=[1_c_size_t, int(iNY, kind=c_size_t), int(iNX, kind=c_size_t)],              &
                 iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
                 iValues=iSumOfUpslopeCells)

    call netcdf_close_file( pNCFILE )

  end subroutine routing_D8_initialize

!--------------------------------------------------------------------------------------------------

  function routing_D8_get_index( iCol, iRow )   result( iIndex )

    integer (kind=c_int), intent(in)   :: iCol
    integer (kind=c_int), intent(in)   :: iRow
    integer (kind=c_int)               :: iIndex

    ! [ LOCALS ]
    integer (kind=c_int)   :: iOrderIndex

    iIndex = D8_UNDETERMINED
 
    do iOrderIndex = lbound(COL1D,1), ubound(COL1D,1) 

      if( COL1D(iOrderIndex) == iCol  .and.  ROW1D(iOrderIndex) == iRow ) then
      	iIndex = iOrderIndex
      	exit
      endif

    enddo

  end function routing_D8_get_index

!--------------------------------------------------------------------------------------------------

  subroutine routing_D8_assign_downstream_row_col( lActive )

    logical (kind=c_bool), intent(in)    :: lActive(:,:)

    ! [ LOCALS ]
    integer (kind=c_int) :: iRownum
    integer (kind=c_int) :: iColnum
    integer (kind=c_int) :: iCol_lbound, iCol_ubound
    integer (kind=c_int) :: iRow_lbound, iRow_ubound

    iCol_lbound = lbound(lActive, 1)
    iCol_ubound = ubound(lActive, 1)

    iRow_lbound = lbound(lActive, 2)
    iRow_ubound = ubound(lActive, 2)

    associate ( dir => pD8_FLOWDIR%pGrdBase%iData )


	    do iRownum=iRow_lbound, iRow_ubound
  	    do iColnum=iCol_lbound, iCol_ubound

          select case ( dir(iColnum, iRownum) )

            case ( D8_EAST )

              iTargetCol(iColnum, iRownum) = iColnum + 1
              iTargetRow(iColnum, iRownum) = iRownum
        
            case ( D8_SOUTHEAST )

              iTargetCol(iColnum, iRownum) = iColnum + 1
              iTargetRow(iColnum, iRownum) = iRownum + 1
        
            case ( D8_SOUTH )

              iTargetCol(iColnum, iRownum) = iColnum
              iTargetRow(iColnum, iRownum) = iRownum + 1
        
            case ( D8_SOUTHWEST )

              iTargetCol(iColnum, iRownum) = iColnum - 1
              iTargetRow(iColnum, iRownum) = iRownum + 1
        
            case ( D8_WEST )

              iTargetCol(iColnum, iRownum) = iColnum - 1
              iTargetRow(iColnum, iRownum) = iRownum
        
            case ( D8_NORTHWEST )

              iTargetCol(iColnum, iRownum) = iColnum - 1
              iTargetRow(iColnum, iRownum) = iRownum - 1
        
            case ( D8_NORTH )

              iTargetCol(iColnum, iRownum) = iColnum
              iTargetRow(iColnum, iRownum) = iRownum - 1
        
            case ( D8_NORTHEAST )

              iTargetCol(iColnum, iRownum) = iColnum + 1
              iTargetRow(iColnum, iRownum) = iRownum - 1
        
            case default

              iTargetCol(iColnum, iRownum) = D8_UNDETERMINED
              iTargetRow(iColnum, iRownum) = D8_UNDETERMINED

          end select 
          
        enddo

      enddo

    end associate   


  end subroutine routing_D8_assign_downstream_row_col

!--------------------------------------------------------------------------------------------------

  subroutine routing_D8_determine_solution_order( lActive )

    logical (kind=c_bool), intent(in)    :: lActive(:,:)

    ! [ LOCALS ]
    integer (kind=c_int)  :: iRownum
    integer (kind=c_int)  :: iColnum
    integer (kind=c_int)  :: iColsrch
    integer (kind=c_int)  :: iRowsrch
    integer (kind=c_int)  :: iNumberOfChangedCells
    integer (kind=c_int)  :: iCol_lbound, iCol_ubound
    integer (kind=c_int)  :: iRow_lbound, iRow_ubound
    integer (kind=c_int)  :: iTempSum, iTempConnections
    logical (kind=c_bool) :: lAnyUnmarkedUpslopeCells 
    integer (kind=c_int)  :: iNumberRemaining
    integer (kind=c_int)  :: iOrderIndex
    integer (kind=c_int)  :: iIndex
    logical (kind=c_bool) :: lCircular
    integer (kind=c_int)  :: iNumberOfIterationsWithoutChange 

    iCol_lbound = lbound(lActive, 1)
    iCol_ubound = ubound(lActive, 1)

    iRow_lbound = lbound(lActive, 2)
    iRow_ubound = ubound(lActive, 2)

    lDownhillMarked = lFALSE
    iSumOfUpslopeCells = 0_c_int

    iOrderIndex = 0

    ! first pass through: simply make a note of how many upslope connections feed into each cell
    do iRownum=iRow_lbound, iRow_ubound
      do iColnum=iCol_lbound, iCol_ubound

        ! ignore current cell if it is not an active cell
	      if ( .not. lActive(iColnum, iRownum) ) cycle

        lCircular = lFALSE
        iTempConnections = 0_c_int
		    
        do iRowsrch=max( iRowNum-1, iRow_lbound),min( iRownum+1, iRow_ubound) 
          do iColsrch=max( iColNum-1, iCol_lbound),min( iColnum+1, iCol_ubound) 			    

            ! ignore search cell if it is not an active cell
	          if ( .not. lActive(iColsrch, iRowsrch) ) cycle
	      
            ! no need to consider current cell
            if ( ( iColsrch == iColnum ) .and. ( iRowsrch == iRownum ) )  cycle

            ! if adjacent cell points to current cell, note it and move on	
            if ( ( iTargetCol(iColsrch, iRowsrch) == iColnum ) &
            	.and. ( iTargetRow(iColsrch, iRowsrch) == iRownum ) )    &

              iTempConnections = iTempConnections + 1 
  
!             ! if the current cell points back at the search cell, we have
!             ! circular flow. convert the current cell to an undetermined
!             ! flow direction	  
!             if ( ( iTargetCol(iColnum, iRownum) == iColsrch )      &
!             	.and. ( iTargetRow(iColnum, iRownum) == iRowsrch ) ) then

!               iTargetCol( iColnum, iRownum ) = D8_UNDETERMINED
!               iTargetRow( iColnum, iRownum ) = D8_UNDETERMINED

!             endif                 

          enddo                  
        enddo    

        iNumberOfConnections(iColnum, iRownum) = iTempConnections
        
        if ( iTempConnections == 0 ) then

          lDownhillMarked(iColnum, iRownum) = lTRUE
          iOrderIndex = iOrderIndex + 1
          COLUMN_INDEX(iOrderIndex) = iColnum
          ROW_INDEX(iOrderIndex) = iRownum
          ORDER_INDEX(iOrderIndex) = routing_D8_get_index( iColnum, iRownum ) 

          ! this is the first pass through. if there are no contributing cells
          ! then record the target index and record the cell as marked
          if ( iTargetCol(iColNum,iRowNum) /= D8_UNDETERMINED           &
          	 .and. iTargetRow(iColNum, iRowNum) /= D8_UNDETERMINED ) then
            TARGET_INDEX(iOrderIndex) = routing_D8_get_index( iTargetCol(iColnum, iRownum ), &
          	      iTargetRow( iColNum, iRowNum ) )
          else
          
            TARGET_INDEX(iOrderIndex) = D8_UNDETERMINED

          endif

        elseif ( iTempConnections == 1 .and. lCircular ) then
        
          iTargetCol( iColnum, iRownum ) = D8_UNDETERMINED
          iTargetRow( iColnum, iRownum ) = D8_UNDETERMINED
          lDownhillMarked(iColnum, iRownum) = lTRUE
          iOrderIndex = iOrderIndex + 1
          COLUMN_INDEX(iOrderIndex) = iColnum
          ROW_INDEX(iOrderIndex) = iRownum
          ORDER_INDEX(iOrderIndex) = routing_D8_get_index( iColnum, iRownum ) 
          TARGET_INDEX(iOrderIndex) = D8_UNDETERMINED

        endif  

	    enddo
	  enddo            

    print *, "### at end of first loop of cell ordering... ", count( lDownhillMarked ), " cells marked so far " &
      //"out of ", count( lActive ), " active cells."


iNumberOfIterationsWithoutChange = 0

main_loop: do

      iNumberOfChangedCells = 0

	    do iRownum=iRow_lbound, iRow_ubound
        do iColnum=iCol_lbound, iCol_ubound
 
	        if ( .not. lActive(iColnum, iRownum) ) cycle
	        if ( lDownhillMarked(iColnum, iRownum) ) cycle

	        iTempSum = 0
	        iTempConnections = 0
	        lAnyUnmarkedUpslopeCells = lFALSE

	local_search: do iRowsrch=max( iRowNum-1, iRow_lbound),min( iRownum+1, iRow_ubound) 
                  do iColsrch=max( iColNum-1, iCol_lbound),min( iColnum+1, iCol_ubound) 			    
	              
	 			            ! if adjacent cell points to current cell, note it and move on	
	 			            if ( ( iTargetCol(iColsrch, iRowsrch) == iColnum ) &
	 			            	.and. ( iTargetRow(iColsrch, iRowsrch) == iRownum ) ) then
	 			               			              
	 			              if ( .not. lActive( iColsrch, iRowsrch ) ) then

	 			              	cycle

                      ! if the upslope cell is unmarked as of yet, defer doing anything
                      ! with the current cell
	 			              elseif(	.not. lDownhillMarked( iColsrch, iRowsrch ) ) then

	                      lAnyUnmarkedUpslopeCells = lTRUE
	 			              	exit local_search
	 			           	  
	 			           	  else

	 			                iTempSum = iTempSum + iSumOfUpslopeCells(iColsrch, iRowsrch) + 1
	 			                iTempConnections = iTempConnections + 1 
	 			           	  
	 			           	  endif

	 			            endif

	                enddo
	              enddo local_search

	              ! OK this is the end of the local search area; did we uncover any unmarked cells
	              ! contributing flow to the current cell? if so, ignore and move on. otherwise,
	              ! mark current cell as marked, update stats, and continue with next cell

	              if ( .not. lAnyUnmarkedUpslopeCells ) then

                  iNumberOfChangedCells = iNumberOfChangedCells + 1
	                iSumOfUpslopeCells(iColnum, iRownum) = iTempSum
	                
	                iNumberOfConnections(iColnum, iRownum) = iTempConnections
	                lDownhillMarked(iColnum, iRownum) = lTRUE
                  iOrderIndex = iOrderIndex + 1
                  COLUMN_INDEX(iOrderIndex) = iColnum
                  ROW_INDEX(iOrderIndex) = iRownum
                  ORDER_INDEX(iOrderIndex) = routing_D8_get_index( iColnum, iRownum )
				
				          if ( iTargetCol(iColNum,iRowNum) /= D8_UNDETERMINED           &
				          	 .and. iTargetRow(iColNum, iRowNum) /= D8_UNDETERMINED ) then
				               TARGET_INDEX(iOrderIndex) = routing_D8_get_index( iTargetCol(iColnum, iRownum ), &
				          	      iTargetRow( iColNum, iRowNum ) )
				          else
				          
				            TARGET_INDEX(iOrderIndex) = D8_UNDETERMINED

		              endif

                elseif ( iNumberOfIterationsWithoutChange > 10 ) then

	                iSumOfUpslopeCells(iColnum, iRownum) = iTempSum
	                
	                iNumberOfChangedCells = iNumberOfChangedCells + 1
	                iNumberOfConnections(iColnum, iRownum) = iTempConnections
	                lDownhillMarked(iColnum, iRownum) = lTRUE
                  iOrderIndex = iOrderIndex + 1
                  COLUMN_INDEX(iOrderIndex) = iColnum
                  ROW_INDEX(iOrderIndex) = iRownum
                  ORDER_INDEX(iOrderIndex) = routing_D8_get_index( iColnum, iRownum )
				          iTargetCol(iColNum,iRowNum) = D8_UNDETERMINED     
				          iTargetRow(iColNum, iRowNum) = D8_UNDETERMINED
				          
				          TARGET_INDEX(iOrderIndex) = D8_UNDETERMINED

		            endif


!                 print *, iOrderIndex, iColnum, iRownum, iTargetcol(iColnum, iRownum), iTargetRow(iColnum, iRownum), &
!                    iTempSum, iTempConnections, ORDER_INDEX(iOrderIndex), &
!                    TARGET_INDEX(iOrderIndex), routing_D8_get_index( iColnum, iRownum ), &
!                    routing_D8_get_index( iTargetCol(iColnum, iRownum ), iTargetRow(iColnum, iRownum ) )

	      enddo  		
	  	enddo

      if (iNumberOfChangedCells == 0) then

      	iNumberOfIterationsWithoutChange = iNumberOfIterationsWithoutChange + 1

      endif

      iNumberRemaining = count( lActive ) - count( lDownhillMarked )

      print *, "### determining solution order... ", count( lDownhillMarked ), " cells marked so far " &
        //"out of ", count( lActive ), " active cells."  

      if ( count( lDownhillMarked ) == count( lActive ) )  exit main_loop

  	enddo main_loop

  end subroutine routing_D8_determine_solution_order


!--------------------------------------------------------------------------------------------------

  subroutine routing_D8_calculate


  end subroutine routing_D8_calculate

end module routing__D8