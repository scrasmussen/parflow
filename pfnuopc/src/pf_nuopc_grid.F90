#include "pf_nuopc_macros.h"

module parflow_nuopc_grid

  use ESMF
  use NUOPC
  use parflow_nuopc_flags

  implicit none

  private

  public grid_write

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine grid_write(grid, fileName, overwrite, status, timeslice, iofmt, &
  relaxedflag, rc)
    type(ESMF_Grid), intent(in)                      :: grid
    character(len=*), intent(in), optional           :: fileName
    logical, intent(in), optional                    :: overwrite
    type(ESMF_FileStatus_Flag), intent(in), optional :: status
    integer, intent(in), optional                    :: timeslice
    type(ESMF_IOFmt_Flag), intent(in), optional      :: iofmt
    logical, intent(in), optional                    :: relaxedflag
    integer, intent(out)                             :: rc
    ! local variables

    logical                 :: ioCapable
    logical                 :: doItFlag
    character(len=64)       :: lfileName
    character(len=64)       :: gridName
    type(ESMF_Array)        :: array
    type(ESMF_ArrayBundle)  :: arraybundle
    logical                 :: isPresent
    integer                 :: dimCount
    integer                 :: dimIndex
    integer,allocatable     :: coordDimCount(:)
    integer                 :: coordDimMax
    integer                 :: stat
    logical                 :: hasCorners

    rc = ESMF_SUCCESS

    ioCapable = (ESMF_IO_PIO_PRESENT .and. &
      (ESMF_IO_NETCDF_PRESENT .or. ESMF_IO_PNETCDF_PRESENT))

    doItFlag = .true. ! default
    if (present(relaxedFlag)) then
      doItFlag = .not.relaxedflag .or. (relaxedflag.and.ioCapable)
    endif

    if (doItFlag) then

      if (present(fileName)) then
        lfileName = trim(fileName)
      else
        call ESMF_GridGet(grid, name=gridName, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        lfileName = trim(gridName)//".nc"
      endif

      arraybundle = ESMF_ArrayBundleCreate(rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      ! -- centers --

      call ESMF_GridGetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        isPresent=isPresent, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      if (isPresent) then
        call ESMF_GridGetCoord(grid, coordDim=1, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_ArraySet(array, name="lon_center", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_GridGetCoord(grid, coordDim=2, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_ArraySet(array, name="lat_center", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif

      ! -- corners --

      call ESMF_GridGetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CORNER, &
        isPresent=hasCorners, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      if (hasCorners) then
        call ESMF_GridGetCoord(grid, coordDim=1, &
          staggerLoc=ESMF_STAGGERLOC_CORNER, array=array, rc=rc)
        if (.not. ESMF_STDERRORCHECK(rc)) then
          call ESMF_ArraySet(array, name="lon_corner", rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
          call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        endif
        call ESMF_GridGetCoord(grid, coordDim=2, &
          staggerLoc=ESMF_STAGGERLOC_CORNER, array=array, rc=rc)
        if (.not. ESMF_STDERRORCHECK(rc)) then
          call ESMF_ArraySet(array, name="lat_corner", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, PASSTHRU)) return  ! bail out
          call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        endif
      endif

      ! -- mask --

      call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_MASK, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      if (isPresent) then
        call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
          itemflag=ESMF_GRIDITEM_MASK, array=array, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_ArraySet(array, name="mask", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif

      ! -- area --

      call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_AREA, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      if (isPresent) then
        call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
          itemflag=ESMF_GRIDITEM_AREA, array=array, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_ArraySet(array, name="area", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif

      call ESMF_ArrayBundleWrite(arraybundle, &
        fileName=trim(lfileName),rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call ESMF_ArrayBundleDestroy(arraybundle,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif
  end subroutine

  !-----------------------------------------------------------------------------

end module parflow_nuopc_grid
