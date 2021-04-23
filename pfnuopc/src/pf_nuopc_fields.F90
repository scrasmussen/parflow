#include "pf_nuopc_macros.h"

module parflow_nuopc_fields

  use ESMF
  use NUOPC
  use parflow_nuopc_flags

  implicit none

  private

  type pf_nuopc_fld_type
    sequence
    character(len=64)           :: sd_name    = "dummy" ! standard name
    character(len=64)           :: st_name    = "dummy" ! state name
    character(len=64)           :: units      = "-"     ! units
    logical                     :: ad_import  = .FALSE. ! advertise import
    logical                     :: ad_export  = .FALSE. ! advertise export
    logical                     :: rl_import  = .FALSE. ! realize import
    logical                     :: rl_export  = .FALSE. ! realize export
    real(ESMF_KIND_R8)          :: vl_default = ESMF_DEFAULT_VALUE ! default value
  end type pf_nuopc_fld_type

  type(pf_nuopc_fld_type),target,dimension(7) :: pf_nuopc_fld_list = (/     &
    pf_nuopc_fld_type("parflow_flux              ","PF_FLUX              ", &
                      "m d-1",  .TRUE.,  .TRUE.), &
    pf_nuopc_fld_type("parflow_porosity          ","PF_POROSITY          ", &
                      "-    ",  .TRUE.,  .TRUE.), &
    pf_nuopc_fld_type("parflow_pressure          ","PF_PRESSURE          ", &
                      "m    ",  .TRUE.,  .TRUE.), &
    pf_nuopc_fld_type("parflow_saturation        ","PF_SATURATION        ", &
                      "-    ",  .TRUE.,  .TRUE.), &
    pf_nuopc_fld_type("parflow_precip_accumulator","PF_PCPDRP_ACCUMULATOR", &
                      "m    ", .FALSE., .FALSE.), &
    pf_nuopc_fld_type("parflow_edir_accumulator  ","PF_EDIR_ACCUMULATOR  ", &
                      "m    ", .FALSE., .FALSE.), &
    pf_nuopc_fld_type("parflow_et_accumulator    ","PF_ET_ACCUMULATOR    ", &
                      "m    ", .FALSE., .FALSE.) /)

  public pf_nuopc_fld_list
  public field_advertise
  public field_realize
  public field_advertise_log
  public field_realize_log
  public field_fill_state

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine field_dictionary_add(fieldList, rc)
    type(pf_nuopc_fld_type), intent(in) :: fieldList(:)
    integer, intent(out) :: rc
    ! local variables
    integer :: n
    logical :: isPresent

    rc = ESMF_SUCCESS

    do n=lbound(fieldList,1),ubound(fieldList,1)
      isPresent = NUOPC_FieldDictionaryHasEntry( &
        fieldList(n)%sd_name, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      if (.not.isPresent) then
        call NUOPC_FieldDictionaryAddEntry( &
          StandardName=trim(fieldList(n)%sd_name), &
          canonicalUnits=trim(fieldList(n)%units), &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      end if
    end do

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine field_realize(fieldList, importState, exportState, grid, &
  num_soil_layers, realizeAllImport, realizeAllExport, shareMemory, rc)
    type(pf_nuopc_fld_type), intent(inout) :: fieldList(:)
    type(ESMF_State), intent(inout)        :: importState
    type(ESMF_State), intent(inout)        :: exportState
    type(ESMF_Grid), intent(in)            :: grid
    integer, intent(in)                    :: num_soil_layers
    logical, intent(in)                    :: realizeAllImport
    logical, intent(in)                    :: realizeAllExport
    logical, intent(in)                    :: shareMemory
    integer, intent(out)                   :: rc
    ! local variables
    integer :: n
    logical :: realizeImport
    logical :: realizeExport
    type(ESMF_Field) :: field_import
    type(ESMF_Field) :: field_export
    real(ESMF_KIND_FIELD), pointer :: ptr_import(:,:,:)

    rc = ESMF_SUCCESS

    do n=lbound(fieldList,1),ubound(fieldList,1)
      ! check realize import
      if (realizeAllImport .AND. fieldList(n)%ad_import) then
        realizeImport = .true.
      else if (fieldList(n)%ad_import) then
        realizeImport = NUOPC_IsConnected(importState, &
          fieldName=trim(fieldList(n)%st_name),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      else
        realizeImport = .false.
      end if
      ! check realize export
      if (realizeAllExport .AND. fieldList(n)%ad_export) then
        realizeExport = .true.
      else if (fieldList(n)%ad_export) then
        realizeExport = NUOPC_IsConnected(exportState, &
          fieldName=trim(fieldList(n)%st_name),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      else
        realizeExport = .false.
      end if
      ! create import field
      if ( realizeImport ) then
        field_import=field_create_layers(grid=grid, &
          layers=num_soil_layers, &
          name=fieldList(n)%st_name, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      end if
      ! create export field
      if ( realizeExport .AND. realizeImport .AND. shareMemory ) then
        call ESMF_FieldGet(field_import, farrayPtr=ptr_import, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        field_export=field_create_shared(grid=grid, &
          farrayPtr=ptr_import, &
          name=fieldList(n)%st_name, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      else if( realizeExport ) then
        field_export=field_create_layers(grid=grid, &
          layers=num_soil_layers, &
          name=fieldList(n)%st_name, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      end if

      if (realizeImport) then
        call NUOPC_Realize(importState, field=field_import, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        fieldList(n)%rl_import = .true.
      else
        call ESMF_StateRemove(importState, (/fieldList(n)%st_name/), &
          relaxedflag=.true., rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        fieldList(n)%rl_import = .false.
      end if

      if (realizeExport) then
        call NUOPC_Realize(exportState, field=field_export, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        fieldList(n)%rl_export = .true.
      else
        call ESMF_StateRemove(exportState, (/fieldList(n)%st_name/), &
          relaxedflag=.true., rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        fieldList(n)%rl_export = .false.
      end if
    end do
  end subroutine

  !-----------------------------------------------------------------------------

  function field_create_layers(grid, layers, name, rc) result(field)
    type(ESMF_Grid), intent(in)         :: grid
    integer, intent(in)                 :: layers
    character(*), intent(in)            :: name
    integer, intent(out)                :: rc
    ! return value
    type(ESMF_Field)                    :: field
    ! local variables

    rc = ESMF_SUCCESS

    field = ESMF_FieldCreate(grid=grid, &
      typekind=ESMF_TYPEKIND_FIELD, &
      gridToFieldMap=(/1,3/), &
      ungriddedLBound=(/1/), &
      ungriddedUBound=(/layers/), &
      name=name, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
  end function

  !-----------------------------------------------------------------------------

  function field_create_shared(grid, farrayPtr, name, rc) result(field)
    type(ESMF_Grid), intent(in)         :: grid
    real(ESMF_KIND_FIELD), pointer      :: farrayPtr(:,:,:)
    character(*), intent(in)            :: name
    integer, intent(out)                :: rc
    ! return value
    type(ESMF_Field)                    :: field
    ! local variables

    rc = ESMF_SUCCESS

    field = ESMF_FieldCreate(grid=grid, &
      farrayPtr=farrayPtr, &
      gridToFieldMap=(/1,3/), &
      name=name, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
  end function

  !-----------------------------------------------------------------------------

  subroutine field_advertise(fieldList, importState, exportState, &
  transferOffer, rc)
    type(pf_nuopc_fld_type), intent(in) :: fieldList(:)
    type(ESMF_State), intent(inout)     :: importState
    type(ESMF_State), intent(inout)     :: exportState
    character(*), intent(in),optional   :: transferOffer
    integer, intent(out)                :: rc
    ! local variables
    integer :: n

    rc = ESMF_SUCCESS

    do n=lbound(fieldList,1),ubound(fieldList,1)
      if (fieldList(n)%ad_import) then
        call NUOPC_Advertise(importState, &
          StandardName=fieldList(n)%sd_name, &
          Units=fieldList(n)%units, &
          TransferOfferGeomObject=transferOffer, &
          name=fieldList(n)%st_name, &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      end if
      if (fieldList(n)%ad_export) then
        call NUOPC_Advertise(exportState, &
          StandardName=fieldList(n)%sd_name, &
          Units=fieldList(n)%units, &
          TransferOfferGeomObject=transferOffer, &
          name=fieldList(n)%st_name, &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      end if
    end do

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine field_advertise_log(fieldList, cname, rc)
    type(pf_nuopc_fld_type), intent(in) :: fieldList(:)
    character(*), intent(in)            :: cname
    integer, intent(out)                :: rc
    ! local variables
    integer                    :: cntImp
    integer                    :: cntExp
    integer                    :: n
    character(32)              :: label
    character(ESMF_MAXSTR)     :: logMsg

    rc = ESMF_SUCCESS

    label = trim(cname)

    ! count advertised import and export fields
    cntImp = 0
    cntExp = 0
    do n = lbound(fieldList,1), ubound(fieldList,1)
      if (fieldList(n)%ad_import) cntImp = cntImp + 1
      if (fieldList(n)%ad_export) cntExp = cntExp + 1
    enddo

    ! log advertised import fields
    write(logMsg,'(a,a,i0,a)') trim(label)//': ', &
      'List of advertised import fields(',cntImp,'):'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') trim(label)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    cntImp = 0
    do n=lbound(fieldList,1), ubound(fieldList,1)
      if (.NOT.fieldList(n)%ad_import) cycle
      cntImp = cntImp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') trim(label)//': ', &
        cntImp,' ',trim(fieldList(n)%st_name), &
        ' ',trim(fieldList(n)%sd_name)
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    enddo

    ! log advertised export fields
    write(logMsg,'(a,a,i0,a)') trim(label)//': ', &
      'List of advertised export fields(',cntExp,'):'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') trim(label)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    cntExp = 0
    do n=lbound(fieldList,1), ubound(fieldList,1)
      if (.NOT.fieldList(n)%ad_export) cycle
      cntExp = cntExp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') trim(label)//': ', &
        cntExp,' ',trim(fieldList(n)%st_name), &
        ' ',trim(fieldList(n)%sd_name)
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine field_realize_log(fieldList, cname, rc)
    type(pf_nuopc_fld_type), intent(in) :: fieldList(:)
    character(*), intent(in)            :: cname
    integer, intent(out)                :: rc
    ! local variables
    integer                    :: cntImp
    integer                    :: cntExp
    integer                    :: n
    character(32)              :: label
    character(ESMF_MAXSTR)     :: logMsg

    rc = ESMF_SUCCESS

    label = trim(cname)

    ! count realized import and export fields
    cntImp = 0
    cntExp = 0
    do n = lbound(fieldList,1), ubound(fieldList,1)
      if (fieldList(n)%rl_import) cntImp = cntImp + 1
      if (fieldList(n)%rl_export) cntExp = cntExp + 1
    enddo

    ! log realized import fields
    write(logMsg,'(a,a,i0,a)') trim(label)//': ', &
      'List of realized import fields(',cntImp,'):'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') trim(label)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    cntImp = 0
    do n=lbound(fieldList,1), ubound(fieldList,1)
      if (.NOT.fieldList(n)%rl_import) cycle
      cntImp = cntImp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') trim(label)//': ', &
        cntImp,' ',trim(fieldList(n)%st_name), &
        ' ',trim(fieldList(n)%sd_name)
      call ESMF_LogWrite(trim(LogMsg), ESMF_LOGMSG_INFO)
    enddo

    ! log realized export fields
    write(logMsg,'(a,a,i0,a)') trim(label)//': ', &
      'List of realized export fields(',cntExp,'):'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') trim(label)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    cntExp = 0
    do n=lbound(fieldList,1), ubound(fieldList,1)
      if (.NOT.fieldList(n)%rl_export) cycle
      cntExp = cntExp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') trim(label)//': ', &
        cntExp,' ',trim(fieldList(n)%st_name), &
        ' ',trim(fieldList(n)%sd_name)
      call ESMF_LogWrite(trim(LogMsg), ESMF_LOGMSG_INFO)
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine field_find_standardname(fieldList, standardName, location, &
  defaultValue, rc)
    type(pf_nuopc_fld_type), intent(in)     :: fieldList(:)
    character(len=64), intent(in)           :: standardName
    integer, intent(out), optional          :: location
    real(ESMF_KIND_R8),intent(out),optional :: defaultValue
    integer, intent(out)                    :: rc
    ! local variables
    integer :: n

    rc = ESMF_RC_NOT_FOUND

    if (present(location)) location = lbound(fieldList,1) - 1
    if (present(defaultValue)) defaultValue = ESMF_DEFAULT_VALUE

    do n=lbound(fieldList,1),ubound(fieldList,1)
      if (fieldList(n)%sd_name .eq. standardName) then
        if (present(location)) location = n
        if (present(defaultValue)) defaultValue = fieldList(n)%vl_default
        rc = ESMF_SUCCESS
        return
      end if
    end do

    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg="Field not found in fieldList "//trim(standardName), &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine field_find_statename(fieldList, stateName, location, &
  defaultValue, rc)
    type(pf_nuopc_fld_type), intent(in)     :: fieldList(:)
    character(len=64), intent(in)           :: stateName
    integer, intent(out), optional          :: location
    real(ESMF_KIND_R8),intent(out),optional :: defaultValue
    integer, intent(out)                    :: rc
    ! local variables
    integer :: n

    rc = ESMF_RC_NOT_FOUND

    if (present(location)) location = lbound(fieldList,1) - 1
    if (present(defaultValue)) defaultValue = ESMF_DEFAULT_VALUE

    do n=lbound(fieldList,1),ubound(fieldList,1)
      if (fieldList(n)%st_name .eq. stateName) then
        if (present(location)) location = n
        if (present(defaultValue)) defaultValue = fieldList(n)%vl_default
        rc = ESMF_SUCCESS
        return
      end if
    end do

    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg="Field not found in fieldList "//trim(stateName), &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine field_fill_state(state, fill_type, fieldList, fillValue, rc)
    type(ESMF_State), intent(inout)               :: state
    type(field_init_flag), intent(in)             :: fill_type
    type(pf_nuopc_fld_type), intent(in), optional :: fieldList(:)
    real(ESMF_KIND_R8), intent(in), optional      :: fillValue
    integer, intent(out)                          :: rc
    ! local variables
    integer                                :: n
    integer                                :: itemCount
    character(len=64),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_Field)                       :: field
    real(ESMF_KIND_R8)                     :: defaultValue
    integer                                :: stat

    rc = ESMF_SUCCESS

    call ESMF_StateGet(state,itemCount=itemCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate(itemNameList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item name memory failed.", &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(itemTypeList(itemCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item type memory failed.", &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_StateGet(state,itemNameList=itemNameList, &
      itemTypeList=itemTypeList,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if ( fill_type .eq. FLD_INIT_ZERO ) then
      do n=1, itemCount
        if ( itemTypeList(n) == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(state, field=field, &
            itemName=itemNameList(n),rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
          call ESMF_FieldFill(field, dataFillScheme="const", &
            const1=0.0_ESMF_KIND_R8, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
        endif
      enddo
    else if ( fill_type .eq. FLD_INIT_FILLV ) then
      if (.not. present(fillValue)) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Missing fillValue for FLD_INIT_FILLV.", &
          line=__LINE__,file=__FILE__,rcToReturn=rc)
        return  ! bail out
      end if
      do n=1, itemCount
        if ( itemTypeList(n) == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(state, field=field, &
            itemName=itemNameList(n),rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
          call ESMF_FieldFill(field, dataFillScheme="const", &
            const1=fillValue, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
        endif
      enddo
    else if ( fill_type .eq. FLD_INIT_DEFAULT ) then
      if (.not. present(fieldList)) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Missing fieldList for FLD_INIT_DEFAULT.", &
          line=__LINE__,file=__FILE__,rcToReturn=rc)
        return  ! bail out
      end if
      do n=1, itemCount
        if ( itemTypeList(n) == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(state, field=field, &
            itemName=itemNameList(n),rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
          call field_find_statename(fieldList, itemNameList(n), &
            defaultValue=defaultValue, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
          call ESMF_FieldFill(field, dataFillScheme="const", &
            const1=defaultValue, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
        endif
      enddo
    else
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="Unsupported fill_type for field_fill_state", &
        line=__LINE__,file=__FILE__,rcToReturn=rc)
      return  ! bail out
    end if

    deallocate(itemNameList)
    deallocate(itemTypeList)

  end subroutine field_fill_state

  !-----------------------------------------------------------------------------

end module parflow_nuopc_fields
