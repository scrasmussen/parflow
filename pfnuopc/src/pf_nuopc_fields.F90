#include "pf_nuopc_macros.h"

module parflow_nuopc_fields

  use ESMF
  use NUOPC
  use parflow_nuopc_flags
  use iso_c_binding, only: c_null_char, c_int, c_double, c_float

  implicit none

  private

  type pf_fld_type
    sequence
    character(len=64)           :: st_name    = "dummy" ! state name
    character(len=64)           :: units      = "-"     ! units
  end type pf_fld_type

  type pf_nuopc_fld_type
    sequence
    character(len=64)           :: sd_name    = "dummy" ! standard name
    character(len=64)           :: st_name    = "dummy" ! state name
    character(len=64)           :: units      = "-"     ! units
    logical*8                   :: layers     = .FALSE. ! layered field
    logical                     :: ad_import  = .FALSE. ! advertise import
    logical                     :: ad_export  = .FALSE. ! advertise export
    logical                     :: rl_import  = .FALSE. ! realize import
    logical                     :: rl_export  = .FALSE. ! realize export
    real(ESMF_KIND_R8)          :: vl_default = ESMF_DEFAULT_VALUE ! default value
  end type pf_nuopc_fld_type

  type(pf_fld_type),target,dimension(4) :: pf_internal_fld_list = (/ &
    pf_fld_type("PF_FLUX      ", "m h-1"), &
    pf_fld_type("PF_POROSITY  ", "-    "), &
    pf_fld_type("PF_PRESSURE  ", "m    "), &
    pf_fld_type("PF_SATURATION", "-    ") /)

  type(pf_nuopc_fld_type),target,dimension(21) :: pf_nuopc_fld_list = (/     &
    pf_nuopc_fld_type("total_water_flux                        ", &
      "FLUX      ", "kg m-2 s-1",  .TRUE.,  .TRUE., .FALSE.), &
    pf_nuopc_fld_type("total_water_flux_layer_1                ", &
      "FLUX1     ", "kg m-2 s-1", .FALSE.,  .TRUE., .FALSE.), &
    pf_nuopc_fld_type("total_water_flux_layer_2                ", &
      "FLUX2     ", "kg m-2 s-1", .FALSE.,  .TRUE., .FALSE.), &
    pf_nuopc_fld_type("total_water_flux_layer_3                ", &
      "FLUX3     ", "kg m-2 s-1", .FALSE.,  .TRUE., .FALSE.), &
    pf_nuopc_fld_type("total_water_flux_layer_4                ", &
      "FLUX4     ", "kg m-2 s-1", .FALSE.,  .TRUE., .FALSE.), &
    pf_nuopc_fld_type("precip_drip                             ", &
      "PCPDRP    ", "kg m-2 s-1", .FALSE.,  .TRUE., .FALSE.), &
    pf_nuopc_fld_type("bare_soil_evaporation                   ", &
      "EDIR      ", "W m-2     ", .FALSE.,  .TRUE., .FALSE.), &
    pf_nuopc_fld_type("vegetation_transpiration                ", &
      "ET        ", "W m-2     ",  .TRUE.,  .TRUE., .FALSE.), &
    pf_nuopc_fld_type("porosity                                ", &
      "POROSITY  ", "-         ",  .TRUE., .FALSE.,  .TRUE.), &
    pf_nuopc_fld_type("pressure                                ", &
      "PRESSURE  ", "m         ",  .TRUE., .FALSE.,  .TRUE.), &
    pf_nuopc_fld_type("saturation                              ", &
      "SATURATION", "-         ",  .TRUE., .FALSE.,  .TRUE.), &
    pf_nuopc_fld_type("soil_moisture_fraction                  ", &
      "SMOIS     ", "-         ",  .TRUE., .FALSE.,  .TRUE.), &
    pf_nuopc_fld_type("soil_moisture_fraction_layer_1          ", &
      "SMOIS1     ", "-        ", .FALSE., .FALSE.,  .TRUE.), &
    pf_nuopc_fld_type("soil_moisture_fraction_layer_2          ", &
      "SMOIS2     ", "-        ", .FALSE., .FALSE.,  .TRUE.), &
    pf_nuopc_fld_type("soil_moisture_fraction_layer_3          ", &
      "SMOIS3     ", "-        ", .FALSE., .FALSE.,  .TRUE.), &
    pf_nuopc_fld_type("soil_moisture_fraction_layer_4          ", &
      "SMOIS4     ", "-        ", .FALSE., .FALSE.,  .TRUE.), &
    pf_nuopc_fld_type("liquid_fraction_of_soil_moisture        ", &
      "SH2O       ", "-        ",  .TRUE., .FALSE.,  .TRUE.), &
    pf_nuopc_fld_type("liquid_fraction_of_soil_moisture_layer_1", &
      "SH2O1      ", "-        ", .FALSE., .FALSE.,  .TRUE.), &
    pf_nuopc_fld_type("liquid_fraction_of_soil_moisture_layer_2", &
      "SH2O2      ", "-        ", .FALSE., .FALSE.,  .TRUE.), &
    pf_nuopc_fld_type("liquid_fraction_of_soil_moisture_layer_3", &
      "SH2O3      ", "-        ", .FALSE., .FALSE.,  .TRUE.), &
    pf_nuopc_fld_type("liquid_fraction_of_soil_moisture_layer_4", &
      "SH2O4      ", "-        ", .FALSE., .FALSE.,  .TRUE.) /)

  public pf_internal_fld_list
  public pf_nuopc_fld_list
  public field_init_internal
  public field_advertise
  public field_realize
  public field_advertise_log
  public field_realize_log
  public field_fill_state
  public field_prep_import
  public field_prep_export

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

  subroutine field_init_internal(fieldList, internalState, grid, &
  num_soil_layers, rc)
    type(pf_fld_type), intent(inout) :: fieldList(:)
    type(ESMF_State), intent(inout)  :: internalState
    type(ESMF_Grid), intent(in)      :: grid
    integer, intent(in)              :: num_soil_layers
    integer, intent(out)             :: rc
    ! local variables
    logical :: isCreated
    integer :: n
    type(ESMF_Field) :: field

    rc = ESMF_SUCCESS

    ! check state
    isCreated = ESMF_StateIsCreated(internalState, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    if (.not. isCreated) then
      internalState = ESMF_StateCreate(rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    ! create internal fields
    do n=lbound(fieldList,1),ubound(fieldList,1)
      field=field_create_layers(grid=grid, &
        layers=num_soil_layers, &
        name=fieldList(n)%st_name, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call ESMF_StateAdd(internalState, (/ field /), rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    end do
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine field_realize(fieldList, importState, exportState, grid, &
  num_soil_layers, realizeAllImport, realizeAllExport, rc)
    type(pf_nuopc_fld_type), intent(inout) :: fieldList(:)
    type(ESMF_State), intent(inout)        :: importState
    type(ESMF_State), intent(inout)        :: exportState
    type(ESMF_Grid), intent(in)            :: grid
    integer, intent(in)                    :: num_soil_layers
    logical, intent(in)                    :: realizeAllImport
    logical, intent(in)                    :: realizeAllExport
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
      if (fieldList(n)%ad_import) then
        if (realizeAllImport) then
          realizeImport = .true.
        else
          realizeImport = NUOPC_IsConnected(importState, &
            fieldName=trim(fieldList(n)%st_name),rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        endif
      else
        realizeImport = .false.
      end if
      ! check realize export
      if (fieldList(n)%ad_export) then
        if (realizeAllExport) then
          realizeExport = .true.
        else
          realizeExport = NUOPC_IsConnected(exportState, &
            fieldName=trim(fieldList(n)%st_name),rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        endif
      else
        realizeExport = .false.
      end if
      ! create import field
      if ( realizeImport ) then
        if (fieldList(n)%layers) then
          field_import=field_create_layers(grid=grid, &
            layers=num_soil_layers, &
            name=fieldList(n)%st_name, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        else
          field_import=field_create(grid=grid, &
            name=fieldList(n)%st_name, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        endif
        call NUOPC_Realize(importState, field=field_import, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        fieldList(n)%rl_import = .true.
      else
        call ESMF_StateRemove(importState, (/fieldList(n)%st_name/), &
          relaxedflag=.true., rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        fieldList(n)%rl_import = .false.
      end if
      ! create export field
      if( realizeExport ) then
        if (fieldList(n)%layers) then
          field_export=field_create_layers(grid=grid, &
            layers=num_soil_layers, &
            name=fieldList(n)%st_name, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        else
          field_export=field_create(grid=grid, &
            name=fieldList(n)%st_name, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        endif
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

  function field_create(grid, name, rc) result(field)
    type(ESMF_Grid), intent(in)         :: grid
    character(*), intent(in)            :: name
    integer, intent(out)                :: rc
    ! return value
    type(ESMF_Field)                    :: field
    ! local variables

    rc = ESMF_SUCCESS

    field = ESMF_FieldCreate(grid=grid, &
      typekind=ESMF_TYPEKIND_FIELD, &
      name=name, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
  end function

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

  subroutine field_prep_import(importState, dz, internalState, forcType, rc)
    type(ESMF_State), intent(in)    :: importState
    real, allocatable, intent(in)   :: dz(:)
    type(ESMF_State), intent(inout) :: internalState
    type(forcing_flag), intent(out) :: forcType
    integer, intent(out)            :: rc

    ! local variables
    integer :: s_flx, s_flx1, s_flx2, s_flx3, s_flx4
    type(ESMF_Field) :: fld_pf_flux
    type(ESMF_Field) :: fld_imp_flux
    type(ESMF_Field) :: fld_imp_flux1
    type(ESMF_Field) :: fld_imp_flux2
    type(ESMF_Field) :: fld_imp_flux3
    type(ESMF_Field) :: fld_imp_flux4
    type(ESMF_Field) :: fld_imp_pcpdrp
    type(ESMF_Field) :: fld_imp_edir
    type(ESMF_Field) :: fld_imp_et
    real(c_float), pointer :: ptr_pf_flux(:, :, :)
    real(c_float), pointer :: ptr_imp_flux(:, :, :)
    real(c_float), pointer :: ptr_imp_flux1(:, :)
    real(c_float), pointer :: ptr_imp_flux2(:, :)
    real(c_float), pointer :: ptr_imp_flux3(:, :)
    real(c_float), pointer :: ptr_imp_flux4(:, :)
    real(c_float), pointer :: ptr_imp_pcpdrp(:, :)
    real(c_float), pointer :: ptr_imp_edir(:, :)
    real(c_float), pointer :: ptr_imp_et(:, :, :)
    type(ESMF_StateItem_Flag) :: itemType
    integer          :: ungriddedLBound(1)
    integer          :: ungriddedUBound(1)
    integer          :: i
    real, parameter  :: LVH2O = 2.501E+6 ! heat of vaporization
    real, parameter  :: CNVMH = (3600d0/1000d0) ! convert mm/s to m/h

    rc = ESMF_SUCCESS
    forcType = FORCING_ERROR

    ! query internal state for pf fields
    call ESMF_StateGet(internalState, itemName="PF_FLUX", &
      field=fld_pf_flux, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_FieldGet(fld_pf_flux, farrayPtr=ptr_pf_flux, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    ! query flux field for soil layers
    call ESMF_FieldGet(fld_pf_flux, ungriddedLBound=ungriddedLBound, &
      ungriddedUBound=ungriddedUBound, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    if (.not.allocated(dz)) then
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="Missing soil layers thickness values.", &
        line=__LINE__,file=__FILE__,rcToReturn=rc)
      return  ! bail out
    elseif ((ungriddedUBound(1)-ungriddedLBound(1)+1).gt.size(dz)) then
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="Missing soil layers thickness values.", &
        line=__LINE__,file=__FILE__,rcToReturn=rc)
      return  ! bail out
    endif

    ! search import for total water flux
    call ESMF_StateGet(importState, itemSearch="FLUX", &
      itemCount=s_flx, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    if (s_flx .gt. 0) then
      ! query import state for pf fields
      call ESMF_StateGet(importState, itemName="FLUX", &
        field=fld_imp_flux, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call ESMF_FieldGet(fld_imp_flux, farrayPtr=ptr_imp_flux, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      do i=ungriddedLBound(1), ungriddedUBound(1)
        ptr_pf_flux(:,i,:) = ptr_imp_flux(:,i,:) * CNVMH / dz(i)
      enddo
      forcType = FORCING_WTRFLX3D
    else
      ! search import for total water flux (layers 1-4)
      call ESMF_StateGet(importState, itemSearch="FLUX1", &
        itemCount=s_flx1, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call ESMF_StateGet(importState, itemSearch="FLUX2", &
        itemCount=s_flx2, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call ESMF_StateGet(importState, itemSearch="FLUX3", &
        itemCount=s_flx3, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call ESMF_StateGet(importState, itemSearch="FLUX4", &
        itemCount=s_flx4, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      if ((s_flx1.gt.0) .and. (s_flx2.gt.0) .and. &
          (s_flx3.gt.0) .and. (s_flx4.gt.0)) then
        if ((ungriddedUBound(1)-ungriddedLBound(1)+1).ne.4) then
          call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
            msg="Unsupported number of soil layers.", &
            line=__LINE__,file=__FILE__,rcToReturn=rc)
          return  ! bail out
        endif
        ! query import state for pf fields
        call ESMF_StateGet(importState, itemName="FLUX1", &
          field=fld_imp_flux1, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_FieldGet(fld_imp_flux1, farrayPtr=ptr_imp_flux1, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_StateGet(importState, itemName="FLUX2", &
          field=fld_imp_flux2, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_FieldGet(fld_imp_flux2, farrayPtr=ptr_imp_flux2, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_StateGet(importState, itemName="FLUX3", &
          field=fld_imp_flux3, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_FieldGet(fld_imp_flux3, farrayPtr=ptr_imp_flux3, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_StateGet(importState, itemName="FLUX4", &
          field=fld_imp_flux4, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_FieldGet(fld_imp_flux4, farrayPtr=ptr_imp_flux4, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        ptr_pf_flux(:,(ungriddedLBound(1)  ),:) = ptr_imp_flux1 * CNVMH / dz(1)
        ptr_pf_flux(:,(ungriddedLBound(1)+1),:) = ptr_imp_flux2 * CNVMH / dz(2)
        ptr_pf_flux(:,(ungriddedLBound(1)+2),:) = ptr_imp_flux3 * CNVMH / dz(3)
        ptr_pf_flux(:,(ungriddedLBound(1)+3),:) = ptr_imp_flux4 * CNVMH / dz(4)
        forcType = FORCING_WTRFLX2D
      else ! calculate total water flux
        ! query import state for pf fields
        call ESMF_StateGet(importState, itemName="PCPDRP", &
          field=fld_imp_pcpdrp, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_FieldGet(fld_imp_pcpdrp, farrayPtr=ptr_imp_pcpdrp, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_StateGet(importState, itemName="EDIR", &
          field=fld_imp_edir, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_FieldGet(fld_imp_edir, farrayPtr=ptr_imp_edir, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_StateGet(importState, itemName="ET", &
          field=fld_imp_et, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_FieldGet(fld_imp_et, farrayPtr=ptr_imp_et, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        ! calculate total water flux
        i=ungriddedLBound(1)
        ptr_pf_flux(:,i,:) = (- ( (ptr_imp_edir(:,:) + ptr_imp_et(:,i,:)) &
                             / LVH2O ) - ptr_imp_pcpdrp(:,:) ) * CNVMH / dz(i)
        do i=ungriddedLBound(1)+1, ungriddedUBound(1)
          ptr_pf_flux(:,i,:) = - (ptr_imp_et(:,i,:)/LVH2O) * CNVMH / dz(i)
        enddo
        forcType = FORCING_COMPOSITE
      endif
    endif
  end subroutine field_prep_import

  !-----------------------------------------------------------------------------

  subroutine field_prep_export(exportState, internalState, rc)
    type(ESMF_State), intent(inout)               :: exportState
    type(ESMF_State), intent(inout)               :: internalState
    integer, intent(out)                          :: rc

    ! local variables
    type(ESMF_Field) :: fld_pf_pressure
    type(ESMF_Field) :: fld_pf_porosity
    type(ESMF_Field) :: fld_pf_saturation
    real(c_float), pointer :: ptr_pf_pressure(:, :, :)
    real(c_float), pointer :: ptr_pf_porosity(:, :, :)
    real(c_float), pointer :: ptr_pf_saturation(:, :, :)
    type(ESMF_Field) :: fld_export
    real(c_float), pointer :: ptr_export2d(:, :)
    real(c_float), pointer :: ptr_export3d(:, :, :)
    integer                               :: stat
    integer                               :: itemCount
    integer                               :: iIndex
    character(len=64),allocatable         :: itemNameList(:)
    type(ESMF_StateItem_Flag),allocatable :: itemTypeList(:)
    integer          :: ungriddedLBound(1)
    integer          :: ungriddedUBound(1)
    character(ESMF_MAXSTR) :: logMsg

    rc = ESMF_SUCCESS

    ! query internal state for pf fields
    call ESMF_StateGet(internalState, itemName="PF_PRESSURE", &
      field=fld_pf_pressure, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_FieldGet(fld_pf_pressure, farrayPtr=ptr_pf_pressure, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(internalState, itemName="PF_POROSITY", &
      field=fld_pf_porosity, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_FieldGet(fld_pf_porosity, farrayPtr=ptr_pf_porosity, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(internalState, itemName="PF_SATURATION", &
      field=fld_pf_saturation, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_FieldGet(fld_pf_saturation, farrayPtr=ptr_pf_saturation, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    ! query saturation field for soil layers
    call ESMF_FieldGet(fld_pf_saturation, ungriddedLBound=ungriddedLBound, &
      ungriddedUBound=ungriddedUBound, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_StateGet(exportState, itemCount=itemCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    if (itemCount > 0 ) then

      allocate(itemNameList(itemCount), itemTypeList(itemCount), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of item list memory failed.", &
        CONTEXT, rcToReturn=rc)) return  ! bail out
      call ESMF_StateGet(exportState, itemNameList=itemNameList, &
        itemTypeList=itemTypeList, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      do iIndex=1, itemCount
        if (itemTypeList(iIndex) == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(exportState, field=fld_export, &
            itemName=itemNameList(iIndex), rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
          select case (itemNameList(iIndex))
            case ('PRESSURE')
              call ESMF_FieldCopy(fld_export, fieldIn=fld_pf_pressure, rc=rc)
              if (ESMF_STDERRORCHECK(rc)) return  ! bail out
            case ('POROSITY')
              call ESMF_FieldCopy(fld_export, fieldIn=fld_pf_porosity, rc=rc)
              if (ESMF_STDERRORCHECK(rc)) return  ! bail out
            case ('SATURATION')
              call ESMF_FieldCopy(fld_export, fieldIn=fld_pf_saturation, rc=rc)
              if (ESMF_STDERRORCHECK(rc)) return  ! bail out
            case ('SMOIS','SH2O')
              call ESMF_FieldGet(fld_export, farrayPtr=ptr_export3d, rc=rc)
              if (ESMF_STDERRORCHECK(rc)) return  ! bail out
              ptr_export3d=ptr_pf_saturation*ptr_pf_porosity
            case ('SMOIS1','SH2O1')
              call ESMF_FieldGet(fld_export, farrayPtr=ptr_export2d, rc=rc)
              if (ESMF_STDERRORCHECK(rc)) return  ! bail out
              ptr_export2d=ptr_pf_saturation(:,ungriddedLBound(1)  ,:) &
                           * ptr_pf_porosity(:,ungriddedLBound(1),:)
            case ('SMOIS2','SH2O2')
              call ESMF_FieldGet(fld_export, farrayPtr=ptr_export2d, rc=rc)
              if (ESMF_STDERRORCHECK(rc)) return  ! bail out
              ptr_export2d=ptr_pf_saturation(:,ungriddedLBound(1)+1,:) &
                           * ptr_pf_porosity(:,ungriddedLBound(1)+1,:)
            case ('SMOIS3','SH2O3')
              call ESMF_FieldGet(fld_export, farrayPtr=ptr_export2d, rc=rc)
              if (ESMF_STDERRORCHECK(rc)) return  ! bail out
              ptr_export2d=ptr_pf_saturation(:,ungriddedLBound(1)+2,:) &
                           * ptr_pf_porosity(:,ungriddedLBound(1)+2,:)
            case ('SMOIS4','SH2O4')
              call ESMF_FieldGet(fld_export, farrayPtr=ptr_export2d, rc=rc)
              if (ESMF_STDERRORCHECK(rc)) return  ! bail out
              ptr_export2d=ptr_pf_saturation(:,ungriddedLBound(1)+3,:) &
                           * ptr_pf_porosity(:,ungriddedLBound(1)+3,:)
            case default
              call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
                msg="Unsupported export field: "//trim(itemNameList(iIndex)), &
                line=__LINE__,file=__FILE__,rcToReturn=rc)
              return  ! bail out
          endselect
        endif
      enddo

      deallocate(itemNameList)
      deallocate(itemTypeList)

    endif

  end subroutine field_prep_export

  !-----------------------------------------------------------------------------

end module parflow_nuopc_fields
