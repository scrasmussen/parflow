module pf_nuopc_test_lnd

  !-----------------------------------------------------------------------------
  ! ATM Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS        => SetServices, &
    model_label_CheckImport => label_CheckImport, &
    model_label_Advance     => label_Advance

  implicit none

  private

  public SetServices

  integer, parameter :: nx = 64
  integer, parameter :: ny = 32
  integer, parameter :: nz = 4
  real(ESMF_KIND_R4), parameter :: filvalue = -1.0E34

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! derive generic model phases
    call NUOPC_CompDerive(model, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! set entry points for initialization phases
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p2"/), userRoutine=InitializeP2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! set entry points for specialized methods
    call ESMF_MethodRemove(model, label=model_label_CheckImport, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_CompSpecialize(model, specLabel=model_label_CheckImport, &
       specRoutine=CheckImport, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_CompSpecialize(model, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
  end subroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine InitializeP1(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! advertise import fields
    call NUOPC_Advertise(importState, &
      StandardName="parflow_flux", name="pf_flux", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_Advertise(importState, &
      StandardName="parflow_porosity", name="pf_porosity", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_Advertise(importState, &
      StandardName="parflow_pressure", name="pf_pressure", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_Advertise(importState, &
      StandardName="parflow_saturation", name="pf_saturation", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! advertise export fields
    call NUOPC_Advertise(exportState, &
      StandardName="parflow_flux", name="pf_flux", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_Advertise(exportState, &
      StandardName="parflow_porosity", name="pf_porosity", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_Advertise(exportState, &
      StandardName="parflow_pressure", name="pf_pressure", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_Advertise(exportState, &
      StandardName="parflow_saturation", name="pf_saturation", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
  end subroutine InitializeP1

  !-----------------------------------------------------------------------------

  subroutine InitializeP2(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    ! local variables
    character(len=64)       :: value
    integer                 :: diagnostic
    character(len=64)       :: geom_type
    type(ESMF_Field)        :: fld_imp_flux
    type(ESMF_Field)        :: fld_imp_porosity
    type(ESMF_Field)        :: fld_imp_pressure
    type(ESMF_Field)        :: fld_imp_saturation
    type(ESMF_Field)        :: fld_exp_flux
    type(ESMF_Field)        :: fld_exp_porosity
    type(ESMF_Field)        :: fld_exp_pressure
    type(ESMF_Field)        :: fld_exp_saturation
    type(ESMF_Grid)         :: lnd_grid

    rc = ESMF_SUCCESS

    ! diagnostic
    call ESMF_AttributeGet(model, name="Diagnostic", &
      value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"max ","high","low ","off "/), &
      specialValueList= (/ 65535, 65535, 65535,     0/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! field geom type
    call ESMF_AttributeGet(model, name="geom", &
      value=geom_type, defaultValue="FLD_GEOM_RGNLCARTESIAN", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! create grid
    if (geom_type .eq. "FLD_GEOM_RGNLCARTESIAN") then
      lnd_grid = ESMF_GridCreateNoPeriDimUfrm(name="LND-Grid", &
        minIndex=(/1, 1/), &
        maxIndex=(/nx, ny/), &
        minCornerCoord=(/    0._ESMF_KIND_R8,     0._ESMF_KIND_R8/), &
        maxCornerCoord=(/64000._ESMF_KIND_R8, 32000._ESMF_KIND_R8/), &
        staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
        coordSys=ESMF_COORDSYS_CART, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
    elseif (geom_type .eq. "FLD_GEOM_GLBLSPHDEG") then
      lnd_grid = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/nx, ny/), &
        minCornerCoord=(/0._ESMF_KIND_R8, -50._ESMF_KIND_R8/), &
        maxCornerCoord=(/360._ESMF_KIND_R8, 70._ESMF_KIND_R8/), &
        staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
        name="LND-Grid", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
    else
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="Unsupported geom type", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! write grid to NetCDF file
    if (btest(diagnostic,16)) then
      call Grid_Diag(lnd_grid, "diagnostic_LND_InitializeP2_grid.nc", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
    endif

    ! create import fields
    fld_imp_flux = ESMF_FieldCreate(name="pf_flux", grid=lnd_grid, &
      typekind=ESMF_TYPEKIND_R4, gridToFieldMap=(/1,3/), &
      ungriddedLBound=(/1/), ungriddedUBound=(/nz/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    fld_imp_porosity = ESMF_FieldCreate(name="pf_porosity", grid=lnd_grid, &
      typekind=ESMF_TYPEKIND_R4, gridToFieldMap=(/1,3/), &
      ungriddedLBound=(/1/), ungriddedUBound=(/nz/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    fld_imp_pressure = ESMF_FieldCreate(name="pf_pressure", grid=lnd_grid, &
      typekind=ESMF_TYPEKIND_R4, gridToFieldMap=(/1,3/), &
      ungriddedLBound=(/1/), ungriddedUBound=(/nz/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    fld_imp_saturation = ESMF_FieldCreate(name="pf_saturation", grid=lnd_grid, &
      typekind=ESMF_TYPEKIND_R4, gridToFieldMap=(/1,3/), &
      ungriddedLBound=(/1/), ungriddedUBound=(/nz/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! realize import fields
    call NUOPC_Realize(importState, field=fld_imp_flux, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_Realize(importState, field=fld_imp_porosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_Realize(importState, field=fld_imp_pressure, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_Realize(importState, field=fld_imp_saturation, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! create export fields
    fld_exp_flux = ESMF_FieldCreate(name="pf_flux", grid=lnd_grid, &
      typekind=ESMF_TYPEKIND_R4, gridToFieldMap=(/1,3/), &
      ungriddedLBound=(/1/), ungriddedUBound=(/nz/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    fld_exp_porosity = ESMF_FieldCreate(name="pf_porosity", grid=lnd_grid, &
      typekind=ESMF_TYPEKIND_R4, gridToFieldMap=(/1,3/), &
      ungriddedLBound=(/1/), ungriddedUBound=(/nz/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    fld_exp_pressure = ESMF_FieldCreate(name="pf_pressure", grid=lnd_grid, &
      typekind=ESMF_TYPEKIND_R4, gridToFieldMap=(/1,3/), &
      ungriddedLBound=(/1/), ungriddedUBound=(/nz/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    fld_exp_saturation = ESMF_FieldCreate(name="pf_saturation", grid=lnd_grid, &
      typekind=ESMF_TYPEKIND_R4, gridToFieldMap=(/1,3/), &
      ungriddedLBound=(/1/), ungriddedUBound=(/nz/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! realize export fields
    call NUOPC_Realize(exportState, field=fld_exp_flux, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_Realize(exportState, field=fld_exp_porosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_Realize(exportState, field=fld_exp_pressure, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_Realize(exportState, field=fld_exp_saturation, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! initialize import fields
    call ESMF_FieldFill(fld_imp_flux, dataFillScheme="const", &
      const1=real(filvalue,ESMF_KIND_R8), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_FieldFill(fld_imp_porosity, dataFillScheme="const", &
      const1=real(filvalue,ESMF_KIND_R8), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_FieldFill(fld_imp_pressure, dataFillScheme="const", &
      const1=real(filvalue,ESMF_KIND_R8), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_FieldFill(fld_imp_saturation, dataFillScheme="const", &
      const1=real(filvalue,ESMF_KIND_R8), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! initialize export fields
    call ESMF_FieldFill(fld_exp_flux, dataFillScheme="const", &
      const1=0.1_ESMF_KIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_FieldFill(fld_exp_porosity, dataFillScheme="const", &
      const1=0.1_ESMF_KIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_FieldFill(fld_exp_pressure, dataFillScheme="const", &
      const1=0.1_ESMF_KIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_FieldFill(fld_exp_saturation, dataFillScheme="const", &
      const1=0.1_ESMF_KIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
  end subroutine InitializeP2

  !-----------------------------------------------------------------------------

  subroutine CheckImport(model, rc)
    type(ESMF_GridComp) :: model
    integer,intent(out) :: rc
    ! local variables
    character(32)               :: cname
    type(ESMF_Clock)            :: modelClock
    type(ESMF_Time)             :: modelCurrTime
    type(ESMF_State)            :: importState
    logical                     :: allCurrTime

    rc = ESMF_SUCCESS

    ! query component for name
    call ESMF_GridCompGet(model, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! query the component for its clock and import state
    call NUOPC_ModelGet(model, modelClock=modelClock, &
      importState=importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! get the stop time out of the clock
    call ESMF_ClockGet(modelClock, currTime=modelCurrTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    allCurrTime = NUOPC_IsAtTime(importState, modelCurrTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    if (.NOT.allCurrTime) then
      call ESMF_LogWrite(trim(cname)//": "// &
        "NUOPC INCOMPATIBILITY DETECTED: Import Fields not at current time", &
        ESMF_LOGMSG_WARNING)
    endif
  end subroutine CheckImport

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    ! local variables
    type(ESMF_VM)    :: vm
    integer          :: localPet
    logical, save    :: first_step = .true.
    type(ESMF_Clock) :: modelClock
    type(ESMF_State) :: importState
    type(ESMF_State) :: exportState
    type(ESMF_Field) :: fld_imp_flux
    type(ESMF_Field) :: fld_imp_porosity
    type(ESMF_Field) :: fld_imp_pressure
    type(ESMF_Field) :: fld_imp_saturation
    type(ESMF_Field) :: fld_exp_flux
    type(ESMF_Field) :: fld_exp_porosity
    type(ESMF_Field) :: fld_exp_pressure
    type(ESMF_Field) :: fld_exp_saturation
    real(ESMF_KIND_R4), pointer :: ptr_flux(:,:,:)
    real(ESMF_KIND_R4), pointer :: ptr_porosity(:,:,:)
    real(ESMF_KIND_R4), pointer :: ptr_pressure(:,:,:)
    real(ESMF_KIND_R4), pointer :: ptr_saturation(:,:,:)
    real(ESMF_KIND_R4) :: lsum_flux(1),       gsum_flux(1)
    real(ESMF_KIND_R4) :: lsum_porosity(1),   gsum_porosity(1)
    real(ESMF_KIND_R4) :: lsum_pressure(1),   gsum_pressure(1)
    real(ESMF_KIND_R4) :: lsum_saturation(1), gsum_saturation(1)
    character(len=160) :: clockString

    rc = ESMF_SUCCESS

    ! query component for vm and local pet
    call ESMF_GridCompGet(model, vm=vm, localPet=localPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! query component for import and export states
    call NUOPC_ModelGet(model, modelClock=modelClock, &
      importState=importState, exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_ClockPrint(modelClock, options="currTime", &
      unit=clockString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! query import state for fields
    call ESMF_StateGet(importState, itemName="pf_flux", &
      field=fld_imp_flux, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_StateGet(importState, itemName="pf_porosity", &
      field=fld_imp_porosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_StateGet(importState, itemName="pf_pressure", &
      field=fld_imp_pressure, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_StateGet(importState, itemName="pf_saturation", &
      field=fld_imp_saturation, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! query export state for fields
    call ESMF_StateGet(exportState, itemName="pf_flux", &
      field=fld_exp_flux, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_StateGet(exportState, itemName="pf_porosity", &
      field=fld_exp_porosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_StateGet(exportState, itemName="pf_pressure", &
      field=fld_exp_pressure, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_StateGet(exportState, itemName="pf_saturation", &
      field=fld_exp_saturation, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! query fields for array pointers
    call ESMF_FieldGet(fld_imp_flux, farrayPtr=ptr_flux, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_FieldGet(fld_imp_porosity, farrayPtr=ptr_porosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_FieldGet(fld_imp_pressure, farrayPtr=ptr_pressure, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_FieldGet(fld_imp_saturation, farrayPtr=ptr_saturation, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! sum data from all PETs
    lsum_flux=sum(ptr_flux,ptr_flux.ne.filvalue)
    call ESMF_VMReduce(vm=vm, sendData=lsum_flux, &
      recvData=gsum_flux, count=1, &
      reduceflag=ESMF_REDUCE_SUM, rootPet=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    lsum_porosity=sum(ptr_porosity,ptr_porosity.ne.filvalue)
    call ESMF_VMReduce(vm=vm, sendData=lsum_porosity, &
      recvData=gsum_porosity, count=1, &
      reduceflag=ESMF_REDUCE_SUM, rootPet=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    lsum_pressure=sum(ptr_pressure,ptr_pressure.ne.filvalue)
    call ESMF_VMReduce(vm=vm, sendData=lsum_pressure, &
      recvData=gsum_pressure, count=1, &
      reduceflag=ESMF_REDUCE_SUM, rootPet=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    lsum_saturation=sum(ptr_saturation,ptr_saturation.ne.filvalue)
    call ESMF_VMReduce(vm=vm, sendData=lsum_saturation, &
      recvData=gsum_saturation, count=1, &
      reduceflag=ESMF_REDUCE_SUM, rootPet=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! print import field sums
    if (localPet .eq. 0) then
      print *,"LND Import Sums"
      print *,"  clock=",trim(clockString)
      print *,"  sum(pf_flux)=",gsum_flux(1)
      print *,"  sum(pf_porosity)=",gsum_porosity(1)
      print *,"  sum(pf_pressure)=",gsum_pressure(1)
      print *,"  sum(pf_saturation)=",gsum_saturation(1)
    end if

    if (first_step) then
      first_step = .false.
    else
      ! copy import to export
      call ESMF_FieldCopy(fld_exp_flux, fieldIn=fld_imp_flux, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_FieldCopy(fld_exp_porosity, fieldIn=fld_imp_porosity, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_FieldCopy(fld_exp_pressure, fieldIn=fld_imp_pressure, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_FieldCopy(fld_exp_saturation, fieldIn=fld_imp_saturation, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
    end if
  end subroutine ModelAdvance

  subroutine Grid_Diag(grid, fileName, overwrite, status, timeslice, iofmt, &
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
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        lfileName = trim(gridName)//".nc"
      endif

      arraybundle = ESMF_ArrayBundleCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return

      ! -- centers --

      call ESMF_GridGetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        isPresent=isPresent, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (isPresent) then
        call ESMF_GridGetCoord(grid, coordDim=1, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_ArraySet(array, name="lon_center", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_GridGetCoord(grid, coordDim=2, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_ArraySet(array, name="lat_center", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
      endif

      ! -- corners --

      call ESMF_GridGetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CORNER, &
        isPresent=hasCorners, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (hasCorners) then
        call ESMF_GridGetCoord(grid, coordDim=1, &
          staggerLoc=ESMF_STAGGERLOC_CORNER, array=array, rc=rc)
        if (.not. ESMF_LogFoundError(rcToCheck=rc)) then
          call ESMF_ArraySet(array, name="lon_corner", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return
          call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return
        endif
        call ESMF_GridGetCoord(grid, coordDim=2, &
          staggerLoc=ESMF_STAGGERLOC_CORNER, array=array, rc=rc)
        if (.not. ESMF_LogFoundError(rcToCheck=rc)) then
          call ESMF_ArraySet(array, name="lat_corner", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return
          call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return
        endif
      endif

      ! -- mask --

      call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_MASK, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (isPresent) then
        call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
          itemflag=ESMF_GRIDITEM_MASK, array=array, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_ArraySet(array, name="mask", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
      endif

      ! -- area --

      call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_AREA, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (isPresent) then
        call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
          itemflag=ESMF_GRIDITEM_AREA, array=array, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_ArraySet(array, name="area", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
        call ESMF_ArrayBundleAdd(arraybundle,(/array/),rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
      endif

      call ESMF_ArrayBundleWrite(arraybundle, &
        fileName=trim(lfileName),rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return

      call ESMF_ArrayBundleDestroy(arraybundle,rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
    endif
  end subroutine

end module pf_nuopc_test_lnd
