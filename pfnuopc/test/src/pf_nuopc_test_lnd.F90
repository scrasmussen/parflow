module pf_nuopc_test_lnd

  !-----------------------------------------------------------------------------
  ! ATM Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS    => SetServices, &
    model_label_Advance => label_Advance

  implicit none

  private

  public SetServices

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
    type(ESMF_Field)        :: fld_flux
    type(ESMF_Field)        :: fld_porosity
    type(ESMF_Field)        :: fld_pressure
    type(ESMF_Field)        :: fld_saturation
    type(ESMF_Grid)         :: lnd_grid

    rc = ESMF_SUCCESS

    ! create grid
    lnd_grid = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/100, 150/), &
      minCornerCoord=(/0._ESMF_KIND_R8, -50._ESMF_KIND_R8/), &
      maxCornerCoord=(/360._ESMF_KIND_R8, 70._ESMF_KIND_R8/), &
      staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
      name="LND-Grid", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! create fields
    fld_flux = ESMF_FieldCreate(name="pf_flux", grid=lnd_grid, &
      typekind=ESMF_TYPEKIND_R4, gridToFieldMap=(/1,3/), &
      ungriddedLBound=(/1/), ungriddedUBound=(/4/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    fld_porosity = ESMF_FieldCreate(name="pf_porosity", grid=lnd_grid, &
      typekind=ESMF_TYPEKIND_R4, gridToFieldMap=(/1,3/), &
      ungriddedLBound=(/1/), ungriddedUBound=(/4/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    fld_pressure = ESMF_FieldCreate(name="pf_pressure", grid=lnd_grid, &
      typekind=ESMF_TYPEKIND_R4, gridToFieldMap=(/1,3/), &
      ungriddedLBound=(/1/), ungriddedUBound=(/4/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    fld_saturation = ESMF_FieldCreate(name="pf_saturation", grid=lnd_grid, &
      typekind=ESMF_TYPEKIND_R4, gridToFieldMap=(/1,3/), &
      ungriddedLBound=(/1/), ungriddedUBound=(/4/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! realize import fields
    call NUOPC_Realize(importState, field=fld_flux, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_Realize(importState, field=fld_porosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_Realize(importState, field=fld_pressure, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_Realize(importState, field=fld_saturation, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! realize export fields
    call NUOPC_Realize(exportState, field=fld_flux, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_Realize(exportState, field=fld_porosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_Realize(exportState, field=fld_pressure, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call NUOPC_Realize(exportState, field=fld_saturation, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! initialize export fields
    call ESMF_FieldFill(fld_flux, dataFillScheme="const", &
      const1=0.1_ESMF_KIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_FieldFill(fld_porosity, dataFillScheme="const", &
      const1=0.1_ESMF_KIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_FieldFill(fld_pressure, dataFillScheme="const", &
      const1=0.1_ESMF_KIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_FieldFill(fld_saturation, dataFillScheme="const", &
      const1=0.1_ESMF_KIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
  end subroutine InitializeP2

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    ! local variables
    type(ESMF_VM)    :: vm
    integer          :: localPet
    type(ESMF_State) :: importState
    type(ESMF_Field) :: fld_flux
    type(ESMF_Field) :: fld_porosity
    type(ESMF_Field) :: fld_pressure
    type(ESMF_Field) :: fld_saturation
    real(ESMF_KIND_R4), pointer :: ptr_flux(:,:,:)
    real(ESMF_KIND_R4), pointer :: ptr_porosity(:,:,:)
    real(ESMF_KIND_R4), pointer :: ptr_pressure(:,:,:)
    real(ESMF_KIND_R4), pointer :: ptr_saturation(:,:,:)
    real(ESMF_KIND_R4) :: lsum_flux(1),       gsum_flux(1)
    real(ESMF_KIND_R4) :: lsum_porosity(1),   gsum_porosity(1)
    real(ESMF_KIND_R4) :: lsum_pressure(1),   gsum_pressure(1)
    real(ESMF_KIND_R4) :: lsum_saturation(1), gsum_saturation(1)

    rc = ESMF_SUCCESS

    ! query component for vm and local pet
    call ESMF_GridCompGet(model, vm=vm, localPet=localPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! query component for import state
    call NUOPC_ModelGet(model, importState=importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! query import state for fields
    call ESMF_StateGet(importState, itemName="pf_flux", &
      field=fld_flux, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_StateGet(importState, itemName="pf_porosity", &
      field=fld_porosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_StateGet(importState, itemName="pf_pressure", &
      field=fld_pressure, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_StateGet(importState, itemName="pf_saturation", &
      field=fld_saturation, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! query fields for array pointers
    call ESMF_FieldGet(fld_flux, farrayPtr=ptr_flux, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_FieldGet(fld_porosity, farrayPtr=ptr_porosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_FieldGet(fld_pressure, farrayPtr=ptr_pressure, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    call ESMF_FieldGet(fld_saturation, farrayPtr=ptr_saturation, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! sum data from all PETs
    lsum_flux=sum(ptr_flux)
    call ESMF_VMReduce(vm=vm, sendData=lsum_flux, &
      recvData=gsum_flux, count=1, &
      reduceflag=ESMF_REDUCE_SUM, rootPet=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    lsum_porosity=sum(ptr_porosity)
    call ESMF_VMReduce(vm=vm, sendData=lsum_porosity, &
      recvData=gsum_porosity, count=1, &
      reduceflag=ESMF_REDUCE_SUM, rootPet=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    lsum_pressure=sum(ptr_pressure)
    call ESMF_VMReduce(vm=vm, sendData=lsum_pressure, &
      recvData=gsum_pressure, count=1, &
      reduceflag=ESMF_REDUCE_SUM, rootPet=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    lsum_saturation=sum(ptr_saturation)
    call ESMF_VMReduce(vm=vm, sendData=lsum_saturation, &
      recvData=gsum_saturation, count=1, &
      reduceflag=ESMF_REDUCE_SUM, rootPet=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return

    ! print import field sums
    if (localPet .eq. 0) then
      print *,"LND Import Sums"
      print *,"  sum(pf_flux)=",gsum_flux(1)
      print *,"  sum(pf_porosity)=",gsum_porosity(1)
      print *,"  sum(pf_pressure)=",gsum_pressure(1)
      print *,"  sum(pf_saturation)=",gsum_saturation(1)
    end if
  end subroutine ModelAdvance

end module pf_nuopc_test_lnd
