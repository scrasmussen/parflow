!NUOPC:MEDIATION_LAYER:PHYSICS
!
#include "pf_nuopc_macros.h"

module parflow_nuopc
  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS           => SetServices, &
    model_label_SetClock       => label_SetClock, &
    model_label_DataInitialize => label_DataInitialize, &
    model_label_CheckImport    => label_CheckImport, &
    model_label_Advance        => label_Advance, &
    model_label_Finalize       => label_Finalize
  use parflow_nuopc_fields
  use parflow_nuopc_flags
  use iso_c_binding, only: c_null_char, c_int, c_double, c_float

  implicit none

  private

  public SetServices

  character(LEN=*), parameter :: label_InternalState = 'InternalState'

  type type_InternalStateStruct
    logical                :: initialized        = .false.
    character(len=64)      :: config_filename    = "dummy"
    logical                :: realize_all_export = .false.
    logical                :: realize_all_import = .false.
    type(field_init_flag)  :: init_export        = FLD_INIT_ZERO
    type(field_init_flag)  :: init_import        = FLD_INIT_ZERO
    type(field_check_flag) :: check_import       = FLD_CHECK_CURRT
    type(field_geom_flag)  :: geom               = FLD_GEOM_TESTG
    integer                :: nx                 = 0
    integer                :: ny                 = 0
    integer                :: nz                 = 4
    character(len=16)      :: transfer_offer     = "cannot provide"
    logical                :: share_field_mem    = .false.
    character(len=64)      :: output_dir         = "."
    type(ESMF_Time)        :: pf_epoch
  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

!EOP

!------------------------------------------------------------------
  contains
!------------------------------------------------------------------

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(type_InternalState)   :: is
    integer                    :: stat

    rc = ESMF_SUCCESS

    ! allocate memory for this internal state and set it in the component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of internal state memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return  ! bail out
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! switching to IPD versions
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3"/), userRoutine=InitializeP3, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
!      phaseLabelList=(/"IPDv03p4"/), userRoutine=InitializeP4, rc=rc)
!    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
!      phaseLabelList=(/"IPDv03p5"/), userRoutine=InitializeP5, rc=rc)
!    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_SetClock, &
      specRoutine=SetClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_DataInitialize, &
       specRoutine=DataInitialize, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_MethodRemove(gcomp, label=model_label_CheckImport, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_CheckImport, &
       specRoutine=CheckImport, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call NUOPC_CompSpecialize(gcomp, speclabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Finalize, &
      specRoutine=ModelFinalize, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

  end subroutine

!..................................................................

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! local variables
    character(32)              :: cname
    character(*), parameter    :: rname="InitializeP0"
    integer                    :: verbosity, diagnostic
    character(len=64)          :: value
    type(type_InternalState)   :: is
    integer                    :: stat

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"max ","high","low ","off "/), &
      specialValueList= (/ 65535, 65535, 65535,     0/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"max ","high","low ","off "/), &
      specialValueList= (/ 65535, 65281,  8193,     0/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Switch to IPDv03 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call PF_AttributeRead(rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! prepare diagnostics folder
    if (btest(diagnostic,16)) then
      call ESMF_UtilIOMkDir(pathName=trim(is%wrap%output_dir), &
        relaxedFlag=.true., rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine PF_AttributeRead(rc)
      integer, intent(out)  :: rc

      ! local variables
      logical                    :: configIsPresent
      type(ESMF_Config)          :: config
      type(NUOPC_FreeFormat)     :: attrFF
      character(len=64)          :: value
      character(ESMF_MAXSTR)     :: logMsg

      ! check gcomp for config
      call ESMF_GridCompGet(gcomp, configIsPresent=configIsPresent, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      ! read and ingest free format component attributes
      if (configIsPresent) then
        call ESMF_GridCompGet(gcomp, config=config, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        attrFF = NUOPC_FreeFormatCreate(config, &
          label=trim(cname)//"_attributes::", relaxedflag=.true., rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call NUOPC_CompAttributeIngest(gcomp, attrFF, addFlag=.true., rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif

      ! Realize all import fields
      call ESMF_AttributeGet(gcomp, name="realize_all_import", value=value, &
        defaultValue="false", convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%realize_all_import = (trim(value)=="true")

      ! Realize all export fields
      call ESMF_AttributeGet(gcomp, name="realize_all_export", value=value, &
        defaultValue="false", convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%realize_all_export = (trim(value)=="true")

      ! Set configuration file name
      call ESMF_AttributeGet(gcomp, name="filename", value=value, &
        defaultValue="config_file", convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%config_filename=value

      ! import data initialization type
      call ESMF_AttributeGet(gcomp, name="initialize_import", &
        value=value, defaultValue="FLD_INIT_ZERO", &
        convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%init_import = value

      ! export data initialization type
      call ESMF_AttributeGet(gcomp, name="initialize_export", &
        value=value, defaultValue="FLD_INIT_ZERO", &
        convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%init_export = value

      ! import check type
      call ESMF_AttributeGet(gcomp, name="check_import", &
        value=value, defaultValue="FLD_CHECK_CURRT", &
        convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%check_import = value

      ! field geom type
      call ESMF_AttributeGet(gcomp, name="geom", &
        value=value, defaultValue="FLD_GEOM_TESTG", &
        convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%geom = value

      ! share field memory
      call ESMF_AttributeGet(gcomp, name="share_field_mem", value=value, &
        defaultValue="false", convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%share_field_mem = (trim(value)=="true")

      ! Get component output directory
      call ESMF_AttributeGet(gcomp, name="output_directory", &
        value=value, defaultValue=trim(cname)//"_OUTPUT", &
        convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%output_dir = trim(value)

      if (btest(verbosity,16)) then
        call ESMF_LogWrite(trim(cname)//": Settings",ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
          '  Verbosity            = ',verbosity
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
          '  Diagnostic           = ',diagnostic
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        value = is%wrap%init_export
        write (logMsg, "(A,(A,A))") trim(cname)//': ', &
          '  Initialize Export    = ',trim(value)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        value = is%wrap%init_import
        write (logMsg, "(A,(A,A))") trim(cname)//': ', &
          '  Initialize Import    = ',trim(value)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        value = is%wrap%geom
        write (logMsg, "(A,(A,A))") trim(cname)//': ', &
          '  Geom                 = ',trim(value)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        value = is%wrap%check_import
        write (logMsg, "(A,(A,A))") trim(cname)//': ', &
          '  Check Import         = ',trim(value)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,L1))") trim(cname)//': ', &
          '  Realze All Imports   = ',is%wrap%realize_all_import
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,L1))") trim(cname)//': ', &
          '  Realze All Exports   = ',is%wrap%realize_all_export
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,A))") trim(cname)//': ', &
          '  Config Filename      = ',is%wrap%config_filename
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,L1))") trim(cname)//': ', &
          '  Share Field Memory   = ',is%wrap%share_field_mem
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,A))") trim(cname)//': ', &
          '  Output Directory     = ',is%wrap%output_dir
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
      endif

    end subroutine

  end subroutine

!..................................................................

  subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)     :: gcomp
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Clock)        :: clock
    integer,intent(out)     :: rc

    ! LOCAL VARIABLES
    character(32)              :: cname
    character(*), parameter    :: rname="InitializeP1"
    integer                    :: verbosity, diagnostic
    character(len=64)          :: value
    type(type_InternalState)   :: is
    integer                    :: stat
    type(ESMF_VM)              :: vm
    integer                    :: localPet, petCount
    integer                    :: fIndex

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"max ","high","low ","off "/), &
      specialValueList= (/ 65535, 65535, 65535,     0/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"max ","high","low ","off "/), &
      specialValueList= (/ 65535, 65281,  8193,     0/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_GridCompGet(gcomp, vm=vm, localPet=localPet, petCount=petCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! determine if component will provide or accept geom
    if (is%wrap%geom .eq. FLD_GEOM_TESTG) then
      is%wrap%transfer_offer="will provide"
    else if (is%wrap%geom .eq. FLD_GEOM_ACCEPT) then
      is%wrap%transfer_offer="cannot provide"
    else
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="Unsupported geom type", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out      LogSetError
    end if

    call field_advertise(fieldList=pf_nuopc_fld_list, &
      importState=importState, &
      exportState=exportState, &
      transferOffer=trim(is%wrap%transfer_offer), &
      rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (btest(verbosity,16)) then
      call field_advertise_log(pf_nuopc_fld_list, trim(cname), rc=rc)
    end if

  end subroutine

!..................................................................

  subroutine InitializeP3(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    ! local Variables
    character(32)              :: cname
    character(*), parameter    :: rname="InitializeP3"
    integer                    :: verbosity, diagnostic
    character(len=64)          :: value
    type(type_InternalState)   :: is
    type(ESMF_VM)              :: vm
    integer                    :: localPet, petCount
    integer(c_int)             :: ierr
    integer(c_int)             :: pfnumprocs
    integer(c_int)             :: pfsubgridcnt
    integer(c_int)             :: lcldecomp(4)
    integer, allocatable       :: gbldecomp(:)
    integer, allocatable       :: deBlockList(:,:,:)
    integer(c_int), allocatable :: mask(:,:)
    type(ESMF_Array)           :: arrayMask
    integer                    :: i
    type(ESMF_Grid)            :: testGrid
    type(ESMF_DistGrid)        :: distgrid
    character(ESMF_MAXSTR)     :: logMsg

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"max ","high","low ","off "/), &
      specialValueList= (/ 65535, 65535, 65535,     0/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"max ","high","low ","off "/), &
      specialValueList= (/ 65535, 65281,  8193,     0/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (btest(verbosity,16)) then
      call ESMF_LogWrite(trim(cname)//": "//rname,ESMF_LOGMSG_INFO)
      write (logMsg, "(A,A)") trim(cname)//': ', &
        '  Calling wrfparflowinit'
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
      write (logMsg, "(A,(A,A))") trim(cname)//': ', &
        '  Config Filename = ',trim(is%wrap%config_filename)
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
      call ESMF_LogFlush()
    end if

    call ESMF_GridCompGet(gcomp, vm=vm, localPet=localPet, &
      petCount=petCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! call parflow c interface
    ! void wrfparflowinit_(char *input_file)
    call wrfparflowinit(trim(is%wrap%config_filename)//c_null_char)

    ! call parflow c interface
    ! void wrfnumprocs_(int *numprocs, int *ierror)
    call wrfnumprocs(pfnumprocs, ierr)
    if (ierr .ne. 0) then
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="wrfnumprocs failed.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    elseif (pfnumprocs .ne. petCount) then
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="pfnumprocs does not equal component petcount.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    if (is%wrap%geom .eq. FLD_GEOM_TESTG) then
      ! call parflow c interface
      ! void wrfsubgridcount_(int *subgridcount, int *ierror)
      call wrfsubgridcount(pfsubgridcnt, ierr)
      if (ierr .ne. 0) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="wrfsubgridcount failed.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      elseif (pfsubgridcnt .ne. 1) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="Unsupported subgrid count", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return  ! bail out      LogSetError
      endif
      ! call parflox c interface
      ! void wrfdeblocksizes_(int *sg,
      !                       int *lowerx, int *upperx,
      !                       int *lowery, int *uppery,
      !                       int *ierror)
      call wrflocaldecomp(0, lcldecomp(1), lcldecomp(2), &
        lcldecomp(3), lcldecomp(4), ierr)
      if (ierr .ne. 0) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="wrflocaldecomp failed.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
      allocate(gbldecomp(petCount*4))
      call ESMF_VMAllGather(vm, sendData=lcldecomp(1:4), &
        recvData=gbldecomp(:), count=4, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      allocate(deBlockList(2,2,petCount))
      do i=0, petCount-1
        deBlockList(1,1,i+1) = gbldecomp((i*4)+1)+1
        deBlockList(1,2,i+1) = gbldecomp((i*4)+2)+1
        deBlockList(2,1,i+1) = gbldecomp((i*4)+3)+1
        deBlockList(2,2,i+1) = gbldecomp((i*4)+4)+1
      enddo
      deallocate(gbldecomp)
      is%wrap%nx=maxval(deBlockList(1,2,:))
      is%wrap%ny=maxval(deBlockList(2,2,:))
      testGrid = ESMF_GridCreate1PeriDimUfrm(name=trim(cname)//"-Grid", &
        maxIndex=(/is%wrap%nx, is%wrap%ny/), &
        minCornerCoord=(/0._ESMF_KIND_R8, -50._ESMF_KIND_R8/), &
        maxCornerCoord=(/360._ESMF_KIND_R8, 70._ESMF_KIND_R8/), &
        deBlockList=deBlockList, &
        staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
        rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      deallocate(deBlockList)
      call ESMF_GridGet(testGrid, distgrid=distgrid, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      allocate(mask(lcldecomp(1):lcldecomp(2),lcldecomp(3):lcldecomp(4)))
      ! call parflox c interface
      ! void wrflocalmask_(int *sg, int *mask, int *ierror)
      call wrflocalmask(0, mask, ierr)
      if (ierr .ne. 0) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="wrflocalmask failed.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
      arrayMask = ESMF_ArrayCreate(distgrid, farray=mask, &
        indexflag=ESMF_INDEX_GLOBAL, datacopyflag=ESMF_DATACOPY_VALUE, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      deallocate(mask)
      call ESMF_GridAddItem(testGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        itemflag=ESMF_GRIDITEM_MASK, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call ESMF_GridSetItem(testGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
          itemflag=ESMF_GRIDITEM_MASK, array=arrayMask, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call field_realize(fieldList=pf_nuopc_fld_list, &
        importState=importState, exportState=exportState, &
        grid=testGrid, num_soil_layers=is%wrap%nz, &
        realizeAllImport=is%wrap%realize_all_import, &
        realizeAllExport=is%wrap%realize_all_export, &
        shareMemory=is%wrap%share_field_mem, &
        rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    else
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="Unsupported geom type", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out      LogSetError
    endif

    call field_fill_state(exportState, &
      fill_type=is%wrap%init_export, &
      fieldList=pf_nuopc_fld_list, &
      fillValue=ESMF_DEFAULT_VALUE, &
      rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (btest(verbosity,16)) then
      call field_realize_log(pf_nuopc_fld_list, trim(cname), rc=rc)
    end if

  end subroutine

!..................................................................

  subroutine InitializeP4(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Field)              :: field
    type(ESMF_Grid)               :: grid
    integer                       :: localDeCount
    character(80)                 :: name
    character(160)                :: msgString

    type(ESMF_DistGrid)           :: distgrid
    integer                       :: dimCount, tileCount, arbDimCount
    integer, allocatable          :: minIndexPTile(:,:), maxIndexPTile(:,:)
    integer                       :: connectionCount
    type(ESMF_DistGridConnection), allocatable :: connectionList(:)
    character(ESMF_MAXSTR)        :: transferAction
    logical                       :: regDecompFlag

    rc = ESMF_SUCCESS

  end subroutine

!..................................................................

  subroutine InitializeP5(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Field)              :: field
    type(ESMF_Grid)               :: grid
    integer                       :: localDeCount
    character(80)                 :: name
    character(160)                :: msgString

    type(ESMF_DistGrid)           :: distgrid
    integer                       :: dimCount, tileCount, arbDimCount
    integer, allocatable          :: minIndexPTile(:,:), maxIndexPTile(:,:)
    integer                       :: connectionCount
    type(ESMF_DistGridConnection), allocatable :: connectionList(:)
    character(ESMF_MAXSTR)        :: transferAction
    logical                       :: regDecompFlag

    rc = ESMF_SUCCESS

  end subroutine

!..................................................................

  subroutine SetClock(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    ! local variables
    character(32)            :: cname
    character(*), parameter  :: rname="SetClock"
    integer                  :: verbosity, diagnostic
    character(len=64)        :: value
    type(type_InternalState) :: is

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"max ","high","low ","off "/), &
      specialValueList= (/ 65535, 65535, 65535,     0/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"max ","high","low ","off "/), &
      specialValueList= (/ 65535, 65281,  8193,     0/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_TimeSet(is%wrap%pf_epoch, &
      yy=1900, mm=1, dd=1, &
       h=0,     m=0,  s=0, &
      calkindflag=ESMF_CALKIND_GREGORIAN, &
      rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

  end subroutine

!..................................................................

  subroutine DataInitialize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(32)            :: cname
    character(*), parameter  :: rname="DataInitialize"
    integer                  :: verbosity, diagnostic
    character(len=64)        :: value
    type(type_InternalState) :: is
    integer                  :: stat
    type(ESMF_Clock)         :: modelClock
    type(ESMF_Time)          :: currTime
    type(ESMF_State)         :: importState
    logical                  :: importInit

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"max ","high","low ","off "/), &
      specialValueList= (/ 65535, 65535, 65535,     0/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"max ","high","low ","off "/), &
      specialValueList= (/ 65535, 65281,  8193,     0/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query the component for its clock and importState
    call NUOPC_ModelGet(gcomp, &
      modelClock=modelClock, &
      importState=importState, &
      rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if ( is%wrap%init_import .eq. FLD_INIT_IMPORT ) then
      call ESMF_ClockGet(modelClock, currTime=currTime, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      importInit = NUOPC_IsAtTime(importState, time=currTime, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      if (importInit) then
        call ESMF_LogWrite( &
          trim(cname)//': '//rname//' Initialize-Data-Dependency SATISFIED!!!', &
          ESMF_LOGMSG_INFO)
      else
        call ESMF_LogWrite( &
          trim(cname)//': '//rname//' Initialize-Data-Dependency NOT YET SATISFIED!!!', &
          ESMF_LOGMSG_INFO)
      endif
    else
      call field_fill_state(importState, &
        fill_type=is%wrap%init_import, &
        fieldList=pf_nuopc_fld_list, &
        fillValue=ESMF_DEFAULT_VALUE, &
        rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      importInit = .true.
    endif

    ! set InitializeDataComplete Attribute to "true", indicating to the
    ! generic code that all inter-model data dependencies are satisfied
    if (importInit) then
      call NUOPC_CompAttributeSet(gcomp, name="InitializeDataComplete", value="true", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

  end subroutine

!..................................................................

  subroutine CheckImport(gcomp, rc)
    type(ESMF_GridComp) :: gcomp
    integer,intent(out) :: rc

    ! local variables
    character(32)               :: cname
    character(*), parameter     :: rname="CheckImport"
    integer                     :: verbosity, diagnostic
    character(len=64)           :: value
    type(type_InternalState)    :: is
    type(ESMF_State)            :: importState
    type(ESMF_Clock)            :: modelClock
    type(ESMF_Time)             :: modelCurrTime
    type(ESMF_Time)             :: modelNextTime
    logical                     :: checkTime

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"max ","high","low ","off "/), &
      specialValueList= (/ 65535, 65535, 65535,     0/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"max ","high","low ","off "/), &
      specialValueList= (/ 65535, 65281,  8193,     0/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query the component for its clock and importState
    call NUOPC_ModelGet(gcomp, &
      modelClock=modelClock, &
      importState=importState, &
      rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if ( is%wrap%check_import .eq. FLD_CHECK_CURRT ) then
      call ESMF_ClockGet(modelClock, currTime=modelCurrTime, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      checkTime = NUOPC_IsAtTime(importState, modelCurrTime, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    else if ( is%wrap%check_import .eq. FLD_CHECK_NEXTT ) then
      call ESMF_ClockGetNextTime(modelClock, nextTime=modelNextTime, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      checkTime = NUOPC_IsAtTime(importState, modelNextTime, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    else if ( is%wrap%check_import .eq. FLD_CHECK_NONE ) then
      checkTime = .true.
    else
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Unsupported check_import in CheckImport", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    end if

    if (.not.checkTime) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Import Timestamp: Import fields not at correct time. "// &
            "Try check_import=FLD_CHECK_NEXTT for sequential run.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

  end subroutine

!..................................................................

  subroutine ModelAdvance(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(32)               :: cname
    character(*), parameter     :: rname="ModelAdvance"
    integer                     :: verbosity, diagnostic
    character(len=64)           :: value
    type(type_InternalState)    :: is
    type(ESMF_Clock)            :: modelClock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Time)             :: startTime, currTime
    type(ESMF_TimeInterval)     :: elapsedTime, timeStep

    type(ESMF_Field) :: fld_imp_flux
    type(ESMF_Field) :: fld_imp_pressure
    type(ESMF_Field) :: fld_imp_porosity
    type(ESMF_Field) :: fld_imp_saturation
    type(ESMF_Field) :: fld_exp_flux
    type(ESMF_Field) :: fld_exp_pressure
    type(ESMF_Field) :: fld_exp_porosity
    type(ESMF_Field) :: fld_exp_saturation
    integer          :: gridToFieldMap(2)
    integer          :: ungriddedLBound(1)
    integer          :: ungriddedUBound(1)
    integer(c_int)   :: totalLWidth(2,1)
    integer(c_int)   :: totalUWidth(2,1)
    real(c_double)   :: pf_dt, pf_time
    integer(c_int)   :: num_soil_layers
    real(c_float), pointer :: pf_flux(:, :, :)
    real(c_float), pointer :: pf_pressure(:, :, :)
    real(c_float), pointer :: pf_porosity(:, :, :)
    real(c_float), pointer :: pf_saturation(:, :, :)
    character(ESMF_MAXSTR) :: logMsg

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"max ","high","low ","off "/), &
      specialValueList= (/ 65535, 65535, 65535,     0/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"max ","high","low ","off "/), &
      specialValueList= (/ 65535, 65281,  8193,     0/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query the component for its clock, importState, and exportState
    call NUOPC_ModelGet(gcomp, &
      modelClock=modelClock, &
      importState=importState, &
      exportState=exportState, &
      rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! convert elapsed time and time step to hours
    call ESMF_ClockGet(modelClock, &
      startTime=startTime, currTime=currTime, &
      timeStep=timeStep, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    elapsedTime = currTime - startTime
    call ESMF_TimeIntervalGet(elapsedTime, h_r8=pf_time, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_TimeIntervalGet(timeStep, h_r8=pf_dt, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query import state for parflow exchange fields
    call ESMF_StateGet(importState, itemName="PF_FLUX", &
      field=fld_imp_flux, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_FieldGet(fld_imp_flux, farrayPtr=pf_flux, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState, itemName="PF_PRESSURE", &
      field=fld_imp_pressure, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_FieldGet(fld_imp_pressure, farrayPtr=pf_pressure, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState, itemName="PF_POROSITY", &
      field=fld_imp_porosity, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_FieldGet(fld_imp_porosity, farrayPtr=pf_porosity, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_StateGet(importState, itemName="PF_SATURATION", &
      field=fld_imp_saturation, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_FieldGet(fld_imp_saturation, farrayPtr=pf_saturation, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query flux field for ungridded dimension and halo sizes
    call ESMF_FieldGet(fld_imp_flux, &
      ungriddedLBound=ungriddedLBound, &
      ungriddedUBound=ungriddedUBound, &
      totalLWidth=totalLWidth, &
      totalUWidth=totalUWidth, &
      gridToFieldMap=gridToFieldMap, &
      rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! check ungridded dimension
    if (.not.all(gridToFieldMap == (/1, 3/))) then
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="Ungridded dimension index does not equal 2", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out      LogSetError
    end if

    ! compute number of soil layers from ungridded dimensions
    num_soil_layers=ungriddedUBound(1)-ungriddedLBound(1)+1

    if (btest(verbosity,16)) then
      call ESMF_LogWrite(trim(cname)//": "//rname,ESMF_LOGMSG_INFO)
      write (logMsg, "(A,A)") trim(cname)//': ', &
        '  Calling wrfparflowadvance'
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
      write (logMsg, "(A,(A,F0.3))") trim(cname)//': ', &
        '  Current Time(h)       = ',real(pf_time)
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
      write (logMsg, "(A,(A,F0.3))") trim(cname)//': ', &
        '  Time Step(h)          = ',real(pf_dt)
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
      write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
        '  Number of Soil Layers = ',int(num_soil_layers)
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
      write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
        '  Halo Size I Lower     = ',int(totalLWidth(1,1))
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
      write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
        '  Halo Size J Lower     = ',int(totalLWidth(2,1))
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
      write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
        '  Halo Size I Upper     = ',int(totalUWidth(1,1))
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
      write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
        '  Halo Size J Upper     = ',int(totalUWidth(2,1))
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
      call ESMF_LogFlush()
    end if

    ! call parflow c interface
    ! field dimensions (i,num_soil_layers,j)
    ! void wrfparflowadvance_(double *current_time, double *dt,
    !   float * wrf_flux,     float * wrf_pressure,
    !   float * wrf_porosity, float * wrf_saturation,
    !   int *   num_soil_layers,
    !   int *   ghost_size_i_lower, int *   ghost_size_j_lower,
    !   int *   ghost_size_i_upper, int *   ghost_size_j_upper)
    call wrfparflowadvance(pf_time, pf_dt, &
      pf_flux,     pf_pressure, &
      pf_porosity, pf_saturation, &
      num_soil_layers, &
      totalLWidth(1,1), totalLWidth(2,1), &
      totalUWidth(1,1), totalUWidth(2,1))

    ! copy data to export fields
    if (.not.is%wrap%share_field_mem) then
      call ESMF_StateGet(exportState, itemName="PF_FLUX", &
        field=fld_exp_flux, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call ESMF_StateGet(exportState, itemName="PF_PRESSURE", &
        field=fld_exp_pressure, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call ESMF_StateGet(exportState, itemName="PF_POROSITY", &
        field=fld_exp_porosity, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call ESMF_StateGet(exportState, itemName="PF_SATURATION", &
        field=fld_exp_saturation, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call ESMF_FieldCopy(fld_exp_flux, fieldIn=fld_imp_flux, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call ESMF_FieldCopy(fld_exp_porosity, fieldIn=fld_imp_porosity, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call ESMF_FieldCopy(fld_exp_pressure, fieldIn=fld_imp_pressure, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call ESMF_FieldCopy(fld_exp_saturation, fieldIn=fld_imp_saturation, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    end if

  end subroutine

!..................................................................

  subroutine ModelFinalize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(32)              :: cname
    character(*), parameter    :: rname="ModelFinalize"
    integer                    :: verbosity, diagnostic
    character(len=64)          :: value
    type(type_InternalState)   :: is
    integer                    :: stat

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"max ","high","low ","off "/), &
      specialValueList= (/ 65535, 65535, 65535,     0/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"max ","high","low ","off "/), &
      specialValueList= (/ 65535, 65281,  8193,     0/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='Deallocation of internal state memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return  ! bail out

  end subroutine

!------------------------------------------------------------------

end module parflow_nuopc
