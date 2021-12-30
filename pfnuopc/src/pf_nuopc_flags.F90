#include "pf_nuopc_macros.h"

module parflow_nuopc_flags

  use ESMF, only: ESMF_UtilStringUpperCase, ESMF_SUCCESS

  implicit none

  private

  type field_init_flag
    sequence
    private
      integer :: init
  end type field_init_flag

  type(field_init_flag), parameter ::       &
    FLD_INIT_ERROR   = field_init_flag(-1), &
    FLD_INIT_ZERO    = field_init_flag(0),  &
    FLD_INIT_DEFAULT = field_init_flag(1),  &
    FLD_INIT_FILLV   = field_init_flag(2),  &
    FLD_INIT_IMPORT  = field_init_flag(3)

  type field_check_flag
    sequence
    private
      integer :: check
  end type field_check_flag

  type(field_check_flag), parameter ::      &
    FLD_CHECK_ERROR = field_check_flag(-1), &
    FLD_CHECK_CURRT = field_check_flag(0),  &
    FLD_CHECK_NEXTT = field_check_flag(1),  &
    FLD_CHECK_NONE  = field_check_flag(2)

  type field_geom_flag
    sequence
    private
      integer :: geom
  end type field_geom_flag

  type(field_geom_flag), parameter ::              &
    FLD_GEOM_ERROR          = field_geom_flag(-1), &
    FLD_GEOM_RGNLCARTESIAN  = field_geom_flag(0),  &
    FLD_GEOM_ACCEPT         = field_geom_flag(1)

  public field_init_flag
  public field_check_flag
  public field_geom_flag
  public FLD_INIT_ERROR
  public FLD_INIT_ZERO
  public FLD_INIT_DEFAULT
  public FLD_INIT_FILLV
  public FLD_INIT_IMPORT
  public FLD_CHECK_ERROR
  public FLD_CHECK_CURRT
  public FLD_CHECK_NEXTT
  public FLD_CHECK_NONE
  public FLD_GEOM_ERROR
  public FLD_GEOM_RGNLCARTESIAN
  public FLD_GEOM_ACCEPT

  public operator(==), assignment(=)

  interface operator (==)
    module procedure field_init_eq
    module procedure field_check_eq
    module procedure field_geom_eq
  end interface

  interface assignment (=)
    module procedure field_init_toString
    module procedure field_init_frString
    module procedure field_check_toString
    module procedure field_check_frString
    module procedure field_geom_toString
    module procedure field_geom_frString
  end interface

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  function field_init_eq(val1, val2)
    logical field_init_eq
    type(field_init_flag), intent(in) :: val1, val2
    field_init_eq = (val1%init == val2%init)
  end function field_init_eq

  !-----------------------------------------------------------------------------

  subroutine field_init_toString(string, val)
    character(len=*), intent(out) :: string
    type(field_init_flag), intent(in) :: val
    if (val == FLD_INIT_ZERO) then
      write(string,'(a)') 'FLD_INIT_ZERO'
    elseif (val == FLD_INIT_DEFAULT) then
      write(string,'(a)') 'FLD_INIT_DEFAULT'
    elseif (val == FLD_INIT_FILLV) then
      write(string,'(a)') 'FLD_INIT_FILLV'
    elseif (val == FLD_INIT_IMPORT) then
      write(string,'(a)') 'FLD_INIT_IMPORT'
    else
      write(string,'(a)') 'FLD_INIT_ERROR'
    endif
  end subroutine field_init_toString

  !-----------------------------------------------------------------------------

  subroutine field_init_frString(val, string)
    type(field_init_flag), intent(out) :: val
    character(len=*), intent(in) :: string
    character(len=16) :: ustring
    integer :: rc
    ustring = ESMF_UtilStringUpperCase(string, rc=rc)
    if (rc .ne. ESMF_SUCCESS) then
      val = FLD_INIT_ERROR
    elseif (ustring .eq. 'FLD_INIT_ZERO') then
      val = FLD_INIT_ZERO
    elseif (ustring .eq. 'FLD_INIT_DEFAULT') then
      val = FLD_INIT_DEFAULT
    elseif (ustring .eq. 'FLD_INIT_FILLV') then
      val = FLD_INIT_FILLV
    elseif (ustring .eq. 'FLD_INIT_IMPORT') then
      val = FLD_INIT_IMPORT
    else
      val = FLD_INIT_ERROR
    endif
  end subroutine field_init_frString

  !-----------------------------------------------------------------------------

  function field_check_eq(val1, val2)
    logical field_check_eq
    type(field_check_flag), intent(in) :: val1, val2
    field_check_eq = (val1%check == val2%check)
  end function field_check_eq

  !-----------------------------------------------------------------------------

  subroutine field_check_toString(string, val)
    character(len=*), intent(out) :: string
    type(field_check_flag), intent(in) :: val
    if (val == FLD_CHECK_CURRT) then
      write(string,'(a)') 'FLD_CHECK_CURRT'
    elseif (val == FLD_CHECK_NEXTT) then
      write(string,'(a)') 'FLD_CHECK_NEXTT'
    elseif (val == FLD_CHECK_NONE) then
      write(string,'(a)') 'FLD_CHECK_NONE'
    else
      write(string,'(a)') 'FLD_CHECK_ERROR'
    endif
  end subroutine field_check_toString

  !-----------------------------------------------------------------------------

  subroutine field_check_frString(val, string)
    type(field_check_flag), intent(out) :: val
    character(len=*), intent(in) :: string
    character(len=16) :: ustring
    integer :: rc
    ustring = ESMF_UtilStringUpperCase(string, rc=rc)
    if (rc .ne. ESMF_SUCCESS) then
      val = FLD_CHECK_ERROR
    elseif (ustring .eq. 'FLD_CHECK_CURRT') then
      val = FLD_CHECK_CURRT
    elseif (ustring .eq. 'FLD_CHECK_NEXTT') then
      val = FLD_CHECK_NEXTT
    elseif (ustring .eq. 'FLD_CHECK_NONE') then
      val = FLD_CHECK_NONE
    else
      val = FLD_CHECK_ERROR
    endif
  end subroutine field_check_frString

  !-----------------------------------------------------------------------------

  function field_geom_eq(val1, val2)
    logical field_geom_eq
    type(field_geom_flag), intent(in) :: val1, val2
    field_geom_eq = (val1%geom == val2%geom)
  end function field_geom_eq

  !-----------------------------------------------------------------------------

  subroutine field_geom_toString(string, val)
    character(len=*), intent(out) :: string
    type(field_geom_flag), intent(in) :: val
    if (val == FLD_GEOM_RGNLCARTESIAN) then
      write(string,'(a)') 'FLD_GEOM_RGNLCARTESIAN'
    elseif (val == FLD_GEOM_ACCEPT) then
      write(string,'(a)') 'FLD_GEOM_ACCEPT'
    else
      write(string,'(a)') 'FLD_GEOM_ERROR'
    endif
  end subroutine field_geom_toString

  !-----------------------------------------------------------------------------

  subroutine field_geom_frString(val, string)
    type(field_geom_flag), intent(out) :: val
    character(len=*), intent(in) :: string
    character(len=32) :: ustring
    integer :: rc
    ustring = ESMF_UtilStringUpperCase(string, rc=rc)
    if (rc .ne. ESMF_SUCCESS) then
      val = FLD_GEOM_ERROR
    elseif (ustring .eq. 'FLD_GEOM_RGNLCARTESIAN') then
      val = FLD_GEOM_RGNLCARTESIAN
    elseif (ustring .eq. 'FLD_GEOM_ACCEPT') then
      val = FLD_GEOM_ACCEPT
    else
      val = FLD_GEOM_ERROR
    endif
  end subroutine field_geom_frString


end module
