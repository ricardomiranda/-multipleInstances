!------------------------------------------------------------------------------
!        IST/MARETEC, Water Modelling Group, Mohid modelling system
!------------------------------------------------------------------------------
!
! TITLE         : Higher Order Functions
! DATE          : September 2013
! REVISION      : Ricardo Miranda, mail@ricardomiranda.com
! DESCRIPTION   : Example program of Functional Core, Imperative LotkaVolterra according to
!                 Gary Bernhardt
!
!------------------------------------------------------------------------------
!
!This program is free software; you can redistribute it and/or
!modify it under the terms of the GNU General Public License
!version 2, as published by the Free Software Foundation.
!
!This program is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!GNU General Public License for more details.
!
!You should have received a copy of the GNU General Public License
!along with this program; if not, write to the Free Software
!Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
!
!------------------------------------------------------------------------------

program MainMI

    use ModuleHOF

    implicit none

    type T_MainMI
        real(8) :: DT, NbrSteps
    end type T_MainMI

    call Main


    !Subroutines---------------------------------------------------------------

    !Constructor
!    public  :: ConstructMainMI
!    private ::      ASkQuestions
!    private ::      StartMI

    !Selector

    !Modifier
!    public  :: Main
!    private ::      Loop
!    private ::      exampleFunction

    !Destructor
!    public  :: KillMainMI

    !---------------------------------------------------------------------------

    contains

    !--------------------------------------------------------------------------

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONS

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    subroutine ConstructMainMI (ObjMainMI)

        !Arguments---------------------------------------------------------------
        type(T_MainMI), pointer :: ObjMainMI

        !------------------------------------------------------------------------

        call AllocateInstance (ObjMainMI)
        call ASkQuestions     (ObjMainMI)

    end subroutine ConstructMainMI

    !--------------------------------------------------------------------------

    subroutine ASkQuestions (ObjMainMI)

        !Arguments---------------------------------------------------------------
        type(T_MainMI), pointer :: ObjMainMI

        !Local-------------------------------------------------------------------

        !------------------------------------------------------------------------

        print*, "What is the time step?"
        read*,  ObjMainMI%DT

        print*, "How many time steps to compute?"
        read*,  ObjMainMI%NbrSteps

    end subroutine ASkQuestions

    !--------------------------------------------------------------------------

    function StartHOF ()

        !Local-------------------------------------------------------------------
        integer :: IMIN, IMAX, ILB, IUB
        integer :: JMIN, JMAX, JLB, JUB

        !Return----------------------------------------------------------------
        type (T_Arrays), pointer  :: StartHOF

        !Local-----------------------------------------------------------------
        type (T_Arrays), pointer  :: NewObjHDF

        !----------------------------------------------------------------------

        print*, "IMIN?"
        read*,   IMIN

        print*, "IMAX?"
        read*,   IMAX

        print*, "ILB?"
        read*,   ILB

        print*, "IUB?"
        read*,   IUB

        print*, "JMIN?"
        read*,   JMIN

        print*, "JMAX?"
        read*,   JMAX

        print*, "JLB?"
        read*,   JLB

        print*, "JUB?"
        read*,   JUB

        NewObjHDF => ConstructHOF (IMIN, IMAX, ILB, IUB,                       &
                                   JMIN, JMAX, JLB, JUB)

        StartHOF  => NewObjHDF

    end function StartHOF

    !--------------------------------------------------------------------------

    pure subroutine AllocateInstance (ObjMainMI)
        !Arguments-------------------------------------------------------------
        type(T_MainMI), pointer :: ObjMainMI

        !----------------------------------------------------------------------

        allocate (ObjMainMI)

    end subroutine AllocateInstance

    !--------------------------------------------------------------------------

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODI

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !--------------------------------------------------------------------------

    subroutine Main

        !Local-----------------------------------------------------------------
        type(T_MainMI), pointer :: ObjMainMI
        type(T_Arrays ), pointer :: ObjHOF01, ObjHOF02

        !----------------------------------------------------------------------

        call ConstructMainMI (ObjMainMI)

        ObjHOF01 => StartHOF ()
        ObjHOF02 => StartHOF ()
        call Loop          (ObjMainMI, ObjHOF01, ObjHOF02, ObjMainMI%NbrSteps)

    end subroutine Main

    !--------------------------------------------------------------------------

    recursive subroutine Loop (ObjMainMI, ObjHOF01, ObjHOF02, NbrSteps)

        !Arguments-------------------------------------------------------------
        type(T_MainMI), pointer  :: ObjMainMI
        type(T_Arrays ), pointer :: ObjHOF01, ObjHOF02
        real(8), intent(IN)      :: NbrSteps

        !Local-----------------------------------------------------------------
        type(T_Arrays ), pointer :: NewObjHOF01, NewObjHOF02
        real(8)                  :: B01, B02

        !----------------------------------------------------------------------

cd1 :   if (NbrSteps .LE. 0.0) then
            print*, "Simulation terminated successfully."
            call killMainMI (ObjMainMI, ObjHOF01, ObjHOF02)

        else   cd1

        B01 = 1.0
        B02 = 2.0

!$OMP PARALLEL
!$OMP SECTIONS
!$OMP SECTION
            print*, "ObjHOF01, ini"
            NewObjHOF01 => CalcResult (ObjHOF01, B01)
            print*, "ObjHOF01", GetArrayResIJ(ObjHOF01, 1000, 10), GetArrayResIJ(ObjHOF01, 100, 100)
            call HOFGarbageCollector  (ObjHOF01)

!$OMP SECTION
            print*, "ObjHOF02, ini"
            NewObjHOF02 => CalcResult (ObjHOF02, B02)
            print*, "ObjHOF02", GetArrayResIJ(ObjHOF02, 1000, 10), GetArrayResIJ(ObjHOF02, 100, 100)
            call HOFGarbageCollector  (ObjHOF02)
!$OMP END SECTIONS NOWAIT
!$OMP END PARALLEL

            call Loop (ObjMainMI, NewObjHOF01, NewObjHOF02, NbrSteps - ObjMainMI%DT)
        end if cd1

    end subroutine Loop

    !--------------------------------------------------------------------------


    !--------------------------------------------------------------------------

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    pure subroutine KillMainMI (ObjMainMI, ObjHOF01, ObjHOF02)
        !Arguments-------------------------------------------------------------
        type(T_MainMI), pointer :: ObjMainMI
        type(T_Arrays ), pointer :: ObjHOF01, ObjHOF02

        !----------------------------------------------------------------------

        call KillHOF (ObjHOF01)
        call KillHOF (ObjHOF02)

        deallocate   (ObjMainMI)

    end subroutine KillMainMI

    !--------------------------------------------------------------------------

end program MainMI
