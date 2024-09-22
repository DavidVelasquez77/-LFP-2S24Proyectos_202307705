module paisModule
    implicit none

    type :: pais
        character(len=:), allocatable :: nombre, bandera
        integer :: poblacion, saturacion
    contains
        procedure :: crearPais
        end type pais

    contains

    subroutine crearPais(this, nombre, bandera, poblacion, saturacion)
        class(pais), intent(inout) :: this
        character(len=*), intent(in) :: nombre, bandera
        integer, intent(in) :: poblacion, saturacion

        this%nombre = nombre
        this%bandera = bandera
        this%poblacion = poblacion
        this%saturacion = saturacion
    end subroutine crearPais
    
end module paisModule