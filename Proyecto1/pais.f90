module paisModule
    implicit none
    private
    public :: pais

    type :: pais
        private
        character(len=:), allocatable :: nombre, bandera
        integer :: poblacion, saturacion
    contains
        procedure :: crearPais
        procedure :: getNombre
        procedure :: getBandera
        procedure :: getPoblacion
        procedure :: getSaturacion
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

    function getNombre(this) result(nombre)
        class(pais), intent(in) :: this
        character(len=:), allocatable :: nombre
        nombre = this%nombre
    end function getNombre

    function getBandera(this) result(bandera)
        class(pais), intent(in) :: this
        character(len=:), allocatable :: bandera
        bandera = this%bandera
    end function getBandera

    function getPoblacion(this) result(poblacion)
        class(pais), intent(in) :: this
        integer :: poblacion
        poblacion = this%poblacion
    end function getPoblacion

    function getSaturacion(this) result(saturacion)
        class(pais), intent(in) :: this
        integer :: saturacion
        saturacion = this%saturacion
    end function getSaturacion

end module paisModule