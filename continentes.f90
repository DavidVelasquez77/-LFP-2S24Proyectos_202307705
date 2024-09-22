module continenteModule
    use paisModule
    implicit none

    type :: continente
        character(len=:), allocatable :: nombre
        type(pais) :: pais(50)
        integer :: ipais = 1
    contains
        procedure :: crearContinente
    end type continente

    contains
    subroutine crearContinente (this, nombre)
        class(continente), intent(inout) :: this
        character(len=*), intent(in) :: nombre

        this%nombre = nombre
    end subroutine crearContinente
end module continenteModule