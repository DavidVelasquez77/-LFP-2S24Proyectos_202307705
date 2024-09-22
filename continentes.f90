module continenteModule
    use paisModule, only: pais
    implicit none
    private
    public :: continente

    type :: continente
        private
        character(len=:), allocatable :: nombre
        type(pais), allocatable :: paises(:)
        integer :: num_paises = 0
    contains
        procedure :: crearContinente
        procedure :: agregarPais
        procedure :: getNombre
        procedure :: getPais
        procedure :: getNumPaises
    end type continente

contains

    subroutine crearContinente(this, nombre)
        class(continente), intent(inout) :: this
        character(len=*), intent(in) :: nombre

        this%nombre = nombre
        allocate(this%paises(50))
    end subroutine crearContinente

    subroutine agregarPais(this, nuevo_pais)
        class(continente), intent(inout) :: this
        type(pais), intent(in) :: nuevo_pais

        if (this%num_paises < 50) then
            this%num_paises = this%num_paises + 1
            this%paises(this%num_paises) = nuevo_pais
        else
            print *, "Error: Máximo número de países alcanzado"
        end if
    end subroutine agregarPais

    function getNombre(this) result(nombre)
        class(continente), intent(in) :: this
        character(len=:), allocatable :: nombre
        nombre = this%nombre
    end function getNombre

    function getPais(this, index) result(pais_result)
        class(continente), intent(in) :: this
        integer, intent(in) :: index
        type(pais) :: pais_result

        if (index > 0 .and. index <= this%num_paises) then
            pais_result = this%paises(index)
        else
            print *, "Error: Índice de país inválido"
        end if
    end function getPais

    function getNumPaises(this) result(num_paises)
        class(continente), intent(in) :: this
        integer :: num_paises
        num_paises = this%num_paises
    end function getNumPaises

end module continenteModule