module tokenModule
    implicit none
    private
    public :: Token

    type :: Token
        private
        character(len=:), allocatable :: nombre
        character(len=:), allocatable :: lexema
        integer :: linea, columna
    contains
        procedure :: crearToken
        procedure :: getNombre
        procedure :: getLexema
        procedure :: getLinea
        procedure :: getColumna
    end type Token

contains

    subroutine crearToken(this, nombre, lexema, linea, columna)
        class(Token), intent(inout) :: this
        character(len=*), intent(in) :: nombre, lexema
        integer, intent(in) :: linea, columna

        this%nombre = nombre
        this%lexema = lexema
        this%linea = linea
        this%columna = columna
    end subroutine crearToken

    function getNombre(this) result(nombre)
        class(Token), intent(in) :: this
        character(len=:), allocatable :: nombre
        nombre = this%nombre
    end function getNombre

    function getLexema(this) result(lexema)
        class(Token), intent(in) :: this
        character(len=:), allocatable :: lexema
        lexema = this%lexema
    end function getLexema

    function getLinea(this) result(linea)
        class(Token), intent(in) :: this
        integer :: linea
        linea = this%linea
    end function getLinea

    function getColumna(this) result(columna)
        class(Token), intent(in) :: this
        integer :: columna
        columna = this%columna
    end function getColumna

end module tokenModule