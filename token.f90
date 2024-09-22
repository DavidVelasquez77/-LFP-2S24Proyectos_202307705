module tokenModule
    implicit none
    private
    public :: Token, TokenBuilder

    type :: Token
        private
        character(len=:), allocatable :: nombre
        character(len=:), allocatable :: lexema
        integer :: linea
        integer :: columna
    contains
        procedure :: getNombre
        procedure :: getLexema
        procedure :: getLinea
        procedure :: getColumna
    end type Token

    type :: TokenBuilder
        private
        character(len=:), allocatable :: nombre
        character(len=:), allocatable :: lexema
        integer :: linea
        integer :: columna
    contains
        procedure :: setNombre
        procedure :: setLexema
        procedure :: setPosicion
        procedure :: build
    end type TokenBuilder

contains
    function newTokenBuilder() result(builder)
        type(TokenBuilder) :: builder
        builder%linea = 0
        builder%columna = 0
    end function newTokenBuilder

    function setNombre(this, nombre) result(builder)
        class(TokenBuilder), intent(in) :: this
        character(len=*), intent(in) :: nombre
        type(TokenBuilder) :: builder
        
        builder = this
        builder%nombre = nombre
    end function setNombre

    function setLexema(this, lexema) result(builder)
        class(TokenBuilder), intent(in) :: this
        character(len=*), intent(in) :: lexema
        type(TokenBuilder) :: builder
        
        builder = this
        builder%lexema = lexema
    end function setLexema

    function setPosicion(this, linea, columna) result(builder)
        class(TokenBuilder), intent(in) :: this
        integer, intent(in) :: linea, columna
        type(TokenBuilder) :: builder
        
        builder = this
        builder%linea = linea
        builder%columna = columna
    end function setPosicion

    function build(this) result(new_token)
        class(TokenBuilder), intent(in) :: this
        type(Token) :: new_token
        
        new_token%nombre = this%nombre
        new_token%lexema = this%lexema
        new_token%linea = this%linea
        new_token%columna = this%columna
    end function build

    ! Funciones de acceso para Token
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