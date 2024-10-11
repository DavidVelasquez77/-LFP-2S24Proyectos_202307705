module errorModule
    implicit none
    private
    public :: Error

    type :: Error
        private
        character(:), allocatable :: caracter
        character(:), allocatable :: descripcion
        integer :: linea
        integer :: columna
    contains
        procedure :: crearError
        procedure :: obtenerCaracter
        procedure :: obtenerDescripcion
        procedure :: obtenerLinea
        procedure :: obtenerColumna
    end type Error

contains
    subroutine crearError(this, caracter, descripcion, linea, columna)
        class(Error), intent(inout) :: this
        character(len=*), intent(in) :: caracter
        character(len=*), intent(in) :: descripcion
        integer, intent(in) :: linea
        integer, intent(in) :: columna

        this%caracter = caracter
        this%descripcion = descripcion
        this%linea = linea
        this%columna = columna
    end subroutine crearError

    function obtenerCaracter(this) result(caracter)
        class(Error), intent(in) :: this
        character(:), allocatable :: caracter
        caracter = this%caracter
    end function obtenerCaracter

    function obtenerDescripcion(this) result(descripcion)
        class(Error), intent(in) :: this
        character(:), allocatable :: descripcion
        descripcion = this%descripcion
    end function obtenerDescripcion

    function obtenerLinea(this) result(linea)
        class(Error), intent(in) :: this
        integer :: linea
        linea = this%linea
    end function obtenerLinea

    function obtenerColumna(this) result(columna)
        class(Error), intent(in) :: this
        integer :: columna
        columna = this%columna
    end function obtenerColumna

end module errorModule