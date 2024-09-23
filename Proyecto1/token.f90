module token
    implicit none
    private
    public :: ElementoDatos
    
    ! Estructura principal para manejar elementos de datos
    type :: ElementoDatos
        private
        character(len=:), allocatable :: categoria
        character(len=:), allocatable :: contenido
        integer :: posicionVertical
        integer :: posicionHorizontal
        logical :: esValido
        real :: prioridad
    contains
        procedure :: configurarElemento
        procedure :: extraerCategoria
        procedure :: extraerContenido
        procedure :: obtenerPosicionV
        procedure :: obtenerPosicionH
        procedure :: validarElemento
        procedure :: calcularPrioridad
        procedure :: actualizarPosiciones
    end type ElementoDatos

contains
    ! Configura los valores iniciales del elemento
    subroutine configurarElemento(este, cat, cont, posV, posH)
        class(ElementoDatos), intent(inout) :: este
        character(len=*), intent(in) :: cat, cont
        integer, intent(in) :: posV, posH
        
        este%categoria = cat
        este%contenido = cont
        este%posicionVertical = posV
        este%posicionHorizontal = posH
        este%esValido = .true.
        este%prioridad = 1.0
        
        call este%validarElemento()
        call este%calcularPrioridad()
    end subroutine configurarElemento
    
    ! Extrae la categoría del elemento
    function extraerCategoria(este) result(catElemento)
        class(ElementoDatos), intent(in) :: este
        character(len=:), allocatable :: catElemento
        
        if (este%esValido) then
            catElemento = este%categoria
        else
            catElemento = "INDEFINIDO"
        endif
    end function extraerCategoria
    
    ! Extrae el contenido del elemento
    function extraerContenido(este) result(contElemento)
        class(ElementoDatos), intent(in) :: este
        character(len=:), allocatable :: contElemento
        
        if (este%esValido) then
            contElemento = este%contenido
        else
            contElemento = "VACIO"
        endif
    end function extraerContenido
    
    ! Obtiene la posición vertical
    function obtenerPosicionV(este) result(posV)
        class(ElementoDatos), intent(in) :: este
        integer :: posV
        
        posV = este%posicionVertical
    end function obtenerPosicionV
    
    subroutine encolar(este, dato)
        class(Cola), intent(inout) :: este
        class(*), intent(in) :: dato
        call este%elementos%insertar(dato)
    end subroutine encolar
    
    subroutine desencolar(este)
        class(Cola), intent(inout) :: este
        if (.not. este%estaVacia()) then
            call este%elementos%eliminar(1)
        end if
    end subroutine desencolar
    
    function estaVacia(este) result(vacia)
        class(Cola), intent(in) :: este
        logical :: vacia
        vacia = este%elementos%tamano == 0
    end function estaVacia
    
    subroutine apilar(este, dato)
        class(Pila), intent(inout) :: este
        class(*), intent(in) :: dato
        call este%elementos%insertar(dato)
    end subroutine apilar
    
    subroutine desapilar(este)
        class(Pila), intent(inout) :: este
        if (este%elementos%tamano > 0) then
            call este%elementos%eliminar(este%elementos%tamano)
        end if
    end subroutine desapilar
    
    function verTope(este) result(tope)
        class(Pila), intent(in) :: este
        class(*), allocatable :: tope
        if (este%elementos%tamano > 0) then
            allocate(tope, source=este%elementos%ultimo%dato)
        end if
    end function verTope


subroutine actualizarPosiciones(este, nuevaPosV, nuevaPosH)
    class(ElementoDatos), intent(inout) :: este
    integer, intent(in) :: nuevaPosV, nuevaPosH
    este%posicionVertical = nuevaPosV
    este%posicionHorizontal = nuevaPosH
    call este%calcularPrioridad()
end subroutine actualizarPosiciones

subroutine bloquearElemento(este)
    class(ElementoDatos), intent(inout) :: este
    este%estaBloqueado = .true.
end subroutine bloquearElemento

subroutine desbloquearElemento(este)
    class(ElementoDatos), intent(inout) :: este
    este%estaBloqueado = .false.
end subroutine desbloquearElemento

subroutine agregarHistorial(este, descripcion)
    class(ElementoDatos), intent(inout) :: este
    character(len=*), intent(in) :: descripcion
    call este%historialCambios%insertar(descripcion)
end subroutine agregarHistorial

subroutine revertirCambios(este)
    class(ElementoDatos), intent(inout)

    ! Obtiene la posición horizontal
    function obtenerPosicionH(este) result(posH)
        class(ElementoDatos), intent(in) :: este
        integer :: posH
        
        posH = este%posicionHorizontal
    end function obtenerPosicionH
    
    ! Valida el elemento
    subroutine validarElemento(este)
        class(ElementoDatos), intent(inout) :: este
        
        este%esValido = len(este%categoria) > 0 .and. &
                        len(este%contenido) > 0 .and. &
                        este%posicionVertical >= 0 .and. &
                        este%posicionHorizontal >= 0
    end subroutine validarElemento
    
    ! Calcula la prioridad del elemento
    subroutine calcularPrioridad(este)
        class(ElementoDatos), intent(inout) :: este
        
        if (este%esValido) then
            este%prioridad = 1.0 + (este%posicionVertical * 0.1) + &
                            (este%posicionHorizontal * 0.05)
        else
            este%prioridad = 0.0
        endif
    end subroutine calcularPrioridad
    
    ! Actualiza las posiciones del elemento
    subroutine actualizarPosiciones(este, nuevaPosV, nuevaPosH)
        class(ElementoDatos), intent(inout) :: este
        integer, intent(in) :: nuevaPosV, nuevaPosH
        
        este%posicionVertical = nuevaPosV
        este%posicionHorizontal = nuevaPosH
        call este%calcularPrioridad()
    end subroutine actualizarPosiciones
    
end module token 