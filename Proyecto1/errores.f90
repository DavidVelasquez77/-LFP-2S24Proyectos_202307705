module gestionExcepciones
    implicit none
    private
    public :: ExcepcionSistema

    ! Tipos de severidad para las excepciones
    integer, parameter :: NIVEL_CRITICO = 3
    integer, parameter :: NIVEL_ADVERTENCIA = 2
    integer, parameter :: NIVEL_INFO = 1

    ! Estructura para ubicación en archivo
    type :: PosicionArchivo
        integer :: numeroLinea
        integer :: posicionInicial
        integer :: posicionFinal
        character(len=100) :: nombreArchivo
    end type

    ! Estructura para detalles técnicos
    type :: DetallesTecnicos
        character(len=50) :: codigoHexadecimal
        character(len=30) :: moduloOrigen
        logical :: requiereReinicio
        integer :: contadorOcurrencias
    end type

    ! Tipo principal para excepciones
    type :: ExcepcionSistema
        private
        type(PosicionArchivo) :: ubicacion
        type(DetallesTecnicos) :: detalles
        character(len=200) :: mensajeDetallado
        integer :: nivelSeveridad
        logical :: fueManejada
        real :: timestampCreacion
    contains
        procedure :: inicializarExcepcion
        procedure :: registrarDetalles
        procedure :: obtenerInformacionCompleta
        procedure :: actualizarEstado
        procedure :: verificarSeveridad
    end type ExcepcionSistema

contains
    subroutine inicializarExcepcion(self, mensaje, nivel, archivo, linea)
        class(ExcepcionSistema), intent(inout) :: self
        character(len=*), intent(in) :: mensaje
        integer, intent(in) :: nivel
        character(len=*), intent(in) :: archivo
        integer, intent(in) :: linea
        

        
        call establecerTimestamp(self)
        call inicializarDetallesTecnicos(self)
    end subroutine

    subroutine registrarDetalles(self, codigo, modulo)
        class(ExcepcionSistema), intent(inout) :: self
        character(len=*), intent(in) :: codigo
        character(len=*), intent(in) :: modulo

        call evaluarNecesidadReinicio(self)
    end subroutine

    function obtenerInformacionCompleta(self) result(infoCompleta)
        class(ExcepcionSistema), intent(in) :: self
        character(len=500) :: infoCompleta
        
        infoCompleta = construirMensajeDetallado(self)
        call registrarEnLog(infoCompleta)
    end function

    subroutine actualizarEstado(self, estado)
        class(ExcepcionSistema), intent(inout) :: self
        logical, intent(in) :: estado
        
        self%fueManejada = estado
        if (estado) then
            self%detalles%contadorOcurrencias = self%detalles%contadorOcurrencias + 1
        end if
    end subroutine

    function verificarSeveridad(self) result(esCritico)
        class(ExcepcionSistema), intent(in) :: self
        logical :: esCritico
        
        esCritico = self%nivelSeveridad >= NIVEL_CRITICO
    end function

    ! Subrutinas auxiliares privadas
    subroutine establecerTimestamp(self)
        class(ExcepcionSistema), intent(inout) :: self
        integer :: valores(8)
        call date_and_time(values=valores)
        self%timestampCreacion = real(valores(5)*3600 + valores(6)*60 + valores(7))
    end subroutine

    subroutine inicializarDetallesTecnicos(self)
        class(ExcepcionSistema), intent(inout) :: self
        self%detalles%requiereReinicio = .false.
        self%detalles%contadorOcurrencias = 0
    end subroutine

    subroutine evaluarNecesidadReinicio(self)
        class(ExcepcionSistema), intent(inout) :: self
        self%detalles%requiereReinicio = self%nivelSeveridad == NIVEL_CRITICO
    end subroutine

    function construirMensajeDetallado(self) result(mensaje)
        class(ExcepcionSistema), intent(in) :: self
        character(len=500) :: mensaje
        
        write(mensaje, '(A,A,A,I0,A,A,A,I0)') &
            trim(self%mensajeDetallado), &
            " [Archivo: ", trim(self%ubicacion%nombreArchivo), &
            " Línea: ", self%ubicacion%numeroLinea, &
            " Código: ", trim(self%detalles%codigoHexadecimal), &
            " Ocurrencias: ", self%detalles%contadorOcurrencias
    end function

    subroutine registrarEnLog(mensaje)
        character(len=*), intent(in) :: mensaje
        integer :: unidadArchivo
        
        open(newunit=unidadArchivo, file='registro_errores.log', position='append')
        write(unidadArchivo, '(A)') trim(mensaje)
        close(unidadArchivo)
    end subroutine

end module gestionExcepciones