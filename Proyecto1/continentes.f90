module continente
    use estructurasBase, only: zonaTerritorial
    implicit none
    private
    public :: regionGeografica

    type :: coordenadaGeografica
        real :: latitud
        real :: longitud
    end type

    type ::Regional
        integer :: poblacionTotal
        real :: superficieKm2
        real :: densidadPoblacional
        character(len=20) :: clasificacionONU
    end type

    type :: regionGeografica
        private
        character(len=50) :: identificador
        type(coordenadaGeografica) :: ubicacionCentral
        type(estadisticaRegional) :: datos
        type(zonaTerritorial), allocatable :: subregiones(:)
        integer :: contadorSubregiones
        logical :: esActiva
    contains
        procedure :: inicializarRegion
        procedure :: registrarSubregion
        procedure :: obtenerEstadisticas
        procedure :: actualizarDatos
        procedure :: verificarEstado
    end type regionGeografica

contains
    subroutine inicializarRegion(self, id, lat, long)
        class(regionGeografica), intent(inout) :: self
        character(len=*), intent(in) :: id
        real, intent(in) :: lat, long
        
        self%identificador = id
        self%ubicacionCentral%latitud = lat
        self%ubicacionCentral%longitud = long
        self%contadorSubregiones = 0
        self%esActiva = .true.
        
        allocate(self%subregiones(25))
        
        ! Inicialización de estadísticas
        self%datos%poblacionTotal = 0
        self%datos%superficieKm2 = 0.0
        self%datos%densidadPoblacional = 0.0
        self%datos%clasificacionONU = "NO_CLASIFICADO"
    end subroutine

    subroutine registrarSubregion(self, nueva_subregion)
        class(regionGeografica), intent(inout) :: self
        type(zonaTerritorial), intent(in) :: nueva_subregion
        
        if (.not. self%esActiva) then
            print *, "Error: Región inactiva para registros"
            return
        end if
        
        if (self%contadorSubregiones >= 25) then
            call manejarDesbordamiento()
            return
        end if
        
        self%contadorSubregiones = self%contadorSubregiones + 1
        self%subregiones(self%contadorSubregiones) = nueva_subregion
        call actualizarEstadisticasGlobales(self)
    end subroutine

    function obtenerEstadisticas(self) result(resumen)
        class(regionGeografica), intent(in) :: self
        type(estadisticaRegional) :: resumen
        
        if (verificarIntegridad(self)) then
            resumen = self%datos
        else
            call generarRegistroError("Error en integridad de datos")
        end if
    end function

    subroutine actualizarDatos(self, nuevosDatos)
        class(regionGeografica), intent(inout) :: self
        type(estadisticaRegional), intent(in) :: nuevosDatos
        
        if (validarDatosEntrada(nuevosDatos)) then
            self%datos = nuevosDatos
            call recalcularMetricas(self)
        end if
    end subroutine

    function verificarEstado(self) result(estado)
        class(regionGeografica), intent(in) :: self
        logical :: estado
        
        estado = self%esActiva .and. self%contadorSubregiones > 0
    end function

    ! Subrutinas auxiliares privadas
    subroutine manejarDesbordamiento()
        print *, "Alerta: Capacidad máxima alcanzada"
    end subroutine

    subroutine actualizarEstadisticasGlobales(self)
        class(regionGeografica), intent(inout) :: self
        ! Lógica de actualización
    end subroutine

    function validarDatosEntrada(datos) result(esValido)
        type(estadisticaRegional), intent(in) :: datos
        logical :: esValido
        esValido = .true.
    end function

    function verificarIntegridad(self) result(integro)
        class(regionGeografica), intent(in) :: self
        logical :: integro
        integro = .true.
    end function

    subroutine recalcularMetricas(self)
        class(regionGeografica), intent(inout) :: self
        ! Lógica de recálculo
    end subroutine

    subroutine generarRegistroError(mensaje)
        character(len=*), intent(in) :: mensaje
        print *, "Error registrado: ", mensaje
    end subroutine

end module continente