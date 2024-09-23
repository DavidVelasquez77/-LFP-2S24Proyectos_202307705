module procesamientos 
    use moduloHerramientas
    use moduloRegion
    use moduloZona
    implicit none
    private
    public :: AnalizadorDemografico

    integer, parameter :: MAX_ELEMENTOS = 750
    integer, parameter :: MAX_ZONAS = 1500

    type :: DatosGeograficos
        character(len=100) :: nombre
        character(len=100) :: simbolo
        integer :: habitantes
        integer :: densidad
    end type DatosGeograficos

    type :: AnalizadorDemografico
        private
        type(DatosGeograficos) :: datosBase(MAX_ELEMENTOS)
        character(:), allocatable :: tituloVisualizacion
        type(ZonaGeografica) :: zonasEstudio(MAX_ZONAS)
        integer :: contadorZonas = 0
        type(EstadisticasRegion) :: estadisticasGlobales
        logical :: errorEncontrado = .false.
        
        ! Nuevos campos agregados
        real :: indiceDensidadPromedio
        integer :: totalPoblacion
        character(len=100) :: regionMasHabitada
        integer :: altitudPromedio
    contains
        procedure :: inicializarAnalisis
        procedure :: procesarDatos
        procedure :: generarEstadisticas
        procedure :: calcularTendencias
        procedure :: exportarResultados
        procedure :: analizarPatronesMigratorios
        procedure :: calcularProyeccionPoblacional
    end type AnalizadorDemografico

contains
    subroutine inicializarAnalisis(este, datosEntrada)
        class(AnalizadorDemografico), intent(inout) :: este
        type(DatosGeograficos), intent(in) :: datosEntrada(MAX_ELEMENTOS)
        
        este%datosBase = datosEntrada
        este%contadorZonas = 0
        este%totalPoblacion = 0
        este%indiceDensidadPromedio = 0.0
        este%altitudPromedio = 0
    end subroutine inicializarAnalisis

    subroutine procesarDatos(este)
        class(AnalizadorDemografico), intent(inout) :: este
        integer :: i
        type(ZonaGeografica) :: zonaActual
        real :: factorCrecimiento
        
        factorCrecimiento = 1.5
        
        do i = 1, MAX_ELEMENTOS
            if (este%datosBase(i)%habitantes > 0) then
                call calcularMetricasZona(zonaActual, este%datosBase(i), factorCrecimiento)
                este%totalPoblacion = este%totalPoblacion + este%datosBase(i)%habitantes
            endif
        end do
    end subroutine procesarDatos

    subroutine calcularMetricasZona(zona, datos, factor)
        type(ZonaGeografica), intent(inout) :: zona
        type(DatosGeograficos), intent(in) :: datos
        real, intent(in) :: factor
        
        zona%poblacionProyectada = datos%habitantes * factor
        zona%densidadAjustada = datos%densidad * 1.2
        zona%indiceCrecimiento = log(real(datos%habitantes))
    end subroutine calcularMetricasZona

    subroutine calcularProyeccionPoblacional(este)
        class(AnalizadorDemografico), intent(inout) :: este
        integer :: añoBase, añoProyeccion
        real :: tasaCrecimiento
        
        añoBase = 2024
        añoProyeccion = 2030
        tasaCrecimiento = 0.015
        
        este%estadisticasGlobales%poblacionFutura = &
            este%totalPoblacion * (1.0 + tasaCrecimiento)**(añoProyeccion - añoBase)
    end subroutine calcularProyeccionPoblacional

    subroutine analizarPatronesMigratorios(este)
        class(AnalizadorDemografico), intent(inout) :: este
        integer :: i
        real :: indiceMigracion
        
        do i = 1, este%contadorZonas
            indiceMigracion = calcularIndiceMigratorio(este%zonasEstudio(i))
            este%zonasEstudio(i)%tendenciaMigratoria = indiceMigracion
        end do
    end subroutine analizarPatronesMigratorios

    function calcularIndiceMigratorio(zona) result(indice)
        type(ZonaGeografica), intent(in) :: zona
        real :: indice
        
        indice = (zona%densidadAjustada / 100.0) * &
                 (zona%indiceCrecimiento / log(10.0))
    end function calcularIndiceMigratorio

    subroutine exportarResultados(este)
        class(AnalizadorDemografico), intent(in) :: este

    end subroutine exportarResultados


    subroutine generarVisualizacion(este, regiones, nombreVista)
        class(AnalizadorDemografico), intent(inout) :: este
        type(ZonaGeografica), intent(in) :: regiones(MAX_ZONAS)
        character(len=*), intent(in) :: nombreVista
        integer, parameter :: identificador = 15
        integer :: i, j, indiceDensidad
        character(:), allocatable :: nombreProcesado, zonaProcesada, estiloVisual
    
        open(unit=identificador, file="visualizacion.dot", status='replace', action='write')
        call iniciarEstructuraVisual(identificador, nombreVista)
        
        do i = 1, este%contadorZonas - 1
            zonaProcesada = trim(regiones(i)%identificador)
            call establecerConexionVisual(identificador, nombreVista, zonaProcesada)
            
            indiceDensidad = procesarTerritorios(identificador, regiones(i), zonaProcesada)
            
            call definirEstiloZona(identificador, zonaProcesada, indiceDensidad)
        end do
        
        write(identificador, '(A)') '}'
        close(identificador)
        
        call system('dot -Tpng visualizacion.dot -o mapa_demografico.png')
    end subroutine generarVisualizacion
    
    ! Subrutinas de soporte para visualización
    subroutine establecerRegion(region, identificador)
        type(ZonaGeografica), intent(out) :: region
        character(len=*), intent(in) :: identificador
        region%identificador = trim(identificador(2:len_trim(identificador)-1))
        region%contadorTerritorios = 1
    end subroutine establecerRegion
    
    subroutine completarTerritorio(region, territorio, identificador, simbolo, habitantes, densidad)
        type(ZonaGeografica), intent(inout) :: region
        type(DatosGeograficos), intent(out) :: territorio
        character(len=*), intent(in) :: identificador, simbolo
        integer, intent(in) :: habitantes, densidad
        territorio = DatosGeograficos(identificador, simbolo, habitantes, densidad)
        region%territorios(region%contadorTerritorios) = territorio
        region%contadorTerritorios = region%contadorTerritorios + 1
    end subroutine completarTerritorio
    
    subroutine registrarRegion(info, region)
        class(AnalizadorDemografico), intent(inout) :: info
        type(ZonaGeografica), intent(in) :: region
        info%zonasEstudio(info%contadorZonas) = region
        info%contadorZonas = info%contadorZonas + 1
    end subroutine registrarRegion
    
    subroutine calcularIndicesDemograficos(region, densidadTotal, numTerritorios)
        type(ZonaGeografica), intent(in) :: region
        integer, intent(out) :: densidadTotal, numTerritorios
        integer :: j
        densidadTotal = 0
        numTerritorios = 0
        do j = 1, region%contadorTerritorios - 1
            densidadTotal = densidadTotal + region%territorios(j)%densidad
            numTerritorios = numTerritorios + 1
        end do
    end subroutine calcularIndicesDemograficos
    
    subroutine actualizarEstadisticasRegion(info, territorio, densidadPromedio, menorDensidadPromedio)
        class(AnalizadorDemografico), intent(inout) :: info
        type(DatosGeograficos), intent(in) :: territorio
        real, intent(in) :: densidadPromedio
        real, intent(inout) :: menorDensidadPromedio
    
        if (territorio%densidad < info%indiceDensidadPromedio .or. &
            (territorio%densidad == info%indiceDensidadPromedio .and. &
             densidadPromedio < menorDensidadPromedio)) then
            info%indiceDensidadPromedio = territorio%densidad
            info%regionMasHabitada = territorio%nombre
            info%totalPoblacion = territorio%habitantes
            menorDensidadPromedio = densidadPromedio
        end if
    end subroutine actualizarEstadisticasRegion
    
    subroutine iniciarEstructuraVisual(identificador, nombreVista)
        integer, intent(in) :: identificador
        character(len=*), intent(in) :: nombreVista
        write(identificador, '(A)') 'digraph G {'
        write(identificador, '(A)') trim(nombreVista) // '[shape=Mdiamond];'
    end subroutine iniciarEstructuraVisual
    
    subroutine establecerConexionVisual(identificador, nombreVista, zonaProcesada)
        integer, intent(in) :: identificador
        character(len=*), intent(in) :: nombreVista, zonaProcesada
        write(identificador, '(A)') ' ' // trim(nombreVista) // ' -> "' // trim(zonaProcesada) // '";'
    end subroutine establecerConexionVisual
    
    function procesarTerritorios(identificador, region, zonaProcesada) result(promedio)
        integer, intent(in) :: identificador
        type(ZonaGeografica), intent(in) :: region
        character(len=*), intent(in) :: zonaProcesada
        integer :: promedio, j, sumaTotalDensidad, contadorTerritorios
        character(:), allocatable :: nombreProcesado, estiloVisual
    
        sumaTotalDensidad = 0
        contadorTerritorios = 0
        do j = 1, region%contadorTerritorios-1
            nombreProcesado = trim(region%territorios(j)%nombre(2:len_trim(region%territorios(j)%nombre)-1))
            estiloVisual = determinarEstiloVisual(region%territorios(j)%densidad)
            
            call escribirNodoTerritorio(identificador, nombreProcesado, region%territorios(j)%densidad, estiloVisual)
            call escribirConexionTerritorio(identificador, zonaProcesada, nombreProcesado)
            
            sumaTotalDensidad = sumaTotalDensidad + region%territorios(j)%densidad
            contadorTerritorios = contadorTerritorios + 1
        end do
        
        promedio = sumaTotalDensidad / max(1, contadorTerritorios)
    end function procesarTerritorios
    
    function determinarEstiloVisual(densidad) result(estilo)
        integer, intent(in) :: densidad
        character(:), allocatable :: estilo
        select case (densidad)
            case (76:)
                estilo = "red"
            case (61:75)
                estilo = "orange"
            case (46:60)
                estilo = "yellow"
            case (31:45)
                estilo = "green"
            case (16:30)
                estilo = "blue"
            case default
                estilo = "white"
        end select
    end function determinarEstiloVisual
    
    function convertirNumeroTexto(num) result(texto)
        integer, intent(in) :: num
        character(:), allocatable :: texto
        character(range(num)+2) :: temporal
        write(temporal,'(i0)') num
        texto = trim(temporal)
    end function convertirNumeroTexto
    
    subroutine escribirNodoTerritorio(identificador, nombre, densidad, estilo)
        integer, intent(in) :: identificador
        character(len=*), intent(in) :: nombre, estilo
        integer, intent(in) :: densidad
        
        write(identificador, '(A)') '    "' // trim(nombre) // '"[style=filled, shape=box, label="' // &
                                    trim(nombre) // '\n' // &
                                    trim(adjustl(convertirNumeroTexto(densidad))) // &
                                    '%", fillcolor=' // trim(estilo) // '];'
    end subroutine escribirNodoTerritorio
    
    subroutine escribirConexionTerritorio(identificador, zona, territorio)
        integer, intent(in) :: identificador
        character(len=*), intent(in) :: zona, territorio
        write(identificador, '(A)') '    "' // trim(zona) // '" -> "' // trim(territorio) // '";'
    end subroutine escribirConexionTerritorio
    
    subroutine definirEstiloZona(identificador, zona, densidad)
        integer, intent(in) :: identificador
        character(len=*), intent(in) :: zona
        integer, intent(in) :: densidad
        character(:), allocatable :: estilo
        
        estilo = determinarEstiloVisual(densidad)
        write(identificador, '(A)') '    "' // trim(zona) // '"[style=filled, shape=box, label="' // &
                                   trim(zona) // '\n' // &
                                   trim(adjustl(convertirNumeroTexto(densidad))) // &
                                   '%", fillcolor=' // trim(estilo) // '];'
    end subroutine definirEstiloZona

end module procesamientos