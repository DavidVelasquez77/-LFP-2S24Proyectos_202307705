module informacionModule
    use analizadorModule 
    use paisModule
    use continenteModule
    implicit none 
    private
    public :: Informacion

    integer, parameter :: MAX_TOKENS = 500
    integer, parameter :: MAX_CONTINENTES = 1000

    type :: Informacion 
        private
        type(Token) :: tokens(MAX_TOKENS)
        character(:), allocatable :: nombreGrafica 
        type(continente) :: continentes(MAX_CONTINENTES)
        integer :: iContinentes = 1
        character(len=100) :: paisMenorSaturacion
        integer :: menorSaturacion
        character(len=100) :: banderaPaisMenorSaturacion
        integer :: poblacionMenorSaturacion
        logical :: hay_errores = .false.
    contains 
        procedure :: crearInformacion
        procedure :: iterarTokens
        procedure :: graficar 
        procedure :: encontrarPaisMenorSaturacion
    end type Informacion

contains
    subroutine crearInformacion(this, tokens)
        class(Informacion), intent(inout) :: this
        type(Token), intent(in) :: tokens(MAX_TOKENS)
        this%tokens = tokens
    end subroutine crearInformacion

    subroutine iterarTokens(this)
        class(Informacion), intent(inout) :: this
        integer :: i
        type(Continente) :: continente_actual
        type(Pais) :: pais_actual
        character(:), allocatable :: continenteSinComillas
        logical :: dentroContinente = .false., dentroPais = .false.
        character(:), allocatable :: nombrePais, bandera
        integer :: poblacion, saturacion

        do i = 1, MAX_TOKENS
            select case(trim(this%tokens(i)%lexema))
                case ("grafica")
                    this%nombreGrafica = this%tokens(i+5)%lexema
                case ("continente")
                    call iniciar_continente(continente_actual, this%tokens(i+5)%lexema)
                    dentroContinente = .true.
                case ("pais")
                    dentroPais = .true.
                case ("nombre")
                    if (dentroPais) nombrePais = this%tokens(i+2)%lexema
                case ("bandera")
                    if (dentroPais) bandera = this%tokens(i+2)%lexema
                case ("saturacion")
                    if (dentroPais) read(this%tokens(i+2)%lexema, *) saturacion
                case ("poblacion")
                    if (dentroPais) read(this%tokens(i+2)%lexema, *) poblacion
                case ("}")
                    if (dentroPais) then
                        call finalizar_pais(continente_actual, pais_actual, nombrePais, bandera, poblacion, saturacion)
                        dentroPais = .false.
                    else if (dentroContinente) then
                        call finalizar_continente(this, continente_actual)
                        dentroContinente = .false.
                    end if
            end select
        end do
        
        if (.not. this%hay_errores) then
            call this%graficar(this%continentes, this%nombreGrafica)
            call this%encontrarPaisMenorSaturacion()
        else
            print *, "Se encontraron errores. No se generará la gráfica."
        end if
    end subroutine iterarTokens

    subroutine encontrarPaisMenorSaturacion(this)
        class(Informacion), intent(inout) :: this
        integer :: i, j
        integer :: saturacionContinente, numPaisesContinente
        real :: saturacionPromedioContinente, menorSaturacionPromedio
        
        this%menorSaturacion = 100
        menorSaturacionPromedio = 100.0

        do i = 1, this%iContinentes - 1
            call calcular_saturacion_continente(this%continentes(i), saturacionContinente, numPaisesContinente)
            
            saturacionPromedioContinente = real(saturacionContinente) / max(1, real(numPaisesContinente))
            
            do j = 1, this%continentes(i)%ipais - 1
                call actualizar_pais_menor_saturacion(this, this%continentes(i)%pais(j), saturacionPromedioContinente, menorSaturacionPromedio)
            end do
        end do

        print *, "PAIS_MENOR_SATURACION:", trim(this%paisMenorSaturacion), &
                 "|", this%menorSaturacion, &
                 "|", trim(this%banderaPaisMenorSaturacion), &
                 "|", this%poblacionMenorSaturacion
    end subroutine encontrarPaisMenorSaturacion

    subroutine graficar(this, continentes, nombreGrafica)
        class(Informacion), intent(inout) :: this
        type(Continente), intent(in) :: continentes(MAX_CONTINENTES)
        character(len=*), intent(in) :: nombreGrafica
        integer, parameter :: unit = 15
        integer :: i, j, promedio
        character(:), allocatable :: nombreSinComillas, continenteSinComillas, color

        open(unit=unit, file="grafica.dot", status='replace', action='write')
        call escribir_encabezado_grafica(unit, nombreGrafica)
        
        do i = 1, this%iContinentes - 1
            continenteSinComillas = trim(continentes(i)%nombre)
            call escribir_conexion_grafica(unit, nombreGrafica, continenteSinComillas)
            
            promedio = escribir_paises_continente(unit, continentes(i), continenteSinComillas)
            
            call escribir_continente_grafica(unit, continenteSinComillas, promedio)
        end do
        
        write(unit, '(A)') '}'
        close(unit)
        
        call system('dot -Tpng grafica.dot -o grafica.png')
    end subroutine graficar

    ! Funciones y subrutinas auxiliares
    subroutine iniciar_continente(continente, nombre)
        type(Continente), intent(out) :: continente
        character(len=*), intent(in) :: nombre
        continente%nombre = trim(nombre(2:len_trim(nombre)-1))
        continente%ipais = 1
    end subroutine iniciar_continente

    subroutine finalizar_pais(continente, pais, nombre, bandera, poblacion, saturacion)
        type(Continente), intent(inout) :: continente
        type(Pais), intent(out) :: pais
        character(len=*), intent(in) :: nombre, bandera
        integer, intent(in) :: poblacion, saturacion
        pais = Pais(nombre, bandera, poblacion, saturacion)
        continente%pais(continente%ipais) = pais
        continente%ipais = continente%ipais + 1
    end subroutine finalizar_pais

    subroutine finalizar_continente(info, continente)
        class(Informacion), intent(inout) :: info
        type(Continente), intent(in) :: continente
        info%continentes(info%iContinentes) = continente
        info%iContinentes = info%iContinentes + 1
    end subroutine finalizar_continente

    subroutine calcular_saturacion_continente(continente, saturacion_total, num_paises)
        type(Continente), intent(in) :: continente
        integer, intent(out) :: saturacion_total, num_paises
        integer :: j
        saturacion_total = 0
        num_paises = 0
        do j = 1, continente%ipais - 1
            saturacion_total = saturacion_total + continente%pais(j)%saturacion
            num_paises = num_paises + 1
        end do
    end subroutine calcular_saturacion_continente

    subroutine actualizar_pais_menor_saturacion(info, pais, saturacion_promedio, menor_saturacion_promedio)
        class(Informacion), intent(inout) :: info
        type(Pais), intent(in) :: pais
        real, intent(in) :: saturacion_promedio
        real, intent(inout) :: menor_saturacion_promedio
        if (pais%saturacion < info%menorSaturacion .or. &
            (pais%saturacion == info%menorSaturacion .and. saturacion_promedio < menor_saturacion_promedio)) then
            info%menorSaturacion = pais%saturacion
            info%paisMenorSaturacion = pais%nombre
            info%banderaPaisMenorSaturacion = pais%bandera
            info%poblacionMenorSaturacion = pais%poblacion
            menor_saturacion_promedio = saturacion_promedio
        end if
    end subroutine actualizar_pais_menor_saturacion

    subroutine escribir_encabezado_grafica(unit, nombreGrafica)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: nombreGrafica
        write(unit, '(A)') 'digraph G {'
        write(unit, '(A)') trim(nombreGrafica) // '[shape=Mdiamond];'
    end subroutine escribir_encabezado_grafica

    subroutine escribir_conexion_grafica(unit, nombreGrafica, continenteSinComillas)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: nombreGrafica, continenteSinComillas
        write(unit, '(A)') ' ' // trim(nombreGrafica) // ' -> "' // trim(continenteSinComillas) // '";'
    end subroutine escribir_conexion_grafica

    function escribir_paises_continente(unit, continente, continenteSinComillas) result(promedio)
        integer, intent(in) :: unit
        type(Continente), intent(in) :: continente
        character(len=*), intent(in) :: continenteSinComillas
        integer :: promedio, j, sumaPaises, numPaises
        character(:), allocatable :: nombreSinComillas, color

        sumaPaises = 0
        numPaises = 0
        do j = 1, continente%ipais-1
            nombreSinComillas = trim(continente%pais(j)%nombre(2:len_trim(continente%pais(j)%nombre)-1))
            color = get_color(continente%pais(j)%saturacion)
            
            write(unit, '(A)') '    "' // trim(nombreSinComillas) // '"[style=filled, shape=box, label="' // &
                               trim(nombreSinComillas) // '\n' // &
                               trim(adjustl(int2str(continente%pais(j)%saturacion))) // &
                               '%", fillcolor=' // trim(color) // '];'
            write(unit, '(A)') '    "' // trim(continenteSinComillas) // '" -> "' // trim(nombreSinComillas) // '";'
            
            sumaPaises = sumaPaises + continente%pais(j)%saturacion
            numPaises = numPaises + 1
        end do
        
        promedio = sumaPaises / max(1, numPaises)
    end function escribir_paises_continente

    subroutine escribir_continente_grafica(unit, continenteSinComillas, promedio)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: continenteSinComillas
        integer, intent(in) :: promedio
        character(:), allocatable :: color
        
        color = get_color(promedio)
        write(unit, '(A)') '    "' // trim(continenteSinComillas) // '"[style=filled, shape=box, label="' // &
                           trim(continenteSinComillas) // '\n' // &
                           trim(adjustl(int2str(promedio))) // &
                           '%", fillcolor=' // trim(color) // '];'
    end subroutine escribir_continente_grafica

    function get_color(saturacion) result(color)
        integer, intent(in) :: saturacion
        character(:), allocatable :: color
        select case (saturacion)
            case (76:)
                color = "red"
            case (61:75)
                color = "orange"
            case (46:60)
                color = "yellow"
            case (31:45)
                color = "green"
            case (16:30)
                color = "blue"
            case default
                color = "white"
        end select
    end function get_color

    function int2str(i) result(res)
        integer, intent(in) :: i
        character(:), allocatable :: res
        character(range(i)+2) :: tmp
        write(tmp,'(i0)') i
        res = trim(tmp)
    end function int2str

end module informacionModule