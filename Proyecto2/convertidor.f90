MODULE creador_interfaz
    use administrador_elementos
    use validador_web
    use estructuras_base
    implicit none

    type :: elemento_web
        character(50) :: tipo
        character(50) :: id
        character(100) :: contenido
        character(50) :: clase
        integer :: nivel_anidacion
        logical :: es_auto_cerrado
        character(50), allocatable :: atributos(:)
        character(50), allocatable :: valores(:)
    end type

    type :: plantilla_elemento
        character(50) :: nombre
        character(200) :: estructura_base
        logical :: requiere_cierre
    end type

    type(plantilla_elemento), parameter :: TIPOS_ELEMENTOS(5) = [ &
        plantilla_elemento("entrada", '<input type="%s" id="%s" value="%s"/>', .false.), &
        plantilla_elemento("etiqueta", '<label id="%s">%s</label>', .true.), &
        plantilla_elemento("contenedor", '<div id="%s" class="%s">', .true.), &
        plantilla_elemento("boton", '<button id="%s" type="button">%s</button>', .true.), &
        plantilla_elemento("area_texto", '<textarea id="%s">%s</textarea>', .true.) &
    ]

contains

function iniciar_proceso() result(exito)
    logical :: exito
    character(100) :: ruta_salida
    
    exito = .false.
    ruta_salida = "salida/"
    
    if (validar_componentes()) then
        call procesar_estructura(ruta_salida)
        call aplicar_estilos(ruta_salida)
        exito = .true.
    endif
end function

subroutine procesar_estructura(ruta)
    character(*), intent(in) :: ruta
    integer :: unidad_doc
    
    open(newunit=unidad_doc, file=trim(ruta)//'interfaz.html', status='replace')
    
    call insertar_estructura_base(unidad_doc)
    call construir_cuerpo_web(unidad_doc)
    call cerrar_estructura(unidad_doc)
    
    close(unidad_doc)
end subroutine

subroutine insertar_estructura_base(unidad)
    integer, intent(in) :: unidad
    
    write(unidad, '(a)') '<!DOCTYPE html>'
    write(unidad, '(a)') '<html lang="es">'
    write(unidad, '(a)') '<head>'
    write(unidad, '(a)') '    <meta charset="UTF-8">'
    write(unidad, '(a)') '    <meta name="viewport" content="width=device-width, initial-scale=1.0">'
    write(unidad, '(a)') '    <link rel="stylesheet" href="visual.css">'
    write(unidad, '(a)') '    <title>Interfaz Generada</title>'
    write(unidad, '(a)') '</head>'
    write(unidad, '(a)') '<body>'
end subroutine

subroutine construir_cuerpo_web(unidad)
    integer, intent(in) :: unidad
    integer :: i
    type(elemento_web) :: elemento_actual
    
    do i = 1, total_elementos
        elemento_actual = obtener_elemento(i)
        if (es_elemento_raiz(elemento_actual)) then
            call procesar_elemento_web(unidad, elemento_actual)
        endif
    end do
end subroutine

recursive subroutine procesar_elemento_web(unidad, elemento)
    integer, intent(in) :: unidad
    type(elemento_web), intent(in) :: elemento
    character(1000) :: linea_html
    character(4) :: sangria
    integer :: i
    
    sangria = repeat('    ', elemento%nivel_anidacion)
    
    ! Generar HTML según el tipo de elemento
    select case (trim(elemento%tipo))
        case ('contenedor')
            write(unidad, '(a,a)') sangria, '<div class="contenedor-flex" id="'//trim(elemento%id)//'">'
            call procesar_hijos(unidad, elemento%id, elemento%nivel_anidacion + 1)
            write(unidad, '(a,a)') sangria, '</div>'
            
        case ('entrada')
            linea_html = generar_entrada(elemento)
            write(unidad, '(a,a)') sangria, trim(linea_html)
            
        case ('boton')
            write(unidad, '(a,a)') sangria, '<button class="boton-personalizado" id="'// &
                                  trim(elemento%id)//'">'//trim(elemento%contenido)//'</button>'
                                  
        case ('etiqueta')
            write(unidad, '(a,a)') sangria, '<label class="etiqueta-moderna" id="'// &
                                  trim(elemento%id)//'">'//trim(elemento%contenido)//'</label>'
    end select
end subroutine

function generar_entrada(elemento) result(html)
    type(elemento_web), intent(in) :: elemento
    character(200) :: html
    character(50) :: tipo_entrada
    
    select case (trim(elemento%clase))
        case ('texto')
            tipo_entrada = 'text'
        case ('clave')
            tipo_entrada = 'password'
        case ('numero')
            tipo_entrada = 'number'
        case ('fecha')
            tipo_entrada = 'date'
        case default
            tipo_entrada = 'text'
    end select
    
    write(html, '(a)') '<input type="'//trim(tipo_entrada)//'" '// &
                       'id="'//trim(elemento%id)//'" '// &
                       'class="entrada-moderna" '// &
                       'value="'//trim(elemento%contenido)//'"/>'
end function

subroutine aplicar_estilos(ruta)
    character(*), intent(in) :: ruta
    integer :: unidad_est
    
    open(newunit=unidad_est, file=trim(ruta)//'visual.css', status='replace')
    
    call escribir_estilos_base(unidad_est)
    call escribir_estilos_componentes(unidad_est)
    call escribir_estilos_responsivos(unidad_est)
    
    close(unidad_est)
end subroutine

subroutine escribir_estilos_base(unidad)
    integer, intent(in) :: unidad
    
    write(unidad, '(a)') '* {'
    write(unidad, '(a)') '    margin: 0;'
    write(unidad, '(a)') '    padding: 0;'
    write(unidad, '(a)') '    box-sizing: border-box;'
    write(unidad, '(a)') '}'
    write(unidad, '(a)') ''
    write(unidad, '(a)') 'body {'
    write(unidad, '(a)') '    font-family: Arial, sans-serif;'
    write(unidad, '(a)') '    line-height: 1.6;'
    write(unidad, '(a)') '    color: #333;'
    write(unidad, '(a)') '}'
end subroutine

subroutine escribir_estilos_componentes(unidad)
    integer, intent(in) :: unidad
    
    write(unidad, '(a)') '.contenedor-flex {'
    write(unidad, '(a)') '    display: flex;'
    write(unidad, '(a)') '    flex-direction: column;'
    write(unidad, '(a)') '    gap: 1rem;'
    write(unidad, '(a)') '    padding: 1rem;'
    write(unidad, '(a)') '}'
    
    write(unidad, '(a)') '.entrada-moderna {'
    write(unidad, '(a)') '    padding: 0.5rem;'
    write(unidad, '(a)') '    border: 1px solid #ddd;'
    write(unidad, '(a)') '    border-radius: 4px;'
    write(unidad, '(a)') '    transition: border-color 0.3s;'
    write(unidad, '(a)') '}'
    
    write(unidad, '(a)') '.boton-personalizado {'
    write(unidad, '(a)') '    padding: 0.5rem 1rem;'
    write(unidad, '(a)') '    background-color: #007bff;'
    write(unidad, '(a)') '    color: white;'
    write(unidad, '(a)') '    border: none;'
    write(unidad, '(a)') '    border-radius: 4px;'
    write(unidad, '(a)') '    cursor: pointer;'
    write(unidad, '(a)') '}'
end subroutine

subroutine escribir_estilos_responsivos(unidad)
    integer, intent(in) :: unidad
    
    write(unidad, '(a)') '@media (max-width: 768px) {'
    write(unidad, '(a)') '    .contenedor-flex {'
    write(unidad, '(a)') '        padding: 0.5rem;'
    write(unidad, '(a)') '    }'
    
    write(unidad, '(a)') '    .entrada-moderna {'
    write(unidad, '(a)') '        width: 100%;'
    write(unidad, '(a)') '    }'
    write(unidad, '(a)') '}'
end subroutine

function es_elemento_raiz(elemento) result(es_raiz)
    type(elemento_web), intent(in) :: elemento
    logical :: es_raiz
    es_raiz = elemento%nivel_anidacion == 0
end function

END MODULE creador_interfaz