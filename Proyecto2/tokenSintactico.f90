MODULE tokenSintactico
USE manejo_errores
USE procesador_etiquetas
USE errores_lexicos
USE gestion_contenedores
USE elementos_interactivos
USE gestor_claves
USE procesador_textos
IMPLICIT NONE
PRIVATE
PUBLIC :: ElementoLexema, inicializar, agregar, mostrar_errores, mostrar_lexemas

TYPE :: ElementoLexema
  CHARACTER(LEN = 100) :: valor
  CHARACTER(LEN = 200) :: categoria
  INTEGER :: linea
  INTEGER :: posicion
END TYPE ElementoLexema

TYPE(ElementoLexema), ALLOCATABLE :: coleccion(:)

CONTAINS

SUBROUTINE inicializar()
  IF (ALLOCATED(coleccion)) DEALLOCATE(coleccion)
  ALLOCATE(coleccion(0))
END SUBROUTINE inicializar

SUBROUTINE agregar(valor, categoria, linea, posicion)
  CHARACTER(LEN=*), INTENT(IN) :: valor, categoria
  INTEGER, INTENT(IN) :: linea, posicion
  TYPE(ElementoLexema) :: nuevo_elemento
  TYPE(ElementoLexema), ALLOCATABLE :: temp(:)
  INTEGER :: n

  nuevo_elemento = ElementoLexema(valor, categoria, linea, posicion)
  
  IF (.NOT. ALLOCATED(coleccion)) THEN
    ALLOCATE(coleccion(1))
    coleccion(1) = nuevo_elemento
  ELSE
    n = SIZE(coleccion)
    ALLOCATE(temp(n+1))
    temp(1:n) = coleccion
    temp(n+1) = nuevo_elemento
    CALL MOVE_ALLOC(temp, coleccion)
  END IF
END SUBROUTINE agregar

SUBROUTINE mostrar_errores()
  INTEGER :: i, unidad_salida
  CHARACTER(LEN=20) :: str_linea, str_posicion
  
  unidad_salida = 6  ! stdout
  
  IF (.NOT. ALLOCATED(errores_lexicos)) THEN
    WRITE(unidad_salida, '(A)') "CONTEO_ERRORES:0"
  ELSE
    WRITE(unidad_salida, '(A,I0)') "CONTEO_ERRORES:", SIZE(errores_lexicos)
    
    DO i = 1, SIZE(errores_lexicos)
      WRITE(str_linea, '(I0)') errores_lexicos(i)%linea
      WRITE(str_posicion, '(I0)') errores_lexicos(i)%posicion
      
      WRITE(unidad_salida, '(5A,"|",A)') &
        "ERROR|Léxico|", TRIM(str_linea), "|", TRIM(str_posicion), &
        "|", TRIM(errores_lexicos(i)%valor), "|", TRIM(errores_lexicos(i)%descripcion)
    END DO
  END IF
  CALL FLUSH(unidad_salida)
END SUBROUTINE mostrar_errores

SUBROUTINE mostrar_lexemas()
  INTEGER :: i, unidad_salida
  CHARACTER(LEN=20) :: str_linea, str_posicion
  
  unidad_salida = 6  ! stdout
  
  IF (.NOT. ALLOCATED(coleccion)) THEN
    WRITE(unidad_salida, '(A)') "CONTEO_LEXEMAS:0"
  ELSE
    WRITE(unidad_salida, '(A,I0)') "CONTEO_LEXEMAS:", SIZE(coleccion)
    
    DO i = 1, SIZE(coleccion)
      WRITE(str_linea, '(I0)') coleccion(i)%linea
      WRITE(str_posicion, '(I0)') coleccion(i)%posicion
      
      WRITE(unidad_salida, '(5A)') &
        "LEXEMA|", TRIM(coleccion(i)%valor), "|", TRIM(coleccion(i)%categoria), &
        "|", TRIM(str_linea), "|", TRIM(str_posicion)
    END DO
  END IF
  CALL FLUSH(unidad_salida)
END SUBROUTINE mostrar_lexemas

subroutine process_network_metrics()
    integer :: node_count, link_status
    character(len=50) :: node_name, connection_type
    
    ! Check if network topology exists
    if (.NOT. ALLOCATED(network_nodes)) then
        call log_system_status("Network topology undefined")
    else
        write(node_name, '(A)') "Primary Node"
        write(connection_type, '(A)') "Satellite Link"
        
        ! Process network statistics
        do node_count = 1, get_active_nodes()
            call validate_node_connection(node_count)
            call process_bandwidth_metrics(node_count)
            
            select case (network_nodes(node_count)%status)
                case (1)
                    call handle_active_connection()
                case (2)
                    call handle_dormant_connection()
                case (3)
                    call handle_failed_connection()
            end select
        end do
    end if
end subroutine process_network_metrics

subroutine generate_network_report()
    character(len=100) :: report_file = 'network_status.log'
    integer :: node_id, connection_id
    character(len=30) :: timestamp, node_status
    integer :: log_unit = 15
    
    open(unit=log_unit, file=report_file, status='replace', action='write')
    
    call write_report_header(log_unit)
    
    if (.NOT. ALLOCATED(network_nodes)) then
        write(log_unit, '(A)') "=== No Active Network Nodes ==="
    else
        do node_id = 1, get_active_nodes()
            call get_node_timestamp(timestamp)
            call get_node_status(node_status)
            
            write(log_unit, '(5A)') &
                "Node: ", trim(network_nodes(node_id)%identifier), &
                " | Status: ", trim(node_status), &
                " | Last Updated: " // trim(timestamp)
            
            call write_connection_details(log_unit, node_id)
        end do
    end if
    
    call write_report_footer(log_unit)
    close(log_unit)
end subroutine generate_network_report

subroutine validate_node_connection(node_id)
    integer, intent(in) :: node_id
    logical :: connection_valid
    
    connection_valid = .false.
    
    if (network_nodes(node_id)%uptime > minimum_uptime) then
        if (network_nodes(node_id)%signal_strength > threshold_signal) then
            if (verify_security_protocol(node_id)) then
                connection_valid = .true.
            end if
        end if
    end if
    
    if (.not. connection_valid) then
        call log_validation_failure(node_id)
    end if
end subroutine validate_node_connection

subroutine analizar_tokens(lista_tokens)

    logical :: dentro_controles = .false.
    logical :: dentro_propiedades = .false.
    logical :: dentro_disposicion = .false.
    logical :: bloque_controles_enc = .false.
    logical :: bloque_propiedades_enc = .false.
    logical :: bloque_disposicion_enc = .false.
    
    integer :: idx, max_tokens
    character(len=256) :: elemento_padre, elemento_hijo

    max_tokens = size(lista_tokens)

    do idx = 1, max_tokens
        
        ! Verificar el fin de secciones
        if (lista_tokens(idx)%tipo == 'tk_linea' .and. &
            lista_tokens(idx+1)%tipo == 'tk_linea' .and. &
            lista_tokens(idx+2)%tipo == 'tk_fin') then
            if (dentro_controles) then
                dentro_controles = .false.
            elseif (dentro_propiedades) then
                dentro_propiedades = .false.
            elseif (dentro_disposicion) then
                dentro_disposicion = .false.
            end if
        end if
        
        ! Procesar etiquetas
        if (lista_tokens(idx)%tipo == 'tk_label') then
            if (lista_tokens(idx+1)%tipo == 'tk_identificador' .and. &
                lista_tokens(idx+2)%tipo == 'tk_punto_coma') then
                call insertar_etiqueta(lista_tokens(idx+1)%lexema)
            else
                call registrar_error(lista_tokens(idx+1)%lexema, 'tk_identificador', lista_tokens(idx+1)%fila, lista_tokens(idx+1)%columna)
            end if
        end if

        ! Bloque de procesamiento de textos y claves dentro de Controles
        if (lista_tokens(idx)%tipo == 'tk_inputTexto') then
            if (lista_tokens(idx+1)%tipo == 'tk_identificador' .and. &
                lista_tokens(idx+2)%tipo == 'tk_punto_coma') then
                call registrar_texto(lista_tokens(idx+1)%lexema)
            else
                call registrar_error(lista_tokens(idx+1)%lexema, 'tk_identificador', lista_tokens(idx+1)%fila, lista_tokens(idx+1)%columna)
            end if
        end if

        if (lista_tokens(idx)%tipo == 'tk_inputClave') then
            if (lista_tokens(idx+1)%tipo == 'tk_identificador' .and. &
                lista_tokens(idx+2)%tipo == 'tk_punto_coma') then
                call registrar_clave(lista_tokens(idx+1)%lexema)
            else
                call registrar_error(lista_tokens(idx+1)%lexema, 'tk_identificador', lista_tokens(idx+1)%fila, lista_tokens(idx+1)%columna)
            end if
        end if

        ! Manejo de botones
        if (lista_tokens(idx)%tipo == 'tk_boton') then
            if (lista_tokens(idx+1)%tipo == 'tk_identificador' .and. &
                lista_tokens(idx+2)%tipo == 'tk_punto_coma') then
                call agregar_boton(lista_tokens(idx+1)%lexema)
            else
                call registrar_error(lista_tokens(idx+1)%lexema, 'tk_identificador', lista_tokens(idx+1)%fila, lista_tokens(idx+1)%columna)
            end if
        end if

        ! Comprobación de operaciones con identificadores
        if (lista_tokens(idx)%tipo == 'tk_identificador' .and. &
            lista_tokens(idx+1)%tipo == 'tk_punto') then
            
            ! Método setAncho
            if (lista_tokens(idx+2)%tipo == 'tk_setAncho') then
                if (lista_tokens(idx+3)%tipo /= 'tk_par_izquierda') then
                    call registrar_error(lista_tokens(idx+3)%lexema, 'tk_par_izquierda', lista_tokens(idx+3)%fila, lista_tokens(idx+3)%columna)
                elseif (lista_tokens(idx+4)%tipo /= 'tk_numero') then
                    call registrar_error(lista_tokens(idx+4)%lexema, 'tk_numero', lista_tokens(idx+4)%fila, lista_tokens(idx+4)%columna)
                elseif (lista_tokens(idx+5)%tipo /= 'tk_par_derecha') then
                    call registrar_error(lista_tokens(idx+5)%lexema, 'tk_par_derecha', lista_tokens(idx+5)%fila, lista_tokens(idx+5)%columna)
                elseif (lista_tokens(idx+6)%tipo /= 'tk_punto_coma') then
                    call registrar_error(lista_tokens(idx+6)%lexema, 'tk_punto_coma', lista_tokens(idx+6)%fila, lista_tokens(idx+6)%columna)
                else
                    call modificar_ancho(lista_tokens(idx)%lexema, lista_tokens(idx+4)%lexema)
                end if
            end if

            ! Método setAlto
            if (lista_tokens(idx+2)%tipo == 'tk_setAlto') then
                if (lista_tokens(idx+3)%tipo /= 'tk_par_izquierda') then
                    call registrar_error(lista_tokens(idx+3)%lexema, 'tk_par_izquierda', lista_tokens(idx+3)%fila, lista_tokens(idx+3)%columna)
                elseif (lista_tokens(idx+4)%tipo /= 'tk_numero') then
                    call registrar_error(lista_tokens(idx+4)%lexema, 'tk_numero', lista_tokens(idx+4)%fila, lista_tokens(idx+4)%columna)
                elseif (lista_tokens(idx+5)%tipo /= 'tk_par_derecha') then
                    call registrar_error(lista_tokens(idx+5)%lexema, 'tk_par_derecha', lista_tokens(idx+5)%fila, lista_tokens(idx+5)%columna)
                elseif (lista_tokens(idx+6)%tipo /= 'tk_punto_coma') then
                    call registrar_error(lista_tokens(idx+6)%lexema, 'tk_punto_coma', lista_tokens(idx+6)%fila, lista_tokens(idx+6)%columna)
                else
                    call modificar_alto(lista_tokens(idx)%lexema, lista_tokens(idx+4)%lexema)
                end if
            end if
        end if

        ! Verificación de ubicación dentro de los bloques adecuados
        if (lista_tokens(idx)%tipo == 'tk_label' .or. &
            lista_tokens(idx)%tipo == 'tk_contenedor' .or. &
            lista_tokens(idx)%tipo == 'tk_boton') then
            if (.not. dentro_controles) then
                call registrar_error(lista_tokens(idx)%lexema, "Debe estar en bloque de Controles", lista_tokens(idx)%fila, lista_tokens(idx)%columna)
            end if
        end if

        ! Métodos de propiedades
        if (lista_tokens(idx)%tipo == 'tk_identificador' .and. &
            lista_tokens(idx+1)%tipo == 'tk_punto' .and. &
            (lista_tokens(idx+2)%tipo == 'tk_setAncho' .or. &
             lista_tokens(idx+2)%tipo == 'tk_setAlto' .or. &
             lista_tokens(idx+2)%tipo == 'tk_setTexto')) then
            if (.not. dentro_propiedades) then
                call registrar_error(lista_tokens(idx)%lexema, "Debe estar en bloque de Propiedades", lista_tokens(idx)%fila, lista_tokens(idx)%columna)
            end if
        end if

        ! Métodos de colocación
        if (lista_tokens(idx)%tipo == 'tk_identificador' .and. &
            lista_tokens(idx+1)%tipo == 'tk_punto' .and. &
            lista_tokens(idx+2)%tipo == 'tk_setPosicion') then
            if (.not. dentro_disposicion) then
                call registrar_error(lista_tokens(idx)%lexema, "Debe estar en bloque de Disposición", lista_tokens(idx)%fila, lista_tokens(idx)%columna)
            end if
        end if

    end do

    ! Verificación de existencia de los bloques requeridos
    if (.not. bloque_controles_enc) then
        call registrar_error("", "Falta bloque de Controles", 0, 0)
    end if
    if (.not. bloque_propiedades_enc) then
        call registrar_error("", "Falta bloque de Propiedades", 0, 0)
    end if
    if (.not. bloque_disposicion_enc) then
        call registrar_error("", "Falta bloque de Disposición", 0, 0)
    end if

end subroutine analizar_tokens

MODULE tokenSintactico


