MODULE errores
    implicit none

    type :: InformeDiagnostico
        CHARACTER(LEN = 150) :: fragmento_encontrado
        CHARACTER(LEN = 150) :: fragmento_esperado
        INTEGER :: posicion_fila
        INTEGER :: posicion_columna
        CHARACTER(LEN = 50) :: severidad
        CHARACTER(LEN = 250) :: mensaje_contextual
        LOGICAL :: requiere_atencion_inmediata = .false.
    end type InformeDiagnostico

    type(InformeDiagnostico), ALLOCATABLE :: registro_diagnosticos(:)
    INTEGER :: contador_diagnosticos = 0
    LOGICAL :: sistema_inicializado = .false.

contains 

    subroutine inicializar_sistema()
        if (.NOT. sistema_inicializado) then
            ALLOCATE(registro_diagnosticos(0))
            sistema_inicializado = .true.
        end if
    end subroutine inicializar_sistema

    subroutine finalizar_sistema()
        if (sistema_inicializado) then
            if (ALLOCATED(registro_diagnosticos)) DEALLOCATE(registro_diagnosticos)
            sistema_inicializado = .false.
            contador_diagnosticos = 0
        end if
    end subroutine finalizar_sistema

    subroutine agregar_diagnostico(fragmento_actual, fragmento_esperado, fila, columna, &
                                 severidad, mensaje_adicional, urgente)
        CHARACTER(LEN=*), INTENT(IN) :: fragmento_actual
        CHARACTER(LEN=*), INTENT(IN) :: fragmento_esperado
        INTEGER, INTENT(IN) :: fila, columna
        CHARACTER(LEN=*), INTENT(IN) :: severidad
        CHARACTER(LEN=*), INTENT(IN) :: mensaje_adicional
        LOGICAL, INTENT(IN) :: urgente
        type(InformeDiagnostico) :: nuevo_diagnostico
        type(InformeDiagnostico), ALLOCATABLE :: buffer_temporal(:)
        
        nuevo_diagnostico%fragmento_encontrado = fragmento_actual
        nuevo_diagnostico%fragmento_esperado = fragmento_esperado
        nuevo_diagnostico%posicion_fila = fila
        nuevo_diagnostico%posicion_columna = columna
        nuevo_diagnostico%severidad = severidad
        nuevo_diagnostico%mensaje_contextual = mensaje_adicional
        nuevo_diagnostico%requiere_atencion_inmediata = urgente

        if (.NOT. ALLOCATED(registro_diagnosticos)) then
            call inicializar_sistema()
        end if

        contador_diagnosticos = contador_diagnosticos + 1
        
        ALLOCATE(buffer_temporal(contador_diagnosticos))
        if (contador_diagnosticos > 1) then
            buffer_temporal(1:contador_diagnosticos-1) = registro_diagnosticos
        end if
        buffer_temporal(contador_diagnosticos) = nuevo_diagnostico
        
        if (ALLOCATED(registro_diagnosticos)) DEALLOCATE(registro_diagnosticos)
        ALLOCATE(registro_diagnosticos(contador_diagnosticos))
        registro_diagnosticos = buffer_temporal
        DEALLOCATE(buffer_temporal)

    end subroutine agregar_diagnostico

    subroutine generar_reporte_consola()
        integer :: i
        character(len=20) :: str_fila, str_columna
        
        if (.NOT. ALLOCATED(registro_diagnosticos) .OR. contador_diagnosticos == 0) then
            print *, "Sistema de Diagnóstico: No se detectaron problemas en la compilación."
            return
        end if

        print *, "=== Reporte de Diagnóstico de Compilación ==="
        print *, "Total de incidencias encontradas:", contador_diagnosticos
        print *, "----------------------------------------"
        
        DO i = 1, contador_diagnosticos
            write(str_fila, '(I0)') registro_diagnosticos(i)%posicion_fila
            write(str_columna, '(I0)') registro_diagnosticos(i)%posicion_columna
            
            print *, 'Diagnóstico #', i
            print *, 'Nivel de Severidad:', trim(registro_diagnosticos(i)%severidad)
            print *, 'Fragmento Detectado:', trim(registro_diagnosticos(i)%fragmento_encontrado)
            print *, 'Fragmento Esperado:', trim(registro_diagnosticos(i)%fragmento_esperado)
            print *, 'Ubicación: Fila', trim(str_fila), ', Columna', trim(str_columna)
            print *, 'Contexto:', trim(registro_diagnosticos(i)%mensaje_contextual)
            if (registro_diagnosticos(i)%requiere_atencion_inmediata) then
                print *, '¡ATENCIÓN! Este problema requiere revisión inmediata'
            end if
            print *, '----------------------------------------'
        END DO
    end subroutine generar_reporte_consola
    
    subroutine exportar_diagnostico_json()
        integer :: i, unidad_salida
        character(len=20) :: str_fila, str_columna
        
        unidad_salida = 6  ! Salida estándar
        
        if (.NOT. ALLOCATED(registro_diagnosticos)) then
            write(unidad_salida, '(A)') '{"total_diagnosticos": 0, "diagnosticos": []}'
            call flush(unidad_salida)
            return
        end if
        
        write(unidad_salida, '(A,I0,A)') '{"total_diagnosticos":', contador_diagnosticos, &
                                        ', "diagnosticos": ['
        
        DO i = 1, contador_diagnosticos
            write(str_fila, '(I0)') registro_diagnosticos(i)%posicion_fila
            write(str_columna, '(I0)') registro_diagnosticos(i)%posicion_columna
            
            write(unidad_salida, '(A)') '  {'
            write(unidad_salida, '(A,A,A)') '    "severidad": "', &
                                           trim(registro_diagnosticos(i)%severidad), '",'
            write(unidad_salida, '(A,A,A)') '    "fragmento_detectado": "', &
                                           trim(registro_diagnosticos(i)%fragmento_encontrado), '",'
            write(unidad_salida, '(A,A,A)') '    "fragmento_esperado": "', &
                                           trim(registro_diagnosticos(i)%fragmento_esperado), '",'
            write(unidad_salida, '(A,A,A)') '    "fila": "', trim(str_fila), '",'
            write(unidad_salida, '(A,A,A)') '    "columna": "', trim(str_columna), '",'
            write(unidad_salida, '(A,A,A)') '    "mensaje": "', &
                                           trim(registro_diagnosticos(i)%mensaje_contextual), '",'
            write(unidad_salida, '(A,L1,A)') '    "urgente": ', &
                                            registro_diagnosticos(i)%requiere_atencion_inmediata, &
                                            if(i == contador_diagnosticos, '}', '},')
            
            call flush(unidad_salida)
        END DO
        
        write(unidad_salida, '(A)') ']}'
        call flush(unidad_salida)
    end subroutine exportar_diagnostico_json

END MODULE errores