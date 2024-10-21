MODULE lexico_errores
    implicit none

    type :: tokenInvalido
        CHARACTER(LEN = 150) :: secuencia_token
        CHARACTER(LEN = 200) :: descripcion_problema
        CHARACTER(LEN = 50) :: categoria_error
        INTEGER :: posicion_fila
        INTEGER :: posicion_columna
        REAL :: tiempo_deteccion
        CHARACTER(LEN = 100) :: contexto_linea
        LOGICAL :: esta_procesado = .false.
    end type tokenInvalido

    type :: EstadisticasAnalisis
        INTEGER :: tokens_analizados = 0
        INTEGER :: contador_tokens_invalidos = 0
        INTEGER :: lineas_procesadas = 0
        REAL :: tiempo_total_analisis = 0.0
        CHARACTER(LEN = 100) :: ultimo_token_valido = ""
    end type EstadisticasAnalisis

    type(tokenInvalido), ALLOCATABLE :: registro_tokens_invalidos(:)
    type(EstadisticasAnalisis) :: estadisticas
    LOGICAL :: esta_activo = .false.

contains 

    subroutine iniciarDetector()
        if (.NOT. esta_activo) then
            if (ALLOCATED(registro_tokens_invalidos)) DEALLOCATE(registro_tokens_invalidos)
            ALLOCATE(registro_tokens_invalidos(0))
            estadisticas%tokens_analizados = 0
            estadisticas%contador_tokens_invalidos = 0
            estadisticas%lineas_procesadas = 0
            estadisticas%tiempo_total_analisis = 0.0
            estadisticas%ultimo_token_valido = ""
            esta_activo = .true.
        end if
    end subroutine iniciarDetector

    subroutine registrarTokenInvalido(secuencia, categoria, descripcion, linea, columna, &
                                     tiempo, contexto)
        CHARACTER(LEN=*), INTENT(IN) :: secuencia, categoria, descripcion, contexto
        INTEGER, INTENT(IN) :: linea, columna
        REAL, INTENT(IN) :: tiempo
        type(tokenInvalido) :: nuevo_token_invalido
        type(tokenInvalido), ALLOCATABLE :: buffer_temporal(:)
        INTEGER :: contador_tokens
        
        if (.NOT. esta_activo) call iniciarDetector()

        nuevo_token_invalido%secuencia_token = secuencia
        nuevo_token_invalido%categoria_error = categoria
        nuevo_token_invalido%descripcion_problema = descripcion
        nuevo_token_invalido%posicion_fila = linea
        nuevo_token_invalido%posicion_columna = columna
        nuevo_token_invalido%tiempo_deteccion = tiempo
        nuevo_token_invalido%contexto_linea = contexto
        nuevo_token_invalido%esta_procesado = .true.

        estadisticas%contador_tokens_invalidos = estadisticas%contador_tokens_invalidos + 1
        estadisticas%tiempo_total_analisis = estadisticas%tiempo_total_analisis + tiempo
        
        if (.NOT. ALLOCATED(registro_tokens_invalidos)) then
            ALLOCATE(registro_tokens_invalidos(1))
            registro_tokens_invalidos(1) = nuevo_token_invalido
        else
            contador_tokens = size(registro_tokens_invalidos)
            ALLOCATE(buffer_temporal(contador_tokens + 1))
            buffer_temporal(1:contador_tokens) = registro_tokens_invalidos
            buffer_temporal(contador_tokens + 1) = nuevo_token_invalido
            
            DEALLOCATE(registro_tokens_invalidos)
            ALLOCATE(registro_tokens_invalidos(contador_tokens + 1))
            registro_tokens_invalidos = buffer_temporal
            DEALLOCATE(buffer_temporal)
        end if
    end subroutine registrarTokenInvalido

    subroutine imprimirReporteConsola()
        INTEGER :: i
        CHARACTER(LEN=25) :: str_linea, str_columna, str_tiempo
        
        if (.NOT. esta_activo .OR. .NOT. ALLOCATED(registro_tokens_invalidos)) then
            print *, "Sistema de Análisis Léxico: No se encontraron registros de análisis."
            return
        end if
        
        DO i = 1, size(registro_tokens_invalidos)
            write(str_linea, '(I0)') registro_tokens_invalidos(i)%posicion_fila
            write(str_columna, '(I0)') registro_tokens_invalidos(i)%posicion_columna
            write(str_tiempo, '(F8.3)') registro_tokens_invalidos(i)%tiempo_deteccion
        
        END DO
    end subroutine imprimirReporteConsola
    
    subroutine generarReporte()
        INTEGER :: i, unidad_salida
        CHARACTER(LEN=25) :: str_linea, str_columna, str_tiempo
        
        unidad_salida = 15
        open(unit=unidad_salida, file='reporte_analisis_lexico.md', status='replace')
        
        write(unidad_salida, '(A)') "# Reporte de Análisis Léxico"
        write(unidad_salida, '(A)') ""
        write(unidad_salida, '(A)') "## Resumen Estadístico"
        write(unidad_salida, '(A,I0)') "* Tokens Procesados: ", estadisticas%tokens_analizados
        write(unidad_salida, '(A,I0)') "* Tokens Inválidos: ", estadisticas%contador_tokens_invalidos
        write(unidad_salida, '(A,I0)') "* Líneas Analizadas: ", estadisticas%lineas_procesadas
        write(str_tiempo, '(F8.3)') estadisticas%tiempo_total_analisis
        write(unidad_salida, '(A,A,A)') "* Tiempo Total: ", trim(str_tiempo), " segundos"
        write(unidad_salida, '(A)') ""
        
        if (size(registro_tokens_invalidos) > 0) then
            write(unidad_salida, '(A)') "## Detalle de Tokens Inválidos"
            write(unidad_salida, '(A)') ""
            
            DO i = 1, size(registro_tokens_invalidos)
                write(str_linea, '(I0)') registro_tokens_invalidos(i)%posicion_fila
                write(str_columna, '(I0)') registro_tokens_invalidos(i)%posicion_columna
                
                write(unidad_salida, '(A,I0)') "### Token Inválido #", i
                write(unidad_salida, '(A)') ""
                write(unidad_salida, '(A,A)') "* **Categoría:** ", &
                    trim(registro_tokens_invalidos(i)%categoria_error)
                write(unidad_salida, '(A,A)') "* **Secuencia:** `", &
                    trim(registro_tokens_invalidos(i)%secuencia_token) // "`"
                write(unidad_salida, '(A,A)') "* **Problema:** ", &
                    trim(registro_tokens_invalidos(i)%descripcion_problema)
                write(unidad_salida, '(A,A,A,A)') "* **Ubicación:** Línea ", &
                    trim(str_linea), ", Columna ", trim(str_columna)
                write(unidad_salida, '(A,A)') "* **Contexto:** ", &
                    trim(registro_tokens_invalidos(i)%contexto_linea)
                write(unidad_salida, '(A)') ""
                write(unidad_salida, '(A)') "---"
            END DO
        else
            write(unidad_salida, '(A)') "## No se detectaron tokens inválidos"
        end if
        
        close(unidad_salida)
    end subroutine generarReporte

END MODULE lexico_errores