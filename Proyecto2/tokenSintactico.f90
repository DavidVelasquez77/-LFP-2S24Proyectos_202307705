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
    PUBLIC :: ElementoLexema, inicializar, agregar, mostrar_errores, mostrar_lexemas, analizar_tokens
  
    ! Definición de un tipo para almacenar información sobre los lexemas
    TYPE :: ElementoLexema
      CHARACTER(LEN = 100) :: valor
      CHARACTER(LEN = 200) :: categoria
      INTEGER :: linea
      INTEGER :: posicion
    END TYPE ElementoLexema
  
    ! Colección dinámica de lexemas
    TYPE(ElementoLexema), ALLOCATABLE :: coleccion(:)
  
  CONTAINS
  
    ! Subrutina para inicializar la colección de lexemas, vaciándola si ya estaba previamente asignada
    SUBROUTINE inicializar()
      IF (ALLOCATED(coleccion)) DEALLOCATE(coleccion)
      ALLOCATE(coleccion(0))
    END SUBROUTINE inicializar
  
    ! Subrutina para agregar un nuevo lexema a la colección
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
  
    ! Subrutina para mostrar los errores léxicos encontrados
    SUBROUTINE mostrar_errores()
      INTEGER :: i, unidad_salida
      CHARACTER(LEN=20) :: str_linea, str_posicion
      
      unidad_salida = 6  ! stdout (pantalla)
      
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
  
    ! Subrutina para mostrar la lista de lexemas capturados
    SUBROUTINE mostrar_lexemas()
      INTEGER :: i, unidad_salida
      CHARACTER(LEN=20) :: str_linea, str_posicion
      
      unidad_salida = 6  ! stdout (pantalla)
      
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
  
    ! Subrutina para analizar y procesar los tokens sintácticos
    SUBROUTINE analizar_tokens(lista_tokens)
      logical :: dentro_controles = .false.
      logical :: dentro_propiedades = .false.
      logical :: dentro_disposicion = .false.
      logical :: bloque_controles_enc = .false.
      logical :: bloque_propiedades_enc = .false.
      logical :: bloque_disposicion_enc = .false.
      
      integer :: idx, max_tokens
      character(len=256) :: elemento_padre, elemento_hijo
  
      ! Definir el tamaño máximo de la lista de tokens
      max_tokens = size(lista_tokens)
  
      ! Bucle para recorrer cada token y verificar su tipo y sintaxis
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
  
          ! Procesar campos de texto (inputTexto)
          if (lista_tokens(idx)%tipo == 'tk_inputTexto') then
              if (lista_tokens(idx+1)%tipo == 'tk_identificador' .and. &
                  lista_tokens(idx+2)%tipo == 'tk_punto_coma') then
                  call registrar_texto(lista_tokens(idx+1)%lexema)
              else
                  call registrar_error(lista_tokens(idx+1)%lexema, 'tk_identificador', lista_tokens(idx+1)%fila, lista_tokens(idx+1)%columna)
              end if
          end if
  
          ! Procesar claves (inputClave)
          if (lista_tokens(idx)%tipo == 'tk_inputClave') then
              if (lista_tokens(idx+1)%tipo == 'tk_identificador' .and. &
                  lista_tokens(idx+2)%tipo == 'tk_punto_coma') then
                  call registrar_clave(lista_tokens(idx+1)%lexema)
              else
                  call registrar_error(lista_tokens(idx+1)%lexema, 'tk_identificador', lista_tokens(idx+1)%fila, lista_tokens(idx+1)%columna)
              end if
          end if
  
          ! Procesar botones (boton)
          if (lista_tokens(idx)%tipo == 'tk_boton') then
              if (lista_tokens(idx+1)%tipo == 'tk_identificador' .and. &
                  lista_tokens(idx+2)%tipo == 'tk_punto_coma') then
                  call agregar_boton(lista_tokens(idx+1)%lexema)
              else
                  call registrar_error(lista_tokens(idx+1)%lexema, 'tk_identificador', lista_tokens(idx+1)%fila, lista_tokens(idx+1)%columna)
              end if
          end if
  
          ! Procesar configuraciones de interfaz, como setAncho y setAlto
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

                        ! Método setColor
          if (lista_tokens(idx+2)%tipo == 'tk_setColor') then
            if (lista_tokens(idx+3)%tipo /= 'tk_par_izquierda') then
                call registrar_error(lista_tokens(idx+3)%lexema, 'tk_par_izquierda', lista_tokens(idx+3)%fila, lista_tokens(idx+3)%columna)
            elseif (lista_tokens(idx+4)%tipo /= 'tk_cadena') then
                call registrar_error(lista_tokens(idx+4)%lexema, 'tk_cadena', lista_tokens(idx+4)%fila, lista_tokens(idx+4)%columna)
            elseif (lista_tokens(idx+5)%tipo /= 'tk_par_derecha') then
                call registrar_error(lista_tokens(idx+5)%lexema, 'tk_par_derecha', lista_tokens(idx+5)%fila, lista_tokens(idx+5)%columna)
            elseif (lista_tokens(idx+6)%tipo /= 'tk_punto_coma') then
                call registrar_error(lista_tokens(idx+6)%lexema, 'tk_punto_coma', lista_tokens(idx+6)%fila, lista_tokens(idx+6)%columna)
            else
                call modificar_color(lista_tokens(idx)%lexema, lista_tokens(idx+4)%lexema)
            end if
        end if
        
        ! Método setTexto
        if (lista_tokens(idx+2)%tipo == 'tk_setTexto') then
            if (lista_tokens(idx+3)%tipo /= 'tk_par_izquierda') then
                call registrar_error(lista_tokens(idx+3)%lexema, 'tk_par_izquierda', lista_tokens(idx+3)%fila, lista_tokens(idx+3)%columna)
            elseif (lista_tokens(idx+4)%tipo /= 'tk_cadena') then
                call registrar_error(lista_tokens(idx+4)%lexema, 'tk_cadena', lista_tokens(idx+4)%fila, lista_tokens(idx+4)%columna)
            elseif (lista_tokens(idx+5)%tipo /= 'tk_par_derecha') then
                call registrar_error(lista_tokens(idx+5)%lexema, 'tk_par_derecha', lista_tokens(idx+5)%fila, lista_tokens(idx+5)%columna)
            elseif (lista_tokens(idx+6)%tipo /= 'tk_punto_coma') then
                call registrar_error(lista_tokens(idx+6)%lexema, 'tk_punto_coma', lista_tokens(idx+6)%fila, lista_tokens(idx+6)%columna)
            else
                call modificar_texto(lista_tokens(idx)%lexema, lista_tokens(idx+4)%lexema)
            end if
        end if

        ! Método setFondo
        if (lista_tokens(idx+2)%tipo == 'tk_setFondo') then
            if (lista_tokens(idx+3)%tipo /= 'tk_par_izquierda') then
                call registrar_error(lista_tokens(idx+3)%lexema, 'tk_par_izquierda', lista_tokens(idx+3)%fila, lista_tokens(idx+3)%columna)
            elseif (lista_tokens(idx+4)%tipo /= 'tk_cadena') then
                call registrar_error(lista_tokens(idx+4)%lexema, 'tk_cadena', lista_tokens(idx+4)%fila, lista_tokens(idx+4)%columna)
            elseif (lista_tokens(idx+5)%tipo /= 'tk_par_derecha') then
                call registrar_error(lista_tokens(idx+5)%lexema, 'tk_par_derecha', lista_tokens(idx+5)%fila, lista_tokens(idx+5)%columna)
            elseif (lista_tokens(idx+6)%tipo /= 'tk_punto_coma') then
                call registrar_error(lista_tokens(idx+6)%lexema, 'tk_punto_coma', lista_tokens(idx+6)%fila, lista_tokens(idx+6)%columna)
            else
                call modificar_fondo(lista_tokens(idx)%lexema, lista_tokens(idx+4)%lexema)
            end if
        end if

        ! Método setBorde
        if (lista_tokens(idx+2)%tipo == 'tk_setBorde') then
            if (lista_tokens(idx+3)%tipo /= 'tk_par_izquierda') then
                call registrar_error(lista_tokens(idx+3)%lexema, 'tk_par_izquierda', lista_tokens(idx+3)%fila, lista_tokens(idx+3)%columna)
            elseif (lista_tokens(idx+4)%tipo /= 'tk_numero') then
                call registrar_error(lista_tokens(idx+4)%lexema, 'tk_numero', lista_tokens(idx+4)%fila, lista_tokens(idx+4)%columna)
            elseif (lista_tokens(idx+5)%tipo /= 'tk_par_derecha') then
                call registrar_error(lista_tokens(idx+5)%lexema, 'tk_par_derecha', lista_tokens(idx+5)%fila, lista_tokens(idx+5)%columna)
            elseif (lista_tokens(idx+6)%tipo /= 'tk_punto_coma') then
                call registrar_error(lista_tokens(idx+6)%lexema, 'tk_punto_coma', lista_tokens(idx+6)%fila, lista_tokens(idx+6)%columna)
            else
                call modificar_borde(lista_tokens(idx)%lexema, lista_tokens(idx+4)%lexema)
            end if
        end if

        ! Manejo de Contenedores (bloques de interfaz gráfica)
        if (lista_tokens(idx)%tipo == 'tk_contenedor') then
            if (lista_tokens(idx+1)%tipo /= 'tk_identificador') then
                call registrar_error(lista_tokens(idx+1)%lexema, 'tk_identificador', lista_tokens(idx+1)%fila, lista_tokens(idx+1)%columna)
            elseif (lista_tokens(idx+2)%tipo /= 'tk_llave_izquierda') then
                call registrar_error(lista_tokens(idx+2)%lexema, 'tk_llave_izquierda', lista_tokens(idx+2)%fila, lista_tokens(idx+2)%columna)
            else
                ! Inicio de un nuevo bloque contenedor
                dentro_controles = .true.
                bloque_controles_enc = .true.
                elemento_padre = lista_tokens(idx+1)%lexema
            end if
        end if

        ! Cierre del contenedor
        if (bloque_controles_enc .and. lista_tokens(idx)%tipo == 'tk_llave_derecha') then
            dentro_controles = .false.
            bloque_controles_enc = .false.
        end if

        ! Manejo de Propiedades del Contenedor
        if (lista_tokens(idx)%tipo == 'tk_propiedades') then
            if (lista_tokens(idx+1)%tipo /= 'tk_identificador') then
                call registrar_error(lista_tokens(idx+1)%lexema, 'tk_identificador', lista_tokens(idx+1)%fila, lista_tokens(idx+1)%columna)
            elseif (lista_tokens(idx+2)%tipo /= 'tk_llave_izquierda') then
                call registrar_error(lista_tokens(idx+2)%lexema, 'tk_llave_izquierda', lista_tokens(idx+2)%fila, lista_tokens(idx+2)%columna)
            else
                dentro_propiedades = .true.
                bloque_propiedades_enc = .true.
                elemento_hijo = lista_tokens(idx+1)%lexema
            end if
        end if

        ! Cierre del bloque de propiedades
        if (bloque_propiedades_enc .and. lista_tokens(idx)%tipo == 'tk_llave_derecha') then
            dentro_propiedades = .false.
            bloque_propiedades_enc = .false.
        end if

        ! Manejo de Disposición (layout)
        if (lista_tokens(idx)%tipo == 'tk_disposicion') then
            if (lista_tokens(idx+1)%tipo /= 'tk_identificador') then
                call registrar_error(lista_tokens(idx+1)%lexema, 'tk_identificador', lista_tokens(idx+1)%fila, lista_tokens(idx+1)%columna)
            elseif (lista_tokens(idx+2)%tipo /= 'tk_llave_izquierda') then
                call registrar_error(lista_tokens(idx+2)%lexema, 'tk_llave_izquierda', lista_tokens(idx+2)%fila, lista_tokens(idx+2)%columna)
            else
                dentro_disposicion = .true.
                bloque_disposicion_enc = .true.
                elemento_hijo = lista_tokens(idx+1)%lexema
            end if
        end if

        ! Cierre del bloque de disposición
        if (bloque_disposicion_enc .and. lista_tokens(idx)%tipo == 'tk_llave_derecha') then
            dentro_disposicion = .false.
            bloque_disposicion_enc = .false.
        end if
        
        ! Procesar otros tipos de elementos
        if (lista_tokens(idx)%tipo == 'tk_boton') then
            if (lista_tokens(idx+1)%tipo == 'tk_identificador' .and. &
                lista_tokens(idx+2)%tipo == 'tk_punto_coma') then
                call agregar_boton(lista_tokens(idx+1)%lexema)
            else
                call registrar_error(lista_tokens(idx+1)%lexema, 'tk_identificador', lista_tokens(idx+1)%fila, lista_tokens(idx+1)%columna)
            end if
        end if

        if (lista_tokens(idx)%tipo == 'tk_etiqueta') then
            if (lista_tokens(idx+1)%tipo == 'tk_identificador' .and. &
                lista_tokens(idx+2)%tipo == 'tk_punto_coma') then
                call agregar_etiqueta(lista_tokens(idx+1)%lexema)
            else
                call registrar_error(lista_tokens(idx+1)%lexema, 'tk_identificador', lista_tokens(idx+1)%fila, lista_tokens(idx+1)%columna)
            end if
        end if

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

        ! Método setPosicion
        if (lista_tokens(idx+2)%tipo == 'tk_setPosicion') then
            if (lista_tokens(idx+3)%tipo /= 'tk_par_izquierda') then
                call registrar_error(lista_tokens(idx+3)%lexema, 'tk_par_izquierda', lista_tokens(idx+3)%fila, lista_tokens(idx+3)%columna)
            elseif (lista_tokens(idx+4)%tipo /= 'tk_numero') then
                call registrar_error(lista_tokens(idx+4)%lexema, 'tk_numero', lista_tokens(idx+4)%fila, lista_tokens(idx+4)%columna)
            elseif (lista_tokens(idx+5)%tipo /= 'tk_coma') then
                call registrar_error(lista_tokens(idx+5)%lexema, 'tk_coma', lista_tokens(idx+5)%fila, lista_tokens(idx+5)%columna)
            elseif (lista_tokens(idx+6)%tipo /= 'tk_numero') then
                call registrar_error(lista_tokens(idx+6)%lexema, 'tk_numero', lista_tokens(idx+6)%fila, lista_tokens(idx+6)%columna)
            elseif (lista_tokens(idx+7)%tipo /= 'tk_par_derecha') then
                call registrar_error(lista_tokens(idx+7)%lexema, 'tk_par_derecha', lista_tokens(idx+7)%fila, lista_tokens(idx+7)%columna)
            elseif (lista_tokens(idx+8)%tipo /= 'tk_punto_coma') then
                call registrar_error(lista_tokens(idx+8)%lexema, 'tk_punto_coma', lista_tokens(idx+8)%fila, lista_tokens(idx+8)%columna)
            else
                call modificar_posicion(lista_tokens(idx)%lexema, lista_tokens(idx+4)%lexema, lista_tokens(idx+6)%lexema)
            end if
        end if

        ! Método setVisible
        if (lista_tokens(idx+2)%tipo == 'tk_setVisible') then
            if (lista_tokens(idx+3)%tipo /= 'tk_par_izquierda') then
                call registrar_error(lista_tokens(idx+3)%lexema, 'tk_par_izquierda', lista_tokens(idx+3)%fila, lista_tokens(idx+3)%columna)
            elseif (lista_tokens(idx+4)%tipo /= 'tk_booleano') then
                call registrar_error(lista_tokens(idx+4)%lexema, 'tk_booleano', lista_tokens(idx+4)%fila, lista_tokens(idx+4)%columna)
            elseif (lista_tokens(idx+5)%tipo /= 'tk_par_derecha') then
                call registrar_error(lista_tokens(idx+5)%lexema, 'tk_par_derecha', lista_tokens(idx+5)%fila, lista_tokens(idx+5)%columna)
            elseif (lista_tokens(idx+6)%tipo /= 'tk_punto_coma') then
                call registrar_error(lista_tokens(idx+6)%lexema, 'tk_punto_coma', lista_tokens(idx+6)%fila, lista_tokens(idx+6)%columna)
            else
                call modificar_visible(lista_tokens(idx)%lexema, lista_tokens(idx+4)%lexema)
            end if
        end if

        ! Método setHabilitado
        if (lista_tokens(idx+2)%tipo == 'tk_setHabilitado') then
            if (lista_tokens(idx+3)%tipo /= 'tk_par_izquierda') then
                call registrar_error(lista_tokens(idx+3)%lexema, 'tk_par_izquierda', lista_tokens(idx+3)%fila, lista_tokens(idx+3)%columna)
            elseif (lista_tokens(idx+4)%tipo /= 'tk_booleano') then
                call registrar_error(lista_tokens(idx+4)%lexema, 'tk_booleano', lista_tokens(idx+4)%fila, lista_tokens(idx+4)%columna)
            elseif (lista_tokens(idx+5)%tipo /= 'tk_par_derecha') then
                call registrar_error(lista_tokens(idx+5)%lexema, 'tk_par_derecha', lista_tokens(idx+5)%fila, lista_tokens(idx+5)%columna)
            elseif (lista_tokens(idx+6)%tipo /= 'tk_punto_coma') then
                call registrar_error(lista_tokens(idx+6)%lexema, 'tk_punto_coma', lista_tokens(idx+6)%fila, lista_tokens(idx+6)%columna)
            else
                call modificar_habilitado(lista_tokens(idx)%lexema, lista_tokens(idx+4)%lexema)
            end if
        end if

        ! Método setFuente
        if (lista_tokens(idx+2)%tipo == 'tk_setFuente') then
            if (lista_tokens(idx+3)%tipo /= 'tk_par_izquierda') then
                call registrar_error(lista_tokens(idx+3)%lexema, 'tk_par_izquierda', lista_tokens(idx+3)%fila, lista_tokens(idx+3)%columna)
            elseif (lista_tokens(idx+4)%tipo /= 'tk_cadena') then
                call registrar_error(lista_tokens(idx+4)%lexema, 'tk_cadena', lista_tokens(idx+4)%fila, lista_tokens(idx+4)%columna)
            elseif (lista_tokens(idx+5)%tipo /= 'tk_coma') then
                call registrar_error(lista_tokens(idx+5)%lexema, 'tk_coma', lista_tokens(idx+5)%fila, lista_tokens(idx+5)%columna)
            elseif (lista_tokens(idx+6)%tipo /= 'tk_numero') then
                call registrar_error(lista_tokens(idx+6)%lexema, 'tk_numero', lista_tokens(idx+6)%fila, lista_tokens(idx+6)%columna)
            elseif (lista_tokens(idx+7)%tipo /= 'tk_par_derecha') then
                call registrar_error(lista_tokens(idx+7)%lexema, 'tk_par_derecha', lista_tokens(idx+7)%fila, lista_tokens(idx+7)%columna)
            elseif (lista_tokens(idx+8)%tipo /= 'tk_punto_coma') then
                call registrar_error(lista_tokens(idx+8)%lexema, 'tk_punto_coma', lista_tokens(idx+8)%fila, lista_tokens(idx+8)%columna)
            else
                call modificar_fuente(lista_tokens(idx)%lexema, lista_tokens(idx+4)%lexema, lista_tokens(idx+6)%lexema)
            end if
        end if

        ! Método setAlineacion
        if (lista_tokens(idx+2)%tipo == 'tk_setAlineacion') then
            if (lista_tokens(idx+3)%tipo /= 'tk_par_izquierda') then
                call registrar_error(lista_tokens(idx+3)%lexema, 'tk_par_izquierda', lista_tokens(idx+3)%fila, lista_tokens(idx+3)%columna)
            elseif (lista_tokens(idx+4)%tipo /= 'tk_identificador') then
                call registrar_error(lista_tokens(idx+4)%lexema, 'tk_identificador', lista_tokens(idx+4)%fila, lista_tokens(idx+4)%columna)
            elseif (lista_tokens(idx+5)%tipo /= 'tk_par_derecha') then
                call registrar_error(lista_tokens(idx+5)%lexema, 'tk_par_derecha', lista_tokens(idx+5)%fila, lista_tokens(idx+5)%columna)
            elseif (lista_tokens(idx+6)%tipo /= 'tk_punto_coma') then
                call registrar_error(lista_tokens(idx+6)%lexema, 'tk_punto_coma', lista_tokens(idx+6)%fila, lista_tokens(idx+6)%columna)
            else
                call modificar_alineacion(lista_tokens(idx)%lexema, lista_tokens(idx+4)%lexema)
            end if
        end if
      end do
    END SUBROUTINE analizar_tokens
  END MODULE tokenSintactico
  