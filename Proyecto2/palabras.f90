MODULE palabras
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: ElementoTexto, inicializar, agregar, actualizar_contenedor, modificar_contenido, &
              modificar_posicion, buscar, mostrar_todos
  
    TYPE :: ElementoTexto
      CHARACTER(LEN=50) :: clave
      CHARACTER(LEN=20) :: tipo
      CHARACTER(LEN=200) :: datos
      CHARACTER(LEN=50) :: pos_x
      CHARACTER(LEN=50) :: pos_y
      CHARACTER(LEN=50) :: contenedor
    END TYPE ElementoTexto
  
    TYPE(ElementoTexto), ALLOCATABLE :: coleccion(:)
  
  CONTAINS
  
    SUBROUTINE inicializar()
      IF (ALLOCATED(coleccion)) DEALLOCATE(coleccion)
      ALLOCATE(coleccion(0))
    END SUBROUTINE inicializar
  
    SUBROUTINE agregar(nueva_clave)
      CHARACTER(LEN=*), INTENT(IN) :: nueva_clave
      TYPE(ElementoTexto) :: nuevo_elemento
      TYPE(ElementoTexto), ALLOCATABLE :: temp(:)
      INTEGER :: n
  
      nuevo_elemento = ElementoTexto(nueva_clave, 'Texto', '', '', '', '')
      n = SIZE(coleccion)
      ALLOCATE(temp(n+1))
      temp(1:n) = coleccion
      temp(n+1) = nuevo_elemento
      CALL MOVE_ALLOC(temp, coleccion)
    END SUBROUTINE agregar
  
    SUBROUTINE actualizar_contenedor(clave_hijo, clave_padre)
      CHARACTER(LEN=*), INTENT(IN) :: clave_hijo, clave_padre
      INTEGER :: i
      LOGICAL :: encontrado = .FALSE.
  
      DO i = 1, SIZE(coleccion)
        IF (TRIM(coleccion(i)%clave) == TRIM(clave_hijo)) THEN
          coleccion(i)%contenedor = TRIM(clave_padre)
          encontrado = .TRUE.
          PRINT *, "Contenedor actualizado para:", TRIM(clave_hijo), "->", TRIM(clave_padre)
          EXIT
        END IF
      END DO
  
      IF (.NOT. encontrado) PRINT *, "Advertencia: No se encontró el elemento:", TRIM(clave_hijo)
    END SUBROUTINE actualizar_contenedor
  
    SUBROUTINE modificar_contenido(clave, nuevo_contenido)
      CHARACTER(LEN=*), INTENT(IN) :: clave, nuevo_contenido
      INTEGER :: i
  
      DO i = 1, SIZE(coleccion)
        IF (TRIM(coleccion(i)%clave) == TRIM(clave)) THEN
          coleccion(i)%datos = nuevo_contenido
          EXIT
        END IF
      END DO
    END SUBROUTINE modificar_contenido
  
    SUBROUTINE modificar_posicion(clave, nueva_x, nueva_y)
      CHARACTER(LEN=*), INTENT(IN) :: clave, nueva_x, nueva_y
      INTEGER :: i
  
      DO i = 1, SIZE(coleccion)
        IF (TRIM(coleccion(i)%clave) == TRIM(clave)) THEN
          coleccion(i)%pos_x = nueva_x
          coleccion(i)%pos_y = nueva_y
          EXIT
        END IF
      END DO
    END SUBROUTINE modificar_posicion
  
    FUNCTION buscar(clave) RESULT(existe)
      CHARACTER(LEN=*), INTENT(IN) :: clave
      LOGICAL :: existe
      INTEGER :: i
  
      existe = .FALSE.
      DO i = 1, SIZE(coleccion)
        IF (TRIM(coleccion(i)%clave) == TRIM(clave)) THEN
          existe = .TRUE.
          EXIT
        END IF
      END DO
    END FUNCTION buscar
  
    SUBROUTINE mostrar_todos()
      INTEGER :: i
  
      IF (SIZE(coleccion) == 0) THEN
        PRINT *, "No hay elementos"
      ELSE
        PRINT *, "Elementos encontrados:", SIZE(coleccion)
        DO i = 1, SIZE(coleccion)
          PRINT *, 'Clave:', TRIM(coleccion(i)%clave)
          PRINT *, 'Datos:', TRIM(coleccion(i)%datos)
          PRINT *, 'Posición X:', TRIM(coleccion(i)%pos_x)
          PRINT *, 'Posición Y:', TRIM(coleccion(i)%pos_y)
          PRINT *, 'Contenedor:', TRIM(coleccion(i)%contenedor)
          PRINT *, '------------------------------'
        END DO
      END IF
    END SUBROUTINE mostrar_todos
  
  END MODULE palabras