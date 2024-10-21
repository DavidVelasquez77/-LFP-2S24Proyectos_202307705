MODULE identificador 
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: Etiqueta, inicializar, agregarEtiqueta, moverEtiqueta
    PUBLIC :: redimensionar, cambiarTexto, cambiarColor, mostrarInfo
    PUBLIC :: eliminarEtiqueta, contarEtiquetas

    TYPE :: Etiqueta
        CHARACTER(100) :: id
        REAL :: x, y
        INTEGER :: width, height
        CHARACTER(200) :: content
        INTEGER :: r, g, b
    END TYPE Etiqueta

    TYPE(Etiqueta), ALLOCATABLE :: etiquetas(:)

CONTAINS

    SUBROUTINE inicializar(capacidad)
        INTEGER, INTENT(IN) :: capacidad
        IF (ALLOCATED(etiquetas)) DEALLOCATE(etiquetas)
        ALLOCATE(etiquetas(capacidad))
    END SUBROUTINE inicializar

    SUBROUTINE agregarEtiqueta(etiqueta)
        IMPLICIT NONE
        TYPE(Etiqueta), INTENT(IN) :: etiqueta
        TYPE(Etiqueta), ALLOCATABLE :: temp(:)
        INTEGER :: n

        IF (.NOT. ALLOCATED(etiquetas)) THEN
            ALLOCATE(etiquetas(1))
            etiquetas(1) = etiqueta
        ELSE
            n = SIZE(etiquetas)
            ALLOCATE(temp(n+1))
            temp(1:n) = etiquetas
            temp(n+1) = etiqueta
            CALL MOVE_ALLOC(temp, etiquetas)
        END IF
    END SUBROUTINE agregarEtiqueta

    SUBROUTINE moverEtiqueta(id, x, y)
        CHARACTER(*), INTENT(IN) :: id
        REAL, INTENT(IN) :: x, y
        INTEGER :: i

        DO i = 1, SIZE(etiquetas)
            IF (etiquetas(i)%id == id) THEN
                etiquetas(i)%x = x
                etiquetas(i)%y = y
                RETURN
            END IF
        END DO
        PRINT *, "Etiqueta no encontrada:", id
    END SUBROUTINE moverEtiqueta

    SUBROUTINE redimensionar(id, width, height)
        CHARACTER(*), INTENT(IN) :: id
        INTEGER, INTENT(IN) :: width, height
        INTEGER :: i

        DO i = 1, SIZE(etiquetas)
            IF (etiquetas(i)%id == id) THEN
                etiquetas(i)%width = width
                etiquetas(i)%height = height
                RETURN
            END IF
        END DO
        PRINT *, "Etiqueta no encontrada:", id
    END SUBROUTINE redimensionar

    SUBROUTINE cambiarTexto(id, content)
        CHARACTER(*), INTENT(IN) :: id, content
        INTEGER :: i

        DO i = 1, SIZE(etiquetas)
            IF (etiquetas(i)%id == id) THEN
                etiquetas(i)%content = content
                RETURN
            END IF
        END DO
        PRINT *, "Etiqueta no encontrada:", id
    END SUBROUTINE cambiarTexto

    SUBROUTINE cambiarColor(id, r, g, b)
        CHARACTER(*), INTENT(IN) :: id
        INTEGER, INTENT(IN) :: r, g, b
        INTEGER :: i

        DO i = 1, SIZE(etiquetas)
            IF (etiquetas(i)%id == id) THEN
                etiquetas(i)%r = r
                etiquetas(i)%g = g
                etiquetas(i)%b = b
                RETURN
            END IF
        END DO
        PRINT *, "Etiqueta no encontrada:", id
    END SUBROUTINE cambiarColor

    SUBROUTINE mostrarInfo(id)
        CHARACTER(*), INTENT(IN) :: id
        INTEGER :: i

        DO i = 1, SIZE(etiquetas)
            IF (etiquetas(i)%id == id) THEN
                PRINT *, "ID:", TRIM(etiquetas(i)%id)
                PRINT *, "Posición:", etiquetas(i)%x, etiquetas(i)%y
                PRINT *, "Tamaño:", etiquetas(i)%width, "x", etiquetas(i)%height
                PRINT *, "Contenido:", TRIM(etiquetas(i)%content)
                PRINT *, "Color (RGB):", etiquetas(i)%r, etiquetas(i)%g, etiquetas(i)%b
                RETURN
            END IF
        END DO
        PRINT *, "Etiqueta no encontrada:", id
    END SUBROUTINE mostrarInfo

    SUBROUTINE eliminarEtiqueta(id)
        CHARACTER(*), INTENT(IN) :: id
        INTEGER :: i, n
        TYPE(Etiqueta), ALLOCATABLE :: temp(:)

        n = SIZE(etiquetas)
        ALLOCATE(temp(n-1))
        DO i = 1, n
            IF (etiquetas(i)%id /= id) THEN
                temp(i) = etiquetas(i)
            END IF
        END DO
        CALL MOVE_ALLOC(temp, etiquetas)
    END SUBROUTINE eliminarEtiqueta

    FUNCTION contarEtiquetas() RESULT(count)
        INTEGER :: count
        count = SIZE(etiquetas)
    END FUNCTION contarEtiquetas

END MODULE identificador