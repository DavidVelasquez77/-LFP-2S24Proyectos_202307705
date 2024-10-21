MODULE Button
    USE, INTRINSIC :: ISO_FORTRAN_ENV
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: InterfazVisual, crear_interfaz, agregar_elemento
    PUBLIC :: modificar_elemento, obtener_elemento, eliminar_elemento
    PUBLIC :: mostrar_elementos, existe_elemento

    ! Constantes del módulo
    INTEGER, PARAMETER :: MAX_STR_LEN = 300
    INTEGER, PARAMETER :: DEFAULT_ARRAY_SIZE = 10

    ! Tipo para representar las propiedades visuales
    TYPE :: PropiedadesVisuales
        INTEGER :: x = 0
        INTEGER :: y = 0
        CHARACTER(MAX_STR_LEN) :: color = ''
        CHARACTER(MAX_STR_LEN) :: fuente = ''
        INTEGER :: tamano = 12
    END TYPE PropiedadesVisuales

    ! Tipo principal para elementos visuales
    TYPE :: ElementoVisual
        CHARACTER(MAX_STR_LEN) :: id = ''
        CHARACTER(MAX_STR_LEN) :: tipo = ''
        CHARACTER(MAX_STR_LEN) :: texto = ''
        CHARACTER(MAX_STR_LEN) :: grupo = ''
        LOGICAL :: visible = .TRUE.
        TYPE(PropiedadesVisuales) :: estilo
        CONTAINS
            PROCEDURE :: inicializar => inicializar_elemento
            PROCEDURE :: actualizar => actualizar_elemento
    END TYPE ElementoVisual

    ! Tipo contenedor principal
    TYPE :: InterfazVisual
        PRIVATE
        TYPE(ElementoVisual), ALLOCATABLE :: elementos(:)
        INTEGER :: num_elementos = 0
        INTEGER :: capacidad = 0
        CONTAINS
            PROCEDURE :: inicializar => inicializar_interfaz
            PROCEDURE :: expandir => expandir_capacidad
            PROCEDURE :: agregar => agregar_elemento_interno
            PROCEDURE :: buscar => buscar_elemento
            PROCEDURE :: eliminar => eliminar_elemento_interno
    END TYPE InterfazVisual

CONTAINS

    ! ======= Métodos del ElementoVisual =======
    SUBROUTINE inicializar_elemento(this, id, tipo)
        CLASS(ElementoVisual), INTENT(INOUT) :: this
        CHARACTER(*), INTENT(IN) :: id, tipo
        
        this%id = id
        this%tipo = tipo
        this%visible = .TRUE.
        this%texto = ''
        this%grupo = ''
        
        ! Inicializar propiedades visuales por defecto
        this%estilo%x = 0
        this%estilo%y = 0
        this%estilo%color = 'negro'
        this%estilo%fuente = 'Arial'
        this%estilo%tamano = 12
    END SUBROUTINE inicializar_elemento

    SUBROUTINE actualizar_elemento(this, texto, grupo, visible)
        CLASS(ElementoVisual), INTENT(INOUT) :: this
        CHARACTER(*), INTENT(IN), OPTIONAL :: texto, grupo
        LOGICAL, INTENT(IN), OPTIONAL :: visible
        
        IF (PRESENT(texto)) this%texto = texto
        IF (PRESENT(grupo)) this%grupo = grupo
        IF (PRESENT(visible)) this%visible = visible
    END SUBROUTINE actualizar_elemento

    ! ======= Métodos de la InterfazVisual =======
    SUBROUTINE inicializar_interfaz(this)
        CLASS(InterfazVisual), INTENT(INOUT) :: this
        
        this%capacidad = DEFAULT_ARRAY_SIZE
        this%num_elementos = 0
        ALLOCATE(this%elementos(this%capacidad))
    END SUBROUTINE inicializar_interfaz

    SUBROUTINE expandir_capacidad(this)
        CLASS(InterfazVisual), INTENT(INOUT) :: this
        TYPE(ElementoVisual), ALLOCATABLE :: temp(:)
        INTEGER :: nueva_capacidad
        
        nueva_capacidad = this%capacidad * 2
        ALLOCATE(temp(nueva_capacidad))
        temp(1:this%num_elementos) = this%elementos(1:this%num_elementos)
        
        DEALLOCATE(this%elementos)
        ALLOCATE(this%elementos(nueva_capacidad))
        this%elementos = temp
        this%capacidad = nueva_capacidad
        
        DEALLOCATE(temp)
    END SUBROUTINE expandir_capacidad

    FUNCTION buscar_elemento(this, id) RESULT(indice)
        CLASS(InterfazVisual), INTENT(IN) :: this
        CHARACTER(*), INTENT(IN) :: id
        INTEGER :: indice, i
        
        indice = -1
        DO i = 1, this%num_elementos
            IF (TRIM(this%elementos(i)%id) == TRIM(id)) THEN
                indice = i
                EXIT
            END IF
        END DO
    END FUNCTION buscar_elemento

    SUBROUTINE agregar_elemento_interno(this, elemento)
        CLASS(InterfazVisual), INTENT(INOUT) :: this
        TYPE(ElementoVisual), INTENT(IN) :: elemento
        
        IF (this%num_elementos == this%capacidad) THEN
            CALL this%expandir()
        END IF
        
        this%num_elementos = this%num_elementos + 1
        this%elementos(this%num_elementos) = elemento
    END SUBROUTINE agregar_elemento_interno

    SUBROUTINE eliminar_elemento_interno(this, id)
        CLASS(InterfazVisual), INTENT(INOUT) :: this
        CHARACTER(*), INTENT(IN) :: id
        INTEGER :: indice, i
        
        indice = this%buscar(id)
        IF (indice > 0) THEN
            DO i = indice, this%num_elementos - 1
                this%elementos(i) = this%elementos(i + 1)
            END DO
            this%num_elementos = this%num_elementos - 1
        END IF
    END SUBROUTINE eliminar_elemento_interno

    ! ======= Funciones y Subrutinas Públicas =======
    FUNCTION crear_interfaz() RESULT(interfaz)
        TYPE(InterfazVisual) :: interfaz
        CALL interfaz%inicializar()
    END FUNCTION crear_interfaz

    SUBROUTINE agregar_elemento(interfaz, id, tipo, texto, grupo)
        TYPE(InterfazVisual), INTENT(INOUT) :: interfaz
        CHARACTER(*), INTENT(IN) :: id, tipo
        CHARACTER(*), INTENT(IN), OPTIONAL :: texto, grupo
        TYPE(ElementoVisual) :: nuevo_elemento
        
        CALL nuevo_elemento%inicializar(id, tipo)
        IF (PRESENT(texto)) CALL nuevo_elemento%actualizar(texto=texto)
        IF (PRESENT(grupo)) CALL nuevo_elemento%actualizar(grupo=grupo)
        
        CALL interfaz%agregar(nuevo_elemento)
    END SUBROUTINE agregar_elemento

    SUBROUTINE modificar_elemento(interfaz, id, texto, grupo, visible)
        TYPE(InterfazVisual), INTENT(INOUT) :: interfaz
        CHARACTER(*), INTENT(IN) :: id
        CHARACTER(*), INTENT(IN), OPTIONAL :: texto, grupo
        LOGICAL, INTENT(IN), OPTIONAL :: visible
        INTEGER :: indice
        
        indice = interfaz%buscar(id)
        IF (indice > 0) THEN
            CALL interfaz%elementos(indice)%actualizar(texto, grupo, visible)
        END IF
    END SUBROUTINE modificar_elemento

    FUNCTION obtener_elemento(interfaz, id) RESULT(elemento)
        TYPE(InterfazVisual), INTENT(IN) :: interfaz
        CHARACTER(*), INTENT(IN) :: id
        TYPE(ElementoVisual) :: elemento
        INTEGER :: indice
        
        indice = interfaz%buscar(id)
        IF (indice > 0) THEN
            elemento = interfaz%elementos(indice)
        END IF
    END FUNCTION obtener_elemento

    SUBROUTINE eliminar_elemento(interfaz, id)
        TYPE(InterfazVisual), INTENT(INOUT) :: interfaz
        CHARACTER(*), INTENT(IN) :: id
        
        CALL interfaz%eliminar(id)
    END SUBROUTINE eliminar_elemento

    SUBROUTINE mostrar_elementos(interfaz)
        TYPE(InterfazVisual), INTENT(IN) :: interfaz
        INTEGER :: i
        
        IF (interfaz%num_elementos == 0) THEN
            PRINT *, "No hay elementos para mostrar"
            RETURN
        END IF
        
        DO i = 1, interfaz%num_elementos
            PRINT *, "===== Elemento", i, "====="
            PRINT *, "ID:", TRIM(interfaz%elementos(i)%id)
            PRINT *, "Tipo:", TRIM(interfaz%elementos(i)%tipo)
            PRINT *, "Texto:", TRIM(interfaz%elementos(i)%texto)
            PRINT *, "Grupo:", TRIM(interfaz%elementos(i)%grupo)
            PRINT *, "Visible:", interfaz%elementos(i)%visible
            PRINT *, "Posición:", interfaz%elementos(i)%estilo%x, ",", &
                    interfaz%elementos(i)%estilo%y
            PRINT *, "==================="
        END DO
    END SUBROUTINE mostrar_elementos

    FUNCTION existe_elemento(interfaz, id) RESULT(existe)
        TYPE(InterfazVisual), INTENT(IN) :: interfaz
        CHARACTER(*), INTENT(IN) :: id
        LOGICAL :: existe
        
        existe = interfaz%buscar(id) > 0
    END FUNCTION existe_elemento

END MODULE Button