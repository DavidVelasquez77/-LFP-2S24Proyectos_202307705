MODULE gestor_elementos
    USE, INTRINSIC :: ISO_FORTRAN_ENV
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: RegistroElementos, crear_registro, insertar
    PUBLIC :: actualizar_elemento, obtener_info, eliminar
    PUBLIC :: mostrar_elementos, existe_elemento

    INTEGER, PARAMETER :: TEXTO_CORTO = 30
    INTEGER, PARAMETER :: TEXTO_MEDIO = 80
    INTEGER, PARAMETER :: TEXTO_LARGO = 200
    INTEGER, PARAMETER :: CAPACIDAD_BASE = 10

    TYPE :: Coordenadas
        INTEGER :: fila = 0
        INTEGER :: columna = 0
    END TYPE Coordenadas

    TYPE :: InfoAdicional
        CHARACTER(TEXTO_CORTO) :: fecha_creacion = ''
        CHARACTER(TEXTO_CORTO) :: fecha_modificacion = ''
        INTEGER :: estado = 1
        INTEGER :: prioridad = 0
    END TYPE InfoAdicional

    TYPE :: DatosElemento
        INTEGER :: identificador = 0
        CHARACTER(TEXTO_MEDIO) :: nombre = ''
        CHARACTER(TEXTO_LARGO) :: descripcion = ''
        CHARACTER(TEXTO_MEDIO) :: categoria = ''
        TYPE(Coordenadas) :: ubicacion
        TYPE(InfoAdicional) :: info
        REAL :: valor = 0.0
    CONTAINS
        PROCEDURE :: configurar => configurar_elemento
        PROCEDURE :: modificar => modificar_elemento
    END TYPE DatosElemento

    TYPE :: RegistroElementos
        PRIVATE
        TYPE(DatosElemento), ALLOCATABLE :: elementos(:)
        INTEGER :: total = 0
        INTEGER :: capacidad = 0
        INTEGER :: ultimo_id = 0
    CONTAINS
        PROCEDURE :: iniciar => iniciar_registro
        PROCEDURE :: aumentar => aumentar_capacidad
        PROCEDURE :: insertar => insertar_elemento
        PROCEDURE :: buscar => buscar_elemento
        PROCEDURE :: quitar => quitar_elemento
    END TYPE RegistroElementos

CONTAINS

    SUBROUTINE configurar_elemento(this, nombre, categoria)
        CLASS(DatosElemento), INTENT(INOUT) :: this
        CHARACTER(*), INTENT(IN) :: nombre, categoria
        
        this%nombre = nombre
        this%categoria = categoria
        this%descripcion = ''
        this%info%estado = 1
        this%info%prioridad = 1
        this%info%fecha_creacion = obtener_marca_tiempo()
        this%info%fecha_modificacion = this%info%fecha_creacion
    END SUBROUTINE configurar_elemento

    SUBROUTINE modificar_elemento(this, descripcion, valor, fila, columna)
        CLASS(DatosElemento), INTENT(INOUT) :: this
        CHARACTER(*), INTENT(IN), OPTIONAL :: descripcion
        REAL, INTENT(IN), OPTIONAL :: valor
        INTEGER, INTENT(IN), OPTIONAL :: fila, columna
        
        IF (PRESENT(descripcion)) this%descripcion = descripcion
        IF (PRESENT(valor)) this%valor = valor
        IF (PRESENT(fila)) this%ubicacion%fila = fila
        IF (PRESENT(columna)) this%ubicacion%columna = columna
        this%info%fecha_modificacion = obtener_marca_tiempo()
        this%info%prioridad = this%info%prioridad + 1
    END SUBROUTINE modificar_elemento

    SUBROUTINE iniciar_registro(this)
        CLASS(RegistroElementos), INTENT(INOUT) :: this
        
        this%capacidad = CAPACIDAD_BASE
        this%total = 0
        this%ultimo_id = 0
        ALLOCATE(this%elementos(this%capacidad))
    END SUBROUTINE iniciar_registro

    SUBROUTINE aumentar_capacidad(this)
        CLASS(RegistroElementos), INTENT(INOUT) :: this
        TYPE(DatosElemento), ALLOCATABLE :: temporal(:)
        INTEGER :: nueva_capacidad
        
        nueva_capacidad = this%capacidad * 2
        ALLOCATE(temporal(nueva_capacidad))
        temporal(1:this%total) = this%elementos(1:this%total)
        
        DEALLOCATE(this%elementos)
        ALLOCATE(this%elementos(nueva_capacidad))
        this%elementos = temporal
        this%capacidad = nueva_capacidad
        
        DEALLOCATE(temporal)
    END SUBROUTINE aumentar_capacidad

    FUNCTION buscar_elemento(this, id) RESULT(indice)
        CLASS(RegistroElementos), INTENT(IN) :: this
        INTEGER, INTENT(IN) :: id
        INTEGER :: indice, i
        
        indice = -1
        DO i = 1, this%total
            IF (this%elementos(i)%identificador == id) THEN
                indice = i
                EXIT
            END IF
        END DO
    END FUNCTION buscar_elemento

    FUNCTION crear_registro() RESULT(registro)
        TYPE(RegistroElementos) :: registro
        CALL registro%iniciar()
    END FUNCTION crear_registro

    SUBROUTINE insertar(registro, nombre, categoria, descripcion, valor, fila, columna)
        TYPE(RegistroElementos), INTENT(INOUT) :: registro
        CHARACTER(*), INTENT(IN) :: nombre, categoria
        CHARACTER(*), INTENT(IN), OPTIONAL :: descripcion
        REAL, INTENT(IN), OPTIONAL :: valor
        INTEGER, INTENT(IN), OPTIONAL :: fila, columna
        TYPE(DatosElemento) :: nuevo
        
        registro%ultimo_id = registro%ultimo_id + 1
        nuevo%identificador = registro%ultimo_id
        
        CALL nuevo%configurar(nombre, categoria)
        IF (PRESENT(descripcion)) CALL nuevo%modificar(descripcion=descripcion)
        IF (PRESENT(valor)) CALL nuevo%modificar(valor=valor)
        IF (PRESENT(fila) .AND. PRESENT(columna)) THEN
            CALL nuevo%modificar(fila=fila, columna=columna)
        END IF
        
        CALL registro%insertar_elemento(nuevo)
    END SUBROUTINE insertar

    SUBROUTINE insertar_elemento(this, elemento)
        CLASS(RegistroElementos), INTENT(INOUT) :: this
        TYPE(DatosElemento), INTENT(IN) :: elemento
        
        IF (this%total == this%capacidad) CALL this%aumentar()
        
        this%total = this%total + 1
        this%elementos(this%total) = elemento
    END SUBROUTINE insertar_elemento

    SUBROUTINE actualizar_elemento(registro, id, descripcion, valor, fila, columna)
        TYPE(RegistroElementos), INTENT(INOUT) :: registro
        INTEGER, INTENT(IN) :: id
        CHARACTER(*), INTENT(IN), OPTIONAL :: descripcion
        REAL, INTENT(IN), OPTIONAL :: valor
        INTEGER, INTENT(IN), OPTIONAL :: fila, columna
        INTEGER :: idx
        
        idx = registro%buscar(id)
        IF (idx > 0) THEN
            CALL registro%elementos(idx)%modificar(descripcion, valor, fila, columna)
        END IF
    END SUBROUTINE actualizar_elemento

    FUNCTION obtener_info(registro, id) RESULT(elemento)
        TYPE(RegistroElementos), INTENT(IN) :: registro
        INTEGER, INTENT(IN) :: id
        TYPE(DatosElemento) :: elemento
        INTEGER :: idx
        
        idx = registro%buscar(id)
        IF (idx > 0) elemento = registro%elementos(idx)
    END FUNCTION obtener_info

    SUBROUTINE mostrar_elementos(registro)
        TYPE(RegistroElementos), INTENT(IN) :: registro
        INTEGER :: i
        
        IF (registro%total == 0) THEN
            PRINT *, "No hay elementos registrados"
            RETURN
        END IF
    END SUBROUTINE mostrar_elementos

    FUNCTION existe_elemento(registro, id) RESULT(existe)
        TYPE(RegistroElementos), INTENT(IN) :: registro
        INTEGER, INTENT(IN) :: id
        LOGICAL :: existe
        
        existe = registro%buscar(id) > 0
    END FUNCTION existe_elemento

    SUBROUTINE eliminar(registro, id)
        TYPE(RegistroElementos), INTENT(INOUT) :: registro
        INTEGER, INTENT(IN) :: id
        
        CALL registro%quitar(id)
    END SUBROUTINE eliminar

    SUBROUTINE quitar_elemento(this, id)
        CLASS(RegistroElementos), INTENT(INOUT) :: this
        INTEGER, INTENT(IN) :: id
        INTEGER :: idx, i
        
        idx = this%buscar(id)
        IF (idx > 0) THEN
            DO i = idx, this%total - 1
                this%elementos(i) = this%elementos(i + 1)
            END DO
            this%total = this%total - 1
        END IF
    END SUBROUTINE quitar_elemento

    FUNCTION obtener_marca_tiempo() RESULT(marca)
        CHARACTER(TEXTO_CORTO) :: marca
        INTEGER :: valores(8)
        
        CALL DATE_AND_TIME(VALUES=valores)
        WRITE(marca, '(I4,"-",I2.2,"-",I2.2,"@",I2.2,":",I2.2)') &
              valores(1), valores(2), valores(3), valores(5), valores(6)
    END FUNCTION obtener_marca_tiempo

END MODULE gestor_elementos