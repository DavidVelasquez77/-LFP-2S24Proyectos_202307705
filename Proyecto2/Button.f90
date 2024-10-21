MODULE button
    implicit none
    
    ! Usamos un sistema de matriz para manejar la interfaz
    integer, parameter :: MAX_FILAS = 50
    integer, parameter :: MAX_COLUMNAS = 50
    
    ! Constantes de estado
    integer, parameter :: INACTIVO = 0
    integer, parameter :: ACTIVO = 1
    integer, parameter :: SELECCIONADO = 2
    
    ! Matriz que representa la cuadrícula de la interfaz
    integer :: matriz_interfaz(MAX_FILAS, MAX_COLUMNAS) = 0
    
    ! Estructura para información de elementos
    type :: InfoElemento
        integer :: fila
        integer :: columna
        integer :: ancho
        integer :: alto
        character(len=32) :: nombre
        integer :: estado
        integer :: capa
        character(len=128) :: texto
    end type InfoElemento
    
    ! Lista enlazada para almacenar elementos
    type :: NodoElemento
        type(InfoElemento) :: info
        type(NodoElemento), pointer :: siguiente => null()
    end type NodoElemento
    
    type(NodoElemento), pointer :: primer_elemento => null()
    
    ! Contadores y límites
    integer :: elementos_totales = 0
    integer :: capa_actual = 1
    
contains

    subroutine inicializar_sistema()
        matriz_interfaz = 0
        elementos_totales = 0
        capa_actual = 1
        if (associated(primer_elemento)) call limpiar_memoria()
    end subroutine

    subroutine limpiar_memoria()
        type(NodoElemento), pointer :: actual, siguiente
        
        actual => primer_elemento
        do while (associated(actual))
            siguiente => actual%siguiente
            deallocate(actual)
            actual => siguiente
        end do
        primer_elemento => null()
    end subroutine

    logical function verificar_espacio(f, c, ancho, alto) result(disponible)
        integer, intent(in) :: f, c, ancho, alto
        integer :: i, j
        
        disponible = .false.
        if (f + alto > MAX_FILAS .or. c + ancho > MAX_COLUMNAS) return
        
        do i = f, f + alto - 1
            do j = c, c + ancho - 1
                if (matriz_interfaz(i,j) /= 0) return
            end do
        end do
        disponible = .true.
    end function

    subroutine insertar_elemento(nombre, f, c, ancho, alto, texto)
        character(len=*), intent(in) :: nombre, texto
        integer, intent(in) :: f, c, ancho, alto
        type(NodoElemento), pointer :: nuevo, actual
        integer :: i, j
        
        if (.not. verificar_espacio(f, c, ancho, alto)) then
            print *, "Error: Espacio ocupado o fuera de límites"
            return
        end if
        
        allocate(nuevo)
        nuevo%info = InfoElemento(f, c, ancho, alto, nombre, ACTIVO, capa_actual, texto)
        nuevo%siguiente => null()
        
        ! Actualizar matriz
        do i = f, f + alto - 1
            do j = c, c + ancho - 1
                matriz_interfaz(i,j) = elementos_totales + 1
            end do
        end do
        
        ! Insertar en lista
        if (.not. associated(primer_elemento)) then
            primer_elemento => nuevo
        else
            actual => primer_elemento
            do while (associated(actual%siguiente))
                actual => actual%siguiente
            end do
            actual%siguiente => nuevo
        end if
        
        elementos_totales = elementos_totales + 1
    end subroutine

    subroutine mover_elemento(nombre, nueva_f, nueva_c)
        character(len=*), intent(in) :: nombre
        integer, intent(in) :: nueva_f, nueva_c
        type(NodoElemento), pointer :: actual
        integer :: vieja_f, vieja_c, i, j
        
        actual => primer_elemento
        do while (associated(actual))
            if (actual%info%nombre == nombre) then
                vieja_f = actual%info%fila
                vieja_c = actual%info%columna
                
                if (verificar_espacio(nueva_f, nueva_c, actual%info%ancho, actual%info%alto)) then
                    ! Limpiar posición anterior
                    do i = vieja_f, vieja_f + actual%info%alto - 1
                        do j = vieja_c, vieja_c + actual%info%ancho - 1
                            matriz_interfaz(i,j) = 0
                        end do
                    end do
                    
                    ! Actualizar nueva posición
                    actual%info%fila = nueva_f
                    actual%info%columna = nueva_c
                    do i = nueva_f, nueva_f + actual%info%alto - 1
                        do j = nueva_c, nueva_c + actual%info%ancho - 1
                            matriz_interfaz(i,j) = elementos_totales
                        end do
                    end do
                    return
                end if
            end if
            actual => actual%siguiente
        end do
    end subroutine

    subroutine visualizar_capa(num_capa)
        integer, intent(in) :: num_capa
        type(NodoElemento), pointer :: actual
        
        print *, "=== Visualización de Capa", num_capa, "==="
        actual => primer_elemento
        do while (associated(actual))
            if (actual%info%capa == num_capa) then
                print *, "Elemento:", trim(actual%info%nombre)
                print *, "Posición:", actual%info%fila, actual%info%columna
                print *, "Dimensiones:", actual%info%ancho, "x", actual%info%alto
                print *, "Estado:", actual%info%estado
                print *, "texto:", trim(actual%info%texto)
                print *, "------------------------"
            end if
            actual => actual%siguiente
        end do
    end subroutine

    subroutine cambiar_estado(nombre, nuevo_estado)
        character(len=*), intent(in) :: nombre
        integer, intent(in) :: nuevo_estado
        type(NodoElemento), pointer :: actual
        
        actual => primer_elemento
        do while (associated(actual))
            if (actual%info%nombre == nombre) then
                actual%info%estado = nuevo_estado
                return
            end if
            actual => actual%siguiente
        end do
    end subroutine

END MODULE button