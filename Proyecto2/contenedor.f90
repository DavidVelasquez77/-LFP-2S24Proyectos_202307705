MODULE sistema_layout
    implicit none
    
    ! Enumeraciones para tipos de diseño
    integer, parameter :: FLUJO_HORIZONTAL = 1
    integer, parameter :: FLUJO_VERTICAL = 2
    integer, parameter :: FLUJO_GRID = 3
    
    ! Estructura para colores RGB
    type :: ColorRGB
        integer :: r = 255
        integer :: g = 255
        integer :: b = 255
    end type
    
    ! Estructura para dimensiones
    type :: Dimensiones
        real :: x = 0.0
        real :: y = 0.0
        real :: ancho = 100.0
        real :: alto = 100.0
        real :: margen = 0.0
        real :: relleno = 0.0
    end type
    
    ! Estructura principal de nodo
    type :: Nodo
        character(len=64) :: identificador
        integer :: tipo_flujo
        type(ColorRGB) :: color
        type(Dimensiones) :: dim
        logical :: visible = .true.
        logical :: expandible = .true.
        
        ! Punteros para estructura de árbol
        type(Nodo), allocatable :: padre
        type(Nodo), allocatable :: primer_hijo
        type(Nodo), allocatable :: siguiente
        type(Nodo), allocatable :: anterior
    contains
        procedure :: inicializar => inicializar_nodo
        procedure :: liberar => liberar_nodo
        procedure :: set_padre => asignar_padre
        procedure :: set_hijo => asignar_hijo
        procedure :: set_siguiente => asignar_siguiente
        procedure :: set_anterior => asignar_anterior
    end type
    
    ! Raíz del árbol
    type(Nodo), allocatable :: raiz
    
contains

    subroutine inicializar_nodo(this)
        class(Nodo) :: this
        if (allocated(this%padre)) deallocate(this%padre)
        if (allocated(this%primer_hijo)) deallocate(this%primer_hijo)
        if (allocated(this%siguiente)) deallocate(this%siguiente)
        if (allocated(this%anterior)) deallocate(this%anterior)
    end subroutine

    subroutine asignar_padre(this, otro)
        class(Nodo) :: this
        type(Nodo), intent(in) :: otro
        if (allocated(this%padre)) deallocate(this%padre)
        allocate(this%padre, source=otro)
    end subroutine

    subroutine asignar_hijo(this, hijo)
        class(Nodo) :: this
        type(Nodo), intent(in) :: hijo
        if (allocated(this%primer_hijo)) deallocate(this%primer_hijo)
        allocate(this%primer_hijo, source=hijo)
    end subroutine

    subroutine asignar_siguiente(this, siguiente)
        class(Nodo) :: this
        type(Nodo), intent(in) :: siguiente
        if (allocated(this%siguiente)) deallocate(this%siguiente)
        allocate(this%siguiente, source=siguiente)
    end subroutine

    subroutine asignar_anterior(this, anterior)
        class(Nodo) :: this
        type(Nodo), intent(in) :: anterior
        if (allocated(this%anterior)) deallocate(this%anterior)
        allocate(this%anterior, source=anterior)
    end subroutine

    subroutine liberar_nodo(this)
        class(Nodo) :: this
        if (allocated(this%primer_hijo)) then
            call this%primer_hijo%liberar()
            deallocate(this%primer_hijo)
        end if
        if (allocated(this%siguiente)) then
            call this%siguiente%liberar()
            deallocate(this%siguiente)
        end if
    end subroutine

    subroutine inicializar_sistema()
        if (allocated(raiz)) then
            call raiz%liberar()
            deallocate(raiz)
        end if
        allocate(raiz)
        call raiz%inicializar()
        raiz%identificador = "root"
        raiz%tipo_flujo = FLUJO_VERTICAL
    end subroutine

    function crear_nodo(id, tipo) result(nuevo)
        character(len=*), intent(in) :: id
        integer, intent(in) :: tipo
        type(Nodo), allocatable :: nuevo
        
        allocate(nuevo)
        call nuevo%inicializar()
        nuevo%identificador = id
        nuevo%tipo_flujo = tipo
        nuevo%color = ColorRGB(255, 255, 255)
        nuevo%dim = Dimensiones()
    end function

    recursive function encontrar_nodo(actual, id) result(encontrado)
        type(Nodo), intent(in) :: actual
        character(len=*), intent(in) :: id
        type(Nodo), allocatable :: encontrado
        type(Nodo), allocatable :: temp_hijo
        type(Nodo), allocatable :: temp_siguiente
        
        if (trim(actual%identificador) == trim(id)) then
            allocate(encontrado, source=actual)
            return
        end if
        
        if (allocated(actual%primer_hijo)) then
            temp_hijo = actual%primer_hijo
            if (allocated(temp_hijo)) then
                encontrado = encontrar_nodo(temp_hijo, id)
                if (allocated(encontrado)) return
            end if
        end if
        
        if (allocated(actual%siguiente)) then
            temp_siguiente = actual%siguiente
            if (allocated(temp_siguiente)) then
                encontrado = encontrar_nodo(temp_siguiente, id)
            end if
        end if
    end function

    subroutine insertar_nodo(padre_id, nuevo_id, tipo_flujo)
        character(len=*), intent(in) :: padre_id, nuevo_id
        integer, intent(in) :: tipo_flujo
        type(Nodo), allocatable :: padre, nuevo, actual, temp
        
        padre = encontrar_nodo(raiz, padre_id)
        if (.not. allocated(padre)) then
            print *, "Error: Padre no encontrado -", trim(padre_id)
            return
        end if
        
        nuevo = crear_nodo(nuevo_id, tipo_flujo)
        call nuevo%set_padre(padre)
        
        if (.not. allocated(padre%primer_hijo)) then
            call padre%set_hijo(nuevo)
        else
            actual = padre%primer_hijo
            do while (allocated(actual%siguiente))
                temp = actual%siguiente
                actual = temp
            end do
            call actual%set_siguiente(nuevo)
            call nuevo%set_anterior(actual)
        end if
        
        call recalcular_layout()
    end subroutine

    recursive subroutine recalcular_layout()
        if (.not. allocated(raiz)) return
        call calcular_dimensiones_recursivo(raiz, 0.0, 0.0)
    end subroutine

    recursive subroutine calcular_dimensiones_recursivo(nodo, x_base, y_base)
        type(Nodo), intent(inout) :: nodo
        real, intent(in) :: x_base, y_base
        type(Nodo), allocatable :: hijo, temp
        real :: x_actual, y_actual, max_x, max_y
        
        if (.not. nodo%visible) return
        
        x_actual = x_base + nodo%dim%margen
        y_actual = y_base + nodo%dim%margen
        max_x = x_actual
        max_y = y_actual
        
        nodo%dim%x = x_actual
        nodo%dim%y = y_actual
        
        if (allocated(nodo%primer_hijo)) then
            hijo = nodo%primer_hijo
            do while (allocated(hijo))
                select case (nodo%tipo_flujo)
                    case (FLUJO_HORIZONTAL)
                        call calcular_dimensiones_recursivo(hijo, x_actual, y_actual)
                        x_actual = x_actual + hijo%dim%ancho + nodo%dim%relleno
                        max_x = max(max_x, x_actual)
                        max_y = max(max_y, y_actual + hijo%dim%alto)
                    case (FLUJO_VERTICAL)
                        call calcular_dimensiones_recursivo(hijo, x_actual, y_actual)
                        y_actual = y_actual + hijo%dim%alto + nodo%dim%relleno
                        max_x = max(max_x, x_actual + hijo%dim%ancho)
                        max_y = max(max_y, y_actual)
                    case (FLUJO_GRID)
                        call calcular_dimensiones_recursivo(hijo, x_actual, y_actual)
                        if (x_actual - x_base > nodo%dim%ancho) then
                            x_actual = x_base + nodo%dim%margen
                            y_actual = max_y + nodo%dim%relleno
                        else
                            x_actual = x_actual + hijo%dim%ancho + nodo%dim%relleno
                        end if
                        max_x = max(max_x, x_actual)
                        max_y = max(max_y, y_actual + hijo%dim%alto)
                end select
                
                if (allocated(hijo%siguiente)) then
                    temp = hijo%siguiente
                    hijo = temp
                else
                    deallocate(hijo)
                end if
            end do
        end if
        
        if (nodo%expandible) then
            nodo%dim%ancho = max(nodo%dim%ancho, max_x - x_base + nodo%dim%margen)
            nodo%dim%alto = max(nodo%dim%alto, max_y - y_base + nodo%dim%margen)
        end if
    end subroutine

    recursive subroutine imprimir_arbol(nodo, nivel)
        type(Nodo), intent(in) :: nodo
        integer, intent(in) :: nivel
        character(len=100) :: indentacion
        type(Nodo), allocatable :: hijo, temp
        integer :: i
        
        indentacion = ""
        do i = 1, nivel
            indentacion = trim(indentacion) // "  "
        end do
        
        print *, trim(indentacion) // "ID: " // trim(nodo%identificador)
        print *, trim(indentacion) // "Posición: (", nodo%dim%x, ",", nodo%dim%y, ")"
        print *, trim(indentacion) // "Dimensiones: ", nodo%dim%ancho, "x", nodo%dim%alto
        print *, trim(indentacion) // "Color: (", nodo%color%r, ",", nodo%color%g, ",", nodo%color%b, ")"
        print *, trim(indentacion) // "Visible: ", nodo%visible
        print *, trim(indentacion) // "Expandible: ", nodo%expandible
        
        if (allocated(nodo%primer_hijo)) then
            hijo = nodo%primer_hijo
            do while (allocated(hijo))
                call imprimir_arbol(hijo, nivel + 1)
                if (allocated(hijo%siguiente)) then
                    temp = hijo%siguiente
                    hijo = temp
                else
                    deallocate(hijo)
                end if
            end do
        end if
    end subroutine

END MODULE sistema_layout