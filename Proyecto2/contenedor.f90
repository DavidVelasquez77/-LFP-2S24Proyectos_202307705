MODULE contenedor
    implicit none

    type :: VisualShape
        CHARACTER(LEN = 75) :: identificador = ""
        CHARACTER(LEN = 35) :: shape_type = ""
        CHARACTER(LEN = 25) :: height_units = ""
        CHARACTER(LEN = 25) :: width_units = ""
        CHARACTER(LEN = 35) :: hue = ""
        CHARACTER(LEN = 35) :: saturation = ""
        CHARACTER(LEN = 35) :: brightness = ""
        REAL :: position_horizontal = 0.0, position_vertical = 0.0
        CHARACTER(LEN = 75) :: parent_container = ""
        CHARACTER(LEN = 75), ALLOCATABLE :: child_nodes(:)
        INTEGER :: child_count = 0
        LOGICAL :: is_visible = .true.
        REAL :: opacity = 1.0
        CHARACTER(LEN = 35) :: render_mode = "normal"
    end type VisualShape

    type(VisualShape), ALLOCATABLE :: canvas_elements(:)
    INTEGER :: total_shapes = 0
    LOGICAL :: canvas_initialized = .false.

contains

    subroutine initialize_canvas()
        if (.NOT. canvas_initialized) then
            ALLOCATE(canvas_elements(0))
            canvas_initialized = .true.
        end if
    end subroutine initialize_canvas

    subroutine cleanup_canvas()
        if (canvas_initialized) then
            if (ALLOCATED(canvas_elements)) DEALLOCATE(canvas_elements)
            canvas_initialized = .false.
        end if
    end subroutine cleanup_canvas

    subroutine link_child_to_parent(parent_identificador, child_identificador)
        character(len=*), intent(in) :: parent_identificador, child_identificador
        integer :: idx
        logical :: parent_found = .false.
        character(len=75), allocatable :: temp_children(:)

        do idx = 1, size(canvas_elements)
            if (trim(canvas_elements(idx)%identificador) == trim(parent_identificador)) then
                parent_found = .true.
                
                if (.not. allocated(canvas_elements(idx)%child_nodes)) then
                    allocate(canvas_elements(idx)%child_nodes(1))
                    canvas_elements(idx)%child_count = 1
                    canvas_elements(idx)%child_nodes(1) = trim(child_identificador)
                else
                    allocate(temp_children(canvas_elements(idx)%child_count + 1))
                    temp_children(1:canvas_elements(idx)%child_count) = canvas_elements(idx)%child_nodes
                    temp_children(canvas_elements(idx)%child_count + 1) = trim(child_identificador)
                    
                    deallocate(canvas_elements(idx)%child_nodes)
                    allocate(canvas_elements(idx)%child_nodes(size(temp_children)))
                    canvas_elements(idx)%child_nodes = temp_children
                    canvas_elements(idx)%child_count = size(temp_children)
                end if
                
                call update_parent_reference(child_identificador, parent_identificador)
                exit
            end if
        end do

        if (.not. parent_found) then
            print *, "Advertencia: No se encontró el elemento padre:", trim(parent_identificador)
        end if
    end subroutine link_child_to_parent

    subroutine create_shape(identificador, shape_type)
        CHARACTER(LEN=*), INTENT(IN) :: identificador, shape_type
        type(VisualShape) :: new_shape
        type(VisualShape), ALLOCATABLE :: temp_canvas(:)
        
        new_shape%identificador = trim(identificador)
        new_shape%shape_type = trim(shape_type)
        new_shape%opacity = 1.0
        new_shape%render_mode = "normal"
        
        if (.not. allocated(canvas_elements)) then
            allocate(canvas_elements(1))
            canvas_elements(1) = new_shape
        else
            allocate(temp_canvas(size(canvas_elements) + 1))
            temp_canvas(1:size(canvas_elements)) = canvas_elements
            temp_canvas(size(canvas_elements) + 1) = new_shape
            
            call move_alloc(temp_canvas, canvas_elements)
        end if
        
        total_shapes = total_shapes + 1
    end subroutine create_shape

    subroutine update_parent_reference(child_identificador, parent_identificador)
        character(len=*), intent(in) :: child_identificador, parent_identificador
        integer :: idx
        
        do idx = 1, size(canvas_elements)
            if (trim(canvas_elements(idx)%identificador) == trim(child_identificador)) then
                canvas_elements(idx)%parent_container = trim(parent_identificador)
                exit
            end if
        end do
    end subroutine update_parent_reference

    subroutine set_shape_dimensions(identificador, height, width)
        CHARACTER(LEN=*), INTENT(IN) :: identificador, height, width
        integer :: idx
        
        do idx = 1, size(canvas_elements)
            if (trim(canvas_elements(idx)%identificador) == trim(identificador)) then
                canvas_elements(idx)%height_units = trim(height)
                canvas_elements(idx)%width_units = trim(width)
                exit
            end if
        end do
    end subroutine set_shape_dimensions

    subroutine set_shape_color(identificador, h, s, b)
        CHARACTER(LEN=*), INTENT(IN) :: identificador, h, s, b
        integer :: idx
        
        do idx = 1, size(canvas_elements)
            if (trim(canvas_elements(idx)%identificador) == trim(identificador)) then
                canvas_elements(idx)%hue = trim(h)
                canvas_elements(idx)%saturation = trim(s)
                canvas_elements(idx)%brightness = trim(b)
                exit
            end if
        end do
    end subroutine set_shape_color

    subroutine set_shape_coordinates(identificador, x, y)
        CHARACTER(LEN=*), INTENT(IN) :: identificador
        REAL, INTENT(IN) :: x, y
        integer :: idx
        
        do idx = 1, size(canvas_elements)
            if (trim(canvas_elements(idx)%identificador) == trim(identificador)) then
                canvas_elements(idx)%position_horizontal = x
                canvas_elements(idx)%position_vertical = y
                exit
            end if
        end do
    end subroutine set_shape_coordinates

    subroutine display_canvas_state()
        integer :: i, j
        
        if (.not. allocated(canvas_elements)) then
            print *, "El lienzo está vacío"
            return
        end if
        
        print *, "Informe del Estado del Lienzo"
        print *, "Total de Formas:", total_shapes
        print *, "------------------------"
        
        do i = 1, size(canvas_elements)
            print *, "Identificador:", trim(canvas_elements(i)%identificador)
            print *, "Tipo de Forma:", trim(canvas_elements(i)%shape_type)
            print *, "Contenedor Padre:", trim(canvas_elements(i)%parent_container)
            print *, "Posición: (", canvas_elements(i)%position_horizontal, ",", &
                    canvas_elements(i)%position_vertical, ")"
            print *, "Dimensiones:", trim(canvas_elements(i)%height_units), "x", &
                    trim(canvas_elements(i)%width_units)
            print *, "Color (HSB):", trim(canvas_elements(i)%hue), ",", &
                    trim(canvas_elements(i)%saturation), ",", &
                    trim(canvas_elements(i)%brightness)
            print *, "Opacidad:", canvas_elements(i)%opacity
            print *, "Modo de Renderizado:", trim(canvas_elements(i)%render_mode)
            
            if (canvas_elements(i)%child_count > 0) then
                print *, "Elementos Hijos:"
                do j = 1, canvas_elements(i)%child_count
                    print *, "  -", trim(canvas_elements(i)%child_nodes(j))
                end do
            else
                print *, "Sin Elementos Hijos"
            end if
            print *, "------------------------"
        end do
    end subroutine display_canvas_state

END MODULE contenedor