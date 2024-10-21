MODULE utilities
    implicit none

contains

subroutine rgb_to_hex(r, g, b, hex_color)
    implicit none
    integer, intent(in) :: r, g, b  ! Componentes de color en formato RGB
    character(len=7), intent(out) :: hex_color  ! Resultado en formato hexadecimal

    ! Comprobar que los valores de RGB estén en el rango correcto
    if (r < 0 .or. r > 255 .or. g < 0 .or. g > 255 .or. b < 0 .or. b > 255) then
        print *, "Error: valores RGB fuera de rango. Deben estar entre 0 y 255."
        hex_color = '#000000'
        return
    end if


    write(hex_color, '(a,a,a)', advance='no') '#', z'hex'(r), z'hex'(g), z'hex'(b)
end subroutine rgb_to_hex


function trim_spaces(input_string) result(trimmed_string)
    implicit none
    character(len=*), intent(in) :: input_string
    character(len=255) :: trimmed_string

    trimmed_string = adjustl(adjustl(trim(input_string)))
end function trim_spaces

subroutine generate_unique_id(unique_id)
    implicit none
    character(len=20), intent(out) :: unique_id
    character(len=8) :: current_time

    write(unique_id, '(a,a)', advance='no') 'id_', current_time
end subroutine generate_unique_id

subroutine log_error(error_char, error_message, line_number, char_position)
    implicit none
    character(len=1), intent(in) :: error_char
    character(len=*), intent(in) :: error_message
    integer, intent(in) :: line_number, char_position

    print *, "Error en la línea ", line_number, ", posición ", char_position, ": ", error_message
    print *, "Caracter causante del error: '", error_char, "'"
end subroutine log_error

subroutine log_creation(log_file, log_message)
    implicit none
    character(len=*), intent(in) :: log_file, log_message
    integer :: log_unit

    ! Abrir archivo de log
    open(newunit=log_unit, file=trim(log_file), status='unknown', action='write')

    ! Escribir el mensaje en el archivo de log
    write(log_unit, '(a)') trim(log_message)

    ! Cerrar archivo de log
    close(log_unit)
end subroutine log_creation

END MODULE utilities
