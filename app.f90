program main
    use analizadorModule
    use informacionModule 
    implicit none

    call process_input()

contains

    subroutine process_input()
        character(len=:), allocatable :: entrada
        type(Analizador) :: mi_analizador
        type(Informacion) :: mi_informacion

        entrada = read_stdin()

        call initialize_and_analyze(entrada, mi_analizador)
        call process_information(mi_analizador, mi_informacion)
    end subroutine process_input

    function read_stdin() result(entrada)
        character(len=:), allocatable :: entrada
        character(len=10000) :: buffer
        integer :: io_status

        entrada = ''
        do
            read(*, '(A)', iostat=io_status) buffer
            if (io_status /= 0) exit
            entrada = entrada // trim(buffer) // new_line('a')
        end do
    end function read_stdin

    subroutine initialize_and_analyze(entrada, mi_analizador)
        character(len=:), intent(in) :: entrada
        type(Analizador), intent(inout) :: mi_analizador

        call mi_analizador%inicializarEstado()
        call mi_analizador%analizar(entrada)
    end subroutine initialize_and_analyze

    subroutine process_information(mi_analizador, mi_informacion)
        type(Analizador), intent(in) :: mi_analizador
        type(Informacion), intent(inout) :: mi_informacion

        call mi_informacion%crearInformacion(mi_analizador%tokens)
        mi_informacion%hay_errores = mi_analizador%tieneErrores()
        call mi_informacion%iterarTokens()
    end subroutine process_information

end program main