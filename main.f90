module AnalizadorApp
    use analizadorModule
    use informacionModule
    implicit none
    private
    public :: ejecutar_analizador

contains

    subroutine leer_entrada(entrada)
        character(len=:), allocatable, intent(out) :: entrada
        character(len=10000) :: buffer
        integer :: io_status
        
        entrada = ''
        do
            read(*, '(A)', iostat=io_status) buffer
            if (io_status /= 0) exit
            entrada = entrada // trim(buffer) // new_line('a')
        end do
    end subroutine leer_entrada

    subroutine procesar_entrada(entrada, anl, info)
        character(len=:), allocatable, intent(in) :: entrada
        type(Analizador), intent(inout) :: anl
        type(Informacion), intent(out) :: info

        call anl%inicializarEstado()
        call anl%analizar(entrada)
        
        call info%crearInformacion(anl%tokens)
        info%Error = anl%tieneErrores()
        
        call info%iterarTokens()
    end subroutine procesar_entrada

    subroutine ejecutar_analizador()
        character(len=:), allocatable :: entrada
        type(Analizador) :: anl
        type(Informacion) :: info

        call leer_entrada(entrada)
        call procesar_entrada(entrada, anl, info)
    end subroutine ejecutar_analizador

end module AnalizadorApp

program app
    use AnalizadorApp
    implicit none
    
    call ejecutar_analizador()
end program app