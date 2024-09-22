module analizadorModule
    use tokenModule
    use errorModule
    implicit none
    private
    public :: Analizador

    integer, parameter :: MAX_TOKENS = 500
    integer, parameter :: MAX_ERRORES = 700

    type :: Analizador
        private
        logical :: hay_errores = .false.
        integer :: estado = 0
        type(Token) :: tokens(MAX_TOKENS)
        type(Error) :: errores(MAX_ERRORES)
        integer :: linea = 1
        integer :: columna = 1
        integer :: iTokens = 1
        integer :: iErrores = 1
        integer :: i = 1
        character(:), allocatable :: buffer
    contains
        procedure :: analizar
        procedure :: agregarToken
        procedure :: agregarError
        procedure :: estado0
        procedure :: estado1
        procedure :: estado2
        procedure :: estado3
        procedure :: generarReporteTokens
        procedure :: generarReporteErrores
        procedure :: tieneErrores
    end type Analizador

contains
    subroutine analizar(this, entrada)
        class(Analizador), intent(inout) :: this
        character(len=*), intent(in) :: entrada
        integer :: longitud

        this%i = 1
        this%iTokens = 1
        this%iErrores = 1
        longitud = len_trim(entrada)

        do while(this%i <= longitud)
            select case(this%estado)
                case(0)
                    call this%estado0(entrada(this%i:this%i))
                case(1)
                    call this%estado1(entrada(this%i:this%i))
                case(2)
                    call this%estado2(entrada(this%i:this%i))
                case(3)
                    call this%estado3(entrada(this%i:this%i))
            end select
            this%i = this%i + 1
        end do

        call this%generarReporteTokens('tokens.html')
        call this%generarReporteErrores('errores.html')
    end subroutine analizar

    subroutine agregarToken(this, nombre, lexema, linea, columna)
        class(Analizador), intent(inout) :: this
        character(len=*), intent(in) :: nombre, lexema
        integer, intent(in) :: linea, columna
        
        if (this%iTokens <= MAX_TOKENS) then
            call this%tokens(this%iTokens)%crearToken(nombre, lexema, linea, columna)
            this%iTokens = this%iTokens + 1
        end if
    end subroutine agregarToken

    subroutine agregarError(this, caracter, descripcion, linea, columna)
        class(Analizador), intent(inout) :: this
        character(len=*), intent(in) :: caracter, descripcion
        integer, intent(in) :: linea, columna
        
        if (this%iErrores <= MAX_ERRORES) then
            call this%errores(this%iErrores)%crearError(caracter, descripcion, linea, columna)
            this%iErrores = this%iErrores + 1
            this%hay_errores = .true.
        end if
    end subroutine agregarError

    function tieneErrores(this) result(has_errors)
        class(Analizador), intent(in) :: this
        logical :: has_errors
        has_errors = this%hay_errores
    end function tieneErrores

    subroutine estado0(this, caracter)
        class(Analizador), intent(inout) :: this
        character(len=1), intent(in) :: caracter
        
        select case(caracter)
            case('A':'Z', 'a':'z')
                this%buffer = caracter
                this%estado = 1
            case('"')
                this%buffer = caracter
                this%estado = 2
            case('0':'9')
                this%buffer = caracter
                this%estado = 3
            case(':', '{', '}', '%', ';')
                call this%agregarToken(get_token_name(caracter), caracter, this%linea, this%columna)
            case(new_line('A'))
                this%linea = this%linea + 1
                this%columna = 0
            case(' ', achar(9))
                ! Ignorar espacios y tabulaciones
            case default
                call this%agregarError(caracter, 'Caracter no valido', this%linea, this%columna)
        end select
        this%columna = this%columna + 1
    end subroutine estado0

    subroutine estado1(this, caracter)
        class(Analizador), intent(inout) :: this
        character(len=1), intent(in) :: caracter
        
        if (is_alpha(caracter)) then
            this%buffer = this%buffer // caracter
        else
            if (is_reserved_word(this%buffer)) then
                call this%agregarToken("Palabra_reservada", this%buffer, this%linea, this%columna)
            else
                call this%agregarError(this%buffer, 'Palabra reservada mal escrita o identificador no válido', this%linea, this%columna)
            end if
            this%buffer = ''
            this%estado = 0
            this%i = this%i - 1
        end if
        this%columna = this%columna + 1
    end subroutine estado1

    subroutine estado2(this, caracter)
        class(Analizador), intent(inout) :: this
        character(len=1), intent(in) :: caracter
        
        this%buffer = this%buffer // caracter
        if (caracter == '"') then
            call this%agregarToken('cadena', this%buffer, this%linea, this%columna)
            this%buffer = ''
            this%estado = 0
        end if
        this%columna = this%columna + 1
    end subroutine estado2

    subroutine estado3(this, caracter)
        class(Analizador), intent(inout) :: this
        character(len=1), intent(in) :: caracter
        
        if (is_digit(caracter)) then
            this%buffer = this%buffer // caracter
        else
            call this%agregarToken('entero', this%buffer, this%linea, this%columna)
            this%buffer = ''
            this%estado = 0
            this%i = this%i - 1
        end if
        this%columna = this%columna + 1
    end subroutine estado3

    subroutine generarReporteTokens(this, archivo)
        class(Analizador), intent(in) :: this
        character(len=*), intent(in) :: archivo
        integer :: i, unit = 12
        
        open(unit=unit, file=archivo, status='replace', action='write')
        write(unit, '(A)') "<html><head><script src='https://cdn.tailwindcss.com'></script><style>table {border-collapse: collapse;} th, td {border: 1px solid black; padding: 5px;}</style></head><body>"
        write(unit, '(A)') "<table><tr><th>Nombre</th><th>Lexema</th><th>Linea</th><th>Columna</th></tr>"
        
        do i = 1, this%iTokens-1
            write(unit, '(A, A, A, A, A, I0, A, I0, A)') &
                "<tr class='bg-slate-500' text-center><td>", trim(this%tokens(i)%nombre), "</td><td class='bg-slate-500' text-center>", &
                trim(this%tokens(i)%lexema), "</td><td class='bg-slate-500 text-center'>", &
                this%tokens(i)%linea, "</td><td class='bg-slate-500 text-center'>", &
                this%tokens(i)%columna, "</td></tr>"
        end do
        
        write(unit, '(A)') "</table></body></html>"
        close(unit)
    end subroutine generarReporteTokens

    subroutine generarReporteErrores(this, archivo)
        class(Analizador), intent(in) :: this
        character(len=*), intent(in) :: archivo
        integer :: i, unit = 11
        
        open(unit=unit, file=archivo, status='replace', action='write')
        write(unit, '(A)') "<html><head><style>table {border-collapse: collapse;} th, td {border: 1px solid black; padding: 5px;}</style></head><body>"
        write(unit, '(A)') "<table><tr><th>Caracter</th><th>Descripcion</th><th>Linea</th><th>Columna</th></tr>"
        
        do i = 1, this%iErrores-1
            write(unit, '(A, A, A, A, A, I0, A, I0, A)') &
                "<tr><td>", trim(this%errores(i)%caracter), "</td><td>", &
                trim(this%errores(i)%descripcion), "</td><td>", &
                this%errores(i)%linea, "</td><td>", &
                this%errores(i)%columna, "</td></tr>"
        end do
        
        write(unit, '(A)') "</table></body></html>"
        close(unit)
    end subroutine generarReporteErrores

    ! Funciones auxiliares
    function is_alpha(char) result(res)
        character(len=1), intent(in) :: char
        logical :: res
        res = (char >= 'A' .and. char <= 'Z') .or. (char >= 'a' .and. char <= 'z')
    end function is_alpha

    function is_digit(char) result(res)
        character(len=1), intent(in) :: char
        logical :: res
        res = char >= '0' .and. char <= '9'
    end function is_digit

    function is_reserved_word(word) result(res)
        character(len=*), intent(in) :: word
        logical :: res
        character(len=9), dimension(7), parameter :: reserved_words = &
            [character(len=9) :: "grafica", "nombre", "continente", "pais", "poblacion", "saturacion", "bandera"]
        integer :: i
        
        res = .false.
        do i = 1, size(reserved_words)
            if (trim(word) == trim(reserved_words(i))) then
                res = .true.
                exit
            end if
        end do
    end function is_reserved_word

    function get_token_name(char) result(name)
        character(len=1), intent(in) :: char
        character(len=15) :: name
        
        select case(char)
            case(':')
                name = 'dos_puntos'
            case('{')
                name = 'llave_abre'
            case('}')
                name = 'llave_cierra'
            case('%')
                name = 'porcentaje'
            case(';')
                name = 'punto_y_coma'
            case default
                name = 'desconocido'
        end select
    end function get_token_name

end module analizadorModule