module moduloEvaluador
    use moduloTokens
    use moduloErrores
    implicit none
    private
    public :: Evaluador

    integer, parameter :: MAX_ITEMS = 400
    integer, parameter :: MAX_FALLOS = 600

    type :: Evaluador
        private
        logical :: erroresPresentes = .false.
        integer :: estadoActual = 0
        type(Token) :: itemTokens(MAX_ITEMS)
        type(Error) :: listaErrores(MAX_FALLOS)
        integer :: lineaActual = 1
        integer :: posicionColumna = 1
        integer :: totalTokens = 1
        integer :: totalErrores = 1
        integer :: indice = 1
        character(:), allocatable :: bufferTemporal
    contains
        procedure :: procesarEntrada
        procedure :: insertarToken
        procedure :: insertarError
        procedure :: evaluarEstadoA
        procedure :: evaluarEstadoB
        procedure :: evaluarEstadoC
        procedure :: generarArchivoTokens
        procedure :: generarArchivoErrores
        procedure :: verificarErrores
    end type Evaluador

    implicit none
    private
    public :: StreamParser

    integer, parameter :: BUFFER_SIZE = 500
    integer, parameter :: MAX_SEGMENTS = 1000

    type :: DataSegment
        character(len=50) :: category
        character(len=100) :: value
        integer :: row
        integer :: position
    contains
        procedure :: initializeSegment
    end type DataSegment

    type :: IssueRecord
        character(len=1) :: problematicChar
        character(len=100) :: description
        integer :: row
        integer :: position
    contains
        procedure :: initializeIssue
    end type IssueRecord

    type :: StreamParser
        private
        type(DataSegment), allocatable :: segments(:)
        type(IssueRecord), allocatable :: issues(:)
        integer :: currentMode
        integer :: segmentCount
        integer :: issueCount
        integer :: currentPosition
        integer :: rowNumber
        integer :: colPosition
        character(len=BUFFER_SIZE) :: tempStorage
        logical :: hasIssues
    contains
        procedure :: initializeParser
        procedure :: parseDataStream
        procedure :: handleCharacter
        procedure :: processMode1
        procedure :: processMode2
        procedure :: processMode3
        procedure :: generateReport
    end type StreamParser

contains
    subroutine initializeParser(self)
        class(StreamParser), intent(inout) :: self
        allocate(self%segments(MAX_SEGMENTS))
        allocate(self%issues(MAX_SEGMENTS))
        self%currentMode = 0
        self%segmentCount = 1
        self%issueCount = 1
        self%currentPosition = 1
        self%rowNumber = 1
        self%colPosition = 1
        self%hasIssues = .false.
        self%tempStorage = ''
    end subroutine

    subroutine parseDataStream(self, inputStream)
        class(StreamParser), intent(inout) :: self
        character(len=*), intent(in) :: inputStream
        integer :: streamLength, i

        streamLength = len_trim(inputStream)
        do i = 1, streamLength
            call self%handleCharacter(inputStream(i:i))
        end do
        call self%generateReport('analysis_output.html')
    end subroutine

    subroutine handleCharacter(self, char)
        class(StreamParser), intent(inout) :: self
        character(len=1), intent(in) :: char

        select case (self%currentMode)
            case (0)
                call self%processMode1(char)
            case (1)
                call self%processMode2(char)
            case (2)
                call self%processMode3(char)
        end select
    end subroutine

    subroutine processMode1(self, char)
        class(StreamParser), intent(inout) :: self
        character(len=1), intent(in) :: char
        
        if (isAlpha(char)) then
            self%tempStorage = char
            self%currentMode = 1
        else if (isNumeric(char)) then
            self%tempStorage = char
            self%currentMode = 2
        else if (isSpecial(char)) then
            call addSegment(self, getCharacterType(char), char, self%rowNumber, self%colPosition)
        else if (char == achar(10)) then
            self%rowNumber = self%rowNumber + 1
            self%colPosition = 0
        else if (char /= ' ' .and. char /= achar(9)) then
            call addIssue(self, char, 'Invalid character detected', self%rowNumber, self%colPosition)
        end if
        self%colPosition = self%colPosition + 1
    end subroutine

    ! Helper functions
    function isAlpha(char) result(res)
        character(len=1), intent(in) :: char
        logical :: res
        res = (char >= 'A' .and. char <= 'Z') .or. (char >= 'a' .and. char <= 'z')
    end function

    function isNumeric(char) result(res)
        character(len=1), intent(in) :: char
        logical :: res
        res = (char >= '0' .and. char <= '9')
    end function

    function isSpecial(char) result(res)
        character(len=1), intent(in) :: char
        logical :: res
        res = any(char == ['+', '-', '*', '/', '='])
    end function

    function getCharacterType(char) result(type)
        character(len=1), intent(in) :: char
        character(len=20) :: type
        select case (char)
            case ('+')
                type = 'suma'
            case ('-')
                type = 'resta'
            case ('*')
                type = 'multiplicacion'
            case ('/')
                type = 'division'
            case ('=')
                type = 'igual'
            case default
                type = 'error'
        end select
    end function


    type :: OutputGenerator
        integer :: fileUnit
        character(len=100) :: outputPath
    contains
        procedure :: initializeOutput
        procedure :: writeHeader
        procedure :: writeContent
        procedure :: closeOutput
    end type OutputGenerator

    type :: ContentAnalyzer
        private
        character(len=200) :: accumulator
        integer :: processingPhase
        integer :: sequenceNumber
        logical :: continueProcessing
        
        contains
        procedure :: analyzeSequence
        procedure :: validateContent
        procedure :: generateOutput
        procedure :: handleSpecialCases
    end type ContentAnalyzer

    interface
        module subroutine processChunk(self, inputChar)
            class(ContentAnalyzer), intent(inout) :: self
            character(len=1), intent(in) :: inputChar
        end subroutine
    end interface

contains
    subroutine analyzeSequence(self, inputChar)
        class(ContentAnalyzer), intent(inout) :: self
        character(len=1), intent(in) :: inputChar
        
        if (isValidInput(inputChar)) then
            select case (self%processingPhase)
                case (1)
                    call handleAlphaSequence(self, inputChar)
                case (2)
                    call handleNumericSequence(self, inputChar)
                case (3)
                    call handleSpecialSequence(self, inputChar)
            end select
        else
            call logInvalidInput(self, inputChar)
        end if
    end subroutine

    function isValidInput(char) result(valid)
        character(len=1), intent(in) :: char
        logical :: valid
        valid = isAlphanumeric(char) .or. isOperator(char) .or. isWhitespace(char)
    end function

    function isAlphanumeric(char) result(valid)
        character(len=1), intent(in) :: char
        logical :: valid
        valid = isLetter(char) .or. isNumber(char)
    end function

    function isLetter(char) result(valid)
        character(len=1), intent(in) :: char
        logical :: valid
        valid = (char >= 'A' .and. char <= 'Z') .or. (char >= 'a' .and. char <= 'z')
    end function

    function isNumber(char) result(valid)
        character(len=1), intent(in) :: char
        logical :: valid
        valid = (char >= '0' .and. char <= '9')
    end function

    function isOperator(char) result(valid)
        character(len=1), intent(in) :: char
        logical :: valid
        valid = any(char == ['+', '-', '*', '/', '=', '<', '>'])
    end function

    subroutine handleAlphaSequence(self, char)
        class(ContentAnalyzer), intent(inout) :: self
        character(len=1), intent(in) :: char
        
        if (isLetter(char)) then
            self%accumulator = trim(self%accumulator) // char
        else
            call validateKeyword(self)
            self%processingPhase = 1
        end if
    end subroutine

    subroutine validateKeyword(self)
        class(ContentAnalyzer), intent(inout) :: self

        if (any(trim(self%accumulator) == validKeywords)) then
            call recordValidSequence(self, 'KEYWORD', self%accumulator)
        else
            call recordInvalidSequence(self, 'Invalid keyword found')
        end if
        self%accumulator = ''
    end subroutine

    subroutine generateOutput(self, path)
        class(ContentAnalyzer), intent(in) :: self
        character(len=*), intent(in) :: path
        type(OutputGenerator) :: reporter

        call reporter%initializeOutput(path)
        call reporter%writeHeader()
        call reporter%writeContent()
        call reporter%closeOutput()
    end subroutine

    subroutine recordValidSequence(self, category, content)
        class(ContentAnalyzer), intent(inout) :: self
        character(len=*), intent(in) :: category, content
        ! Implementation for recording valid sequences
    end subroutine

    subroutine recordInvalidSequence(self, message)
        class(ContentAnalyzer), intent(inout) :: self
        character(len=*), intent(in) :: message
        ! Implementation for recording invalid sequences
    end subroutine

end module moduloEvaluador