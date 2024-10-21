MODULE lexAnalyzer
    implicit none

    type :: InvalidToken
        CHARACTER(LEN = 150) :: token_sequence
        CHARACTER(LEN = 200) :: issue_description
        CHARACTER(LEN = 50) :: error_category
        INTEGER :: row_position
        INTEGER :: column_position
        REAL :: detection_time
        CHARACTER(LEN = 100) :: line_context
        LOGICAL :: is_processed = .false.
    end type InvalidToken

    type :: AnalysisStatistics
        INTEGER :: analyzed_tokens = 0
        INTEGER :: invalid_token_count = 0
        INTEGER :: processed_lines = 0
        REAL :: total_analysis_time = 0.0
        CHARACTER(LEN = 100) :: last_valid_token = ""
    end type AnalysisStatistics

    type(InvalidToken), ALLOCATABLE :: invalid_tokens_record(:)
    type(AnalysisStatistics) :: stats
    LOGICAL :: is_active = .false.

contains 

    subroutine start_analysis()
        if (.NOT. is_active) then
            if (ALLOCATED(invalid_tokens_record)) DEALLOCATE(invalid_tokens_record)
            ALLOCATE(invalid_tokens_record(0))
            stats%analyzed_tokens = 0
            stats%invalid_token_count = 0
            stats%processed_lines = 0
            stats%total_analysis_time = 0.0
            stats%last_valid_token = ""
            is_active = .true.
        end if
    end subroutine start_analysis

    subroutine log_invalid_token(seq, category, description, line, column, &
                                  time, context)
        CHARACTER(LEN=*), INTENT(IN) :: seq, category, description, context
        INTEGER, INTENT(IN) :: line, column
        REAL, INTENT(IN) :: time
        type(InvalidToken) :: new_invalid_token
        type(InvalidToken), ALLOCATABLE :: temp_buffer(:)
        INTEGER :: token_count
        
        if (.NOT. is_active) call start_analysis()

        new_invalid_token%token_sequence = seq
        new_invalid_token%error_category = category
        new_invalid_token%issue_description = description
        new_invalid_token%row_position = line
        new_invalid_token%column_position = column
        new_invalid_token%detection_time = time
        new_invalid_token%line_context = context
        new_invalid_token%is_processed = .true.

        stats%invalid_token_count = stats%invalid_token_count + 1
        stats%total_analysis_time = stats%total_analysis_time + time
        
        if (.NOT. ALLOCATED(invalid_tokens_record)) then
            ALLOCATE(invalid_tokens_record(1))
            invalid_tokens_record(1) = new_invalid_token
        else
            token_count = size(invalid_tokens_record)
            ALLOCATE(temp_buffer(token_count + 1))
            temp_buffer(1:token_count) = invalid_tokens_record
            temp_buffer(token_count + 1) = new_invalid_token
            
            DEALLOCATE(invalid_tokens_record)
            ALLOCATE(invalid_tokens_record(token_count + 1))
            invalid_tokens_record = temp_buffer
            DEALLOCATE(temp_buffer)
        end if
    end subroutine log_invalid_token

    subroutine print_console_report()
        INTEGER :: i
        CHARACTER(LEN=25) :: str_line, str_column, str_time
        
        if (.NOT. is_active .OR. .NOT. ALLOCATED(invalid_tokens_record)) then
            print *, "Lexical Analysis System: No analysis records found."
            return
        end if
        
        DO i = 1, size(invalid_tokens_record)
            write(str_line, '(I0)') invalid_tokens_record(i)%row_position
            write(str_column, '(I0)') invalid_tokens_record(i)%column_position
            write(str_time, '(F8.3)') invalid_tokens_record(i)%detection_time
        
        END DO
    end subroutine print_console_report
    
    subroutine create_report()
        INTEGER :: i, output_unit
        CHARACTER(LEN=25) :: str_line, str_column, str_time
        
        output_unit = 15
        open(unit=output_unit, file='lexical_analysis_report.md', status='replace')
        
        write(output_unit, '(A)') "# Lexical Analysis Report"
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "## Statistical Summary"
        write(output_unit, '(A,I0)') "* Tokens Processed: ", stats%analyzed_tokens
        write(output_unit, '(A,I0)') "* Invalid Tokens: ", stats%invalid_token_count
        write(output_unit, '(A,I0)') "* Analyzed Lines: ", stats%processed_lines
        write(str_time, '(F8.3)') stats%total_analysis_time
        write(output_unit, '(A,A,A)') "* Total Time: ", trim(str_time), " seconds"
        write(output_unit, '(A)') ""
        
        if (size(invalid_tokens_record) > 0) then
            write(output_unit, '(A)') "## Invalid Tokens Detail"
            write(output_unit, '(A)') ""
            
            DO i = 1, size(invalid_tokens_record)
                write(str_line, '(I0)') invalid_tokens_record(i)%row_position
                write(str_column, '(I0)') invalid_tokens_record(i)%column_position
                
                write(output_unit, '(A,I0)') "### Invalid Token #", i
                write(output_unit, '(A)') ""
                write(output_unit, '(A,A)') "* **Category:** ", &
                    trim(invalid_tokens_record(i)%error_category)
                write(output_unit, '(A,A)') "* **Sequence:** `", &
                    trim(invalid_tokens_record(i)%token_sequence) // "`"
                write(output_unit, '(A,A)') "* **Issue:** ", &
                    trim(invalid_tokens_record(i)%issue_description)
                write(output_unit, '(A,A,A,A)') "* **Location:** Line ", &
                    trim(str_line), ", Column ", trim(str_column)
                write(output_unit, '(A,A)') "* **Context:** ", &
                    trim(invalid_tokens_record(i)%line_context)
                write(output_unit, '(A)') ""
                write(output_unit, '(A)') "---"
            END DO
        else
            write(output_unit, '(A)') "## No invalid tokens detected"
        end if
        
        close(output_unit)
    end subroutine create_report

END MODULE lexAnalyzer
