PROGRAM lexer_core
    USE token_handler
    USE error_processor
    USE output_generator
    USE syntax_checker
  
    IMPLICIT NONE
    INTEGER :: input_size, line_count, char_pos, machine_state, cursor
    INTEGER :: io_result, input_unit
    CHARACTER(LEN=200000) :: raw_text, line_buffer
    CHARACTER(LEN=1) :: current_char
    CHARACTER(LEN=200) :: token_buffer
    INTEGER :: html_unit, css_unit
    INTEGER :: io_status
    CHARACTER(LEN=100) :: html_file = "lexer_output.html"
    CHARACTER(LEN=100) :: css_file = "lexer_styles.css"
    machine_state = 0
    cursor = 1
    char_pos = 0
    line_count = 1
    token_buffer = ""
  
    raw_text = ""
  
    DO
      READ(*, '(A)', IOSTAT=io_result) line_buffer
      IF (io_result < 0) EXIT
      raw_text = TRIM(raw_text) // TRIM(line_buffer) // NEW_LINE('a')
    END DO
  
    CLOSE(input_unit)
  
    input_size = LEN_TRIM(raw_text)
  
    DO WHILE(cursor <= input_size)
      current_char = raw_text(cursor:cursor)
      SELECT CASE (machine_state)
        CASE (0)
          IF (IS_SPECIAL_CHAR(current_char)) THEN
            machine_state = 1
            char_pos = char_pos + 1
          ELSE IF (IS_ALPHA(current_char)) THEN
            machine_state = 2
          ELSE IF (IS_DIGIT(current_char)) THEN
            machine_state = 3
          ELSE IF (current_char == '"') THEN
            token_buffer = TRIM(token_buffer) // current_char
            char_pos = char_pos + 1
            cursor = cursor + 1 
            machine_state = 4      
          ELSE IF (current_char == '/') THEN
            machine_state = 7
            token_buffer = TRIM(token_buffer) // current_char
            char_pos = char_pos + 1
            cursor = cursor + 1 
          ELSE IF (IS_NEWLINE(current_char)) THEN
            char_pos = 0
            line_count = line_count + 1
            cursor = cursor + 1
          ELSE IF (IS_TAB(current_char)) THEN
            char_pos = char_pos + 4
            cursor = cursor + 1
          ELSE IF (IS_SPACE(current_char)) THEN
            char_pos = char_pos + 1
            cursor = cursor + 1  
          ELSE
            CALL log_error(current_char, "invalido", line_count, char_pos)
            char_pos = char_pos + 1
            cursor = cursor + 1 
          END IF
          
        CASE (1)
          IF (IS_SPECIAL_CHAR(current_char)) THEN
            CALL register_token(current_char, get_token_category(current_char), line_count, char_pos)
          ELSE
            CALL log_error(current_char, "invalido", line_count, char_pos)
            char_pos = char_pos + 1
            cursor = cursor + 1 
          END IF
          cursor = cursor + 1
          machine_state = 0
  
        CASE (2)
          IF (IS_ALPHANUMERIC(current_char)) THEN
            token_buffer = TRIM(token_buffer) // current_char
            char_pos = char_pos + 1
            cursor = cursor + 1
          ELSE
            CALL process_identifier(token_buffer, line_count, char_pos)
            token_buffer = ""
            machine_state = 0      
          END IF
  
        CASE (3)
          IF (IS_DIGIT(current_char)) THEN
            token_buffer = TRIM(token_buffer) // current_char
            char_pos = char_pos + 1
            cursor = cursor + 1
          ELSE
            CALL register_token(token_buffer, 'numeric', line_count, char_pos)
            token_buffer = ""
            machine_state = 0
          END IF
  
        CASE (4)
          IF (IS_PRINTABLE(current_char) .AND. current_char /= '"') THEN
            token_buffer = TRIM(token_buffer) // current_char
            char_pos = char_pos + 1
            cursor = cursor + 1 
            machine_state = 6
          ELSE IF (current_char == '"') THEN
            machine_state = 5
          ELSE
            CALL log_error(current_char, "Non-printable character in string", line_count, char_pos)
            token_buffer = ""
            machine_state = 0
          END IF
  
        CASE (5)
          token_buffer = TRIM(token_buffer) // current_char
          char_pos = char_pos + 1
          cursor = cursor + 1
          CALL register_token(token_buffer, 'string_literal', line_count, char_pos)
          token_buffer = ""
          machine_state = 0
              
        CASE (6)
          IF (IS_PRINTABLE(current_char) .AND. current_char /= '"') THEN
            token_buffer = TRIM(token_buffer) // current_char
            char_pos = char_pos + 1
            cursor = cursor + 1 
          ELSE IF (current_char == '"') THEN
            machine_state = 5
          ELSE
            CALL log_error(current_char, "Unclosed string literal", line_count, char_pos)
            token_buffer = ""
            machine_state = 0
          END IF             
        CASE (7)
          IF (current_char == '/') THEN
            machine_state = 8
            token_buffer = TRIM(token_buffer) // current_char
            char_pos = char_pos + 1
            cursor = cursor + 1
          ELSE IF (current_char == '*') THEN
            machine_state = 9
            token_buffer = TRIM(token_buffer) // current_char
            char_pos = char_pos + 1
            cursor = cursor + 1
          ELSE
            CALL register_token(token_buffer, 'division_op', line_count, char_pos - 1)
            token_buffer = ""
            machine_state = 0
          END IF
        
        CASE (8)
          IF (IS_NEWLINE(current_char)) THEN
            CALL register_token(TRIM(token_buffer), 'line_comment', line_count, char_pos - LEN_TRIM(token_buffer))
            token_buffer = ""
            machine_state = 0
            line_count = line_count + 1
            char_pos = 0
          ELSE
            token_buffer = TRIM(token_buffer) // current_char
            char_pos = char_pos + 1
          END IF
          cursor = cursor + 1
        
      END SELECT
    END DO
    
    CALL parse_syntax()
    CALL display_errors()
    CALL generate_token_report()
    CALL generate_error_report()
    CALL display_label_info()
    CALL display_container_info()
    CALL display_button_info()
    CALL display_key_info()
    CALL display_text_info()
    CALL generate_output_files()
    CALL display_python_tokens()
    CALL display_python_errors()
    
  END PROGRAM lexer_core