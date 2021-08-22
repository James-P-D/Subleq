PROGRAM SUBLEQ
    ! Forces you to declare all variables
    IMPLICIT NONE
    CHARACTER*100 :: input_filename
    INTEGER, DIMENSION(0:100) :: code
    INTEGER :: code_length
    CHARACTER (LEN = 10), dimension(0:100) :: token_array
    
    IF(COMMAND_ARGUMENT_COUNT().NE.1) THEN
        PRINT *, 'ERROR: Expected input file'
        STOP
    ENDIF
    
    CALL GET_COMMAND_ARGUMENT(1, input_filename) 
    
    CALL TOKENISE(input_filename, token_array, code_length)

    CALL PARSE(token_array, code_length, code)
    
    CALL EXECUTE_CODE(code, code_length)
    
    CONTAINS
    
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! TOKENISE()
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        SUBROUTINE TOKENISE(filename, token_array, code_length)
            IMPLICIT NONE
            CHARACTER (LEN = 100) :: filename
            CHARACTER (LEN = 100) :: line_read
            CHARACTER (LEN = 100) :: token            
            INTEGER :: error, comment_index, space_index, token_index, code_length
            !CHARACTER (LEN = 1000), dimension(0:10) :: token_array
            CHARACTER (LEN = 10), dimension(0:100) :: token_array
            
            token_index = 0
            OPEN(11, file=input_filename, status='old')
            DO
                READ (11, '(A)', IOSTAT = error) line_read
                IF (error > 0) THEN
                    ! Nothing more to read, so EXIT (break) from DO loop
                    EXIT
                ELSE                    
                    ! Remove any comments (any text following a '#' symbol)
                    comment_index = INDEX(line_read, '#')
                    IF (comment_index > 0) THEN
                        line_read = line_read(:comment_index - 1)
                    END IF
                    line_read = TRIM(line_read)
                                        
                    DO WHILE (LEN(TRIM(line_read)) > 0)
                        space_index = INDEX(line_read, ' ')
                        IF (space_index > 0) THEN
                            token = TRIM(line_read(:space_index - 1))
                            line_read = TRIM(line_read(space_index + 1:))                            
                        ELSE
                            token = TRIM(line_read)
                        END IF                        
                        
                        IF (LEN(token) > 0) THEN
                            !WRITE (*, '(A)') token
                            token_array(token_index) = token
                            token_index =  token_index + 1
                        ELSE
                            EXIT
                        END IF
                    END DO
                END IF
            END DO
            
            ! Close the file, but keep it
            CLOSE(11, status="KEEP")
            
            code_length = token_index            
        END SUBROUTINE TOKENISE
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! STR_TO_INT() - Stolen from https://stackoverflow.com/questions/24071722/converting-a-string-to-an-integer-in-fortran-90/24077338
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        SUBROUTINE STR_TO_INT(str, int, stat)
            IMPLICIT NONE
            ! Arguments
            CHARACTER (len=*), INTENT(IN) :: str
            INTEGER, INTENT(OUT)          :: int
            INTEGER, INTENT(OUT)          :: stat

            READ(str, *, iostat=stat) int
        END SUBROUTINE STR_TO_INT
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! REC_PARSE()
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        SUBROUTINE REC_PARSE(token_array, code_length, code, replace_this, with_this)
            IMPLICIT NONE
            INTEGER, DIMENSION(0:100) :: code
            INTEGER :: code_length, n, stat, output_int
            CHARACTER (LEN = 10), dimension(0:100) :: token_array
            CHARACTER (LEN = 10) replace_this, with_this
            
            DO n = 0, code_length, 1
                WRITE (*, '(A)') TRIM(token_array(n))   
                CALL STR_TO_INT(TRIM(token_array(n)), output_int, stat)
                IF (STAT == 0) THEN
                    code(n) = output_int
                END IF
            END DO
            
        END SUBROUTINE REC_PARSE
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! PARSE()
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        SUBROUTINE PARSE(token_array, code_length, code)
            IMPLICIT NONE
            INTEGER, DIMENSION(0:100) :: code
            INTEGER :: code_length, n
            CHARACTER (LEN = 10), dimension(0:100) :: token_array
            
            !DO n = 0, code_length, 1
            !    WRITE (*, '(A)') TRIM(token_array(n))                
            !END DO
            
            CALL REC_PARSE(token_array, code_length, code, '', '')
            
        END SUBROUTINE PARSE

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! EXECUTE_CODE()
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        SUBROUTINE EXECUTE_CODE(code, code_length)
            IMPLICIT NONE
            INTEGER, DIMENSION(0:100) :: code
            INTEGER code_length, code_index, a, b, c
            CHARACTER(1) :: output_char, input_char
            
            code_index = 0
            DO WHILE (code_index >= 0)
                IF (code_index + 2 > code_length) THEN
                    PRINT *, 'ERROR: Code over-run'
                    STOP
                END IF
                a = code(code_index)
                code_index = code_index + 1
                b = code(code_index)
                code_index = code_index + 1
                c = code(code_index)
                code_index = code_index + 1
                IF (a < 0) THEN
                    PRINT *, 'ERROR: Input not yet supported'
                    STOP                
                ELSE IF (b < 0) THEN
                    IF (a < 0) THEN
                        PRINT *, 'ERROR: Memory under-run when outputting'
                        STOP
                    ELSE IF (a > code_length) THEN
                        PRINT *, 'ERROR: Memory over-run when outputting'
                        STOP
                    ELSE
                        output_char = char(code(a))
                        write(*, "(2a)", advance="no") output_char
                    END IF
                ELSE
                    IF ((a < 0) .OR. (b < 0)) THEN
                        PRINT *, 'ERROR: Memory under-run'
                        STOP
                    ELSE IF ((a > code_length) .OR. (b > code_length)) THEN
                        PRINT *, 'ERROR: Memory over-run'
                        STOP
                    ELSE
                        code(b) = code(b) - code(a)
                        IF (code(b) <= 0) THEN
                            code_index = c
                        END IF
                    END IF
                END IF                
            END DO
            
            PRINT *, ''
            PRINT *, ''
            PRINT *, 'Program complete!'
        END SUBROUTINE EXECUTE_CODE
    
END PROGRAM SUBLEQ

