PROGRAM SUBLEQ
    IMPLICIT NONE
    CHARACTER * 100                        :: input_filename ! Input file containing Subleq program
    INTEGER, DIMENSION(0:100)              :: code           ! Integer array for our code
    INTEGER                                :: code_length    ! Length of our machine code
    CHARACTER (LEN = 10), dimension(0:100) :: token_array    ! 100 element array of 10-character strings for storing tokens when parsing
    
    ! Check we have an argument to the executable.
    ! If not display the usage message
    IF(COMMAND_ARGUMENT_COUNT().NE.1) THEN
        PRINT *, '  ▄▄▄                                '
        PRINT *, ' █   █       █     █                 '
        PRINT *, ' █           █     █                 '
        PRINT *, '  ▀▀▀▄ █   █ █▀▀▀▄ █     ▄▀▀▀▄ ▄▀▀▀▄ '
        PRINT *, '     █ █   █ █   █ █     █▄▄▄▀ █   █ '
        PRINT *, ' █   █ █   █ █   █ █   ▄ █   ▄ █   █ '
        PRINT *, '  ▀▀▀   ▀▀▀   ▀▀▀   ▀▀▀   ▀▀▀   ▀▀▀█ '
        PRINT *, '                                   █ '
        PRINT *, 'ERROR: Expected input file           '
        PRINT *, '                                     '
        PRINT *, 'Usage: ./subleq.exe FILE.slq         '
        STOP
    ENDIF
    
    ! Get the first parameter and put into input_filename
    CALL GET_COMMAND_ARGUMENT(1, input_filename) 
    
    ! Tokenise the file into our array of strings
    CALL TOKENISE(input_filename, token_array, code_length)
    
    ! Convert our tokens into integers
    CALL PARSE(token_array, code_length, code)
    
    ! Execute the array of integers
    CALL EXECUTE_CODE(code, code_length)
    
    ! Sub-functions below:
    CONTAINS
    
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! TOKENISE()
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        SUBROUTINE TOKENISE(filename, token_array, code_length)
            IMPLICIT NONE
            CHARACTER (LEN = 100) :: filename
            CHARACTER (LEN = 100) :: line_read
            CHARACTER (LEN = 100) :: token            
            INTEGER               :: error, comment_index, space_index, token_index, code_length
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
                    
                    ! Split on space, and save any non-empty strings as tokens
                    DO WHILE (LEN(TRIM(line_read)) > 0)
                        space_index = INDEX(line_read, ' ')
                        IF (space_index > 0) THEN
                            token = TRIM(line_read(:space_index - 1))
                            line_read = TRIM(line_read(space_index + 1:))                            
                        ELSE
                            token = TRIM(line_read)
                        END IF                        
                        
                        IF (LEN(token) > 0) THEN
                            token_array(token_index) = token
                            token_index =  token_index + 1
                        ELSE
                            ! Break out of loop
                            EXIT
                        END IF
                    END DO
                END IF
            END DO
            
            ! Close the file, but keep it
            CLOSE(11, status="KEEP")
            
            ! Finally, number-of-tokens will be our code-length
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

        RECURSIVE SUBROUTINE REC_PARSE(token_array, code_length, code, replace_this, with_this)
            IMPLICIT NONE
            INTEGER, DIMENSION(0:100) :: code
            INTEGER :: code_length, n, stat, output_int, colon_index
            CHARACTER (LEN = 10), dimension(0:100) :: token_array
            CHARACTER (LEN = 10) replace_this, with_this, token, left, right, temp_str
            
            ! Loop through all our tokens
            DO n = 0, (code_length - 1), 1
                token = ADJUSTL(TRIM(token_array(n)))
                
                ! Check for tokens beginning with '?'
                IF (INDEX(token, '?') == 1) THEN
                    token = token(2:)
                    ! Get the string after the '?' symbol, and attempt to convert it to an integer
                    CALL STR_TO_INT(token, output_int, stat)
                                        
                    IF (STAT /= 0) THEN
                        ! If value after '?' wasn't an integer, produce error message and bail
                        PRINT *, 'ERROR: Unable to parse'
                        WRITE (*, '(A)') token_array(n)
                        STOP
                    ELSE
                        ! Otherwise, update the token with the current index adjusted to our '?' value
                        output_int = output_int + n
                        WRITE (token_array(n), '(i10)') output_int
                        token_array(n) = ADJUSTL(TRIM(token_array(n)))
                    END IF
                ELSE IF (token == replace_this) THEN
                    ! If we've reached a symbol we need to replace, do so
                    WRITE (token_array(n), '(a)') with_this
                ELSE
                    ! If we have a ':' in our token..
                    colon_index = INDEX(token, ':')
                    IF (colon_index > 0) THEN        
                        ! Split the token to get the left and right-hand sides                    
                        left = ADJUSTL(TRIM(token(:colon_index - 1)))
                        right = ADJUSTL(TRIM(token(colon_index + 1:)))
                        
                        ! Write our current index to temp_str
                        WRITE (temp_str, '(i10)') n          

                        ! Check if right-hand-side is an integer                        
                        CALL STR_TO_INT(right, output_int, stat)
                        IF (STAT == 0) THEN
                            ! Update and recurse
                            WRITE (token_array(n), '(a)') right
                            CALL REC_PARSE(token_array, code_length, code, left, ADJUSTL(TRIM(temp_str)))                        
                        ELSE IF (left == right) THEN
                            ! Update and recurse
                            WRITE (token_array(n), '(a)') ADJUSTL(TRIM(temp_str))
                            CALL REC_PARSE(token_array, code_length, code, left, ADJUSTL(TRIM(temp_str)))                        
                        ELSE
                            ! Update and recurse
                            WRITE (token_array(n), '(a)') right
                            CALL REC_PARSE(token_array, code_length, code, left, ADJUSTL(TRIM(temp_str)))                        
                        END IF
                    END IF
                END IF                                
            END DO            
        END SUBROUTINE REC_PARSE
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! PARSE()
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        SUBROUTINE PARSE(token_array, code_length, code)
            IMPLICIT NONE
            INTEGER, DIMENSION(0:100) :: code
            INTEGER :: code_length, n, output_int, stat
            CHARACTER (LEN = 10), dimension(0:100) :: token_array
            CHARACTER (LEN = 10) token
            
            ! Parse our tokens into an integer-array
            CALL REC_PARSE(token_array, code_length, code, '', '')        

            ! Check all values in our token list are integers
            DO n = 0, (code_length - 1), 1
                token = ADJUSTL(TRIM(token_array(n)))
                
                CALL STR_TO_INT(token, output_int, stat)
                IF (STAT == 0) THEN
                    ! Since it's an integer, assign it to our code-array
                    code(n) = output_int
                ELSE
                    ! If it's not, then something went wrong, so bail
                    PRINT *, 'ERROR: Unable to parse'
                    WRITE (*, '(A)') token
                    STOP
                END IF
            END DO                        
        END SUBROUTINE PARSE

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! EXECUTE_CODE()
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        SUBROUTINE EXECUTE_CODE(code, code_length)
            IMPLICIT NONE
            INTEGER, DIMENSION(0:100) :: code
            INTEGER code_length, code_index, a, b, c
            CHARACTER(1) :: output_char, input_char
            
            ! For each item in our code
            code_index = 0
            DO WHILE (code_index >= 0)
                ! First check that we can get the 3 values for a, b, and c
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
                    ! If a < 0 then input (to-be-implemented...)
                    PRINT *, 'ERROR: Input not yet supported'
                    STOP                
                ELSE IF (b < 0) THEN
                    ! If b < 0 then output the value at 'a'
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
                    ! First check 'a' and 'b' are in range...
                    IF ((a < 0) .OR. (b < 0)) THEN
                        PRINT *, 'ERROR: Memory under-run'
                        STOP
                    ELSE IF ((a > code_length) .OR. (b > code_length)) THEN
                        PRINT *, 'ERROR: Memory over-run'
                        STOP
                    ELSE
                        ! ..and if they are, subtract-and-jump-if-less-than-or-equal
                        code(b) = code(b) - code(a)
                        IF (code(b) <= 0) THEN
                            code_index = c
                        END IF
                    END IF
                END IF                
            END DO
            
            PRINT *, ''
            PRINT *, 'Program complete!'
        END SUBROUTINE EXECUTE_CODE
    
END PROGRAM SUBLEQ

