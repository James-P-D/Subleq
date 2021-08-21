PROGRAM SUBLEQ
  ! Forces you to declare all variables
  IMPLICIT NONE
  CHARACTER*100 :: input_filename
  INTEGER, DIMENSION(0:100) :: code
  INTEGER :: code_length
  
  IF(COMMAND_ARGUMENT_COUNT().NE.1) THEN
    PRINT *, 'Expected input file'
    STOP
  ENDIF
  
  CALL GET_COMMAND_ARGUMENT(1, input_filename) 
  
  ! CALL PARSE_FILE(input_filename)

  code_length = 11
  code(0) = 9
  code(1) = -1
  code(2) = 3
  code(3) = 10
  code(4) =  -1
  code(5) =  6
  code(6) =  0
  code(7) =  0
  code(8) =  -1
  code(9) =  72
  code(10) =  105
  
  CALL EXECUTE_CODE(code, code_length)
  
  CONTAINS
    SUBROUTINE PARSE_FILE(filename)
      IMPLICIT NONE
      CHARACTER (LEN = 100) :: filename
      CHARACTER (LEN = 100) :: line_read
      CHARACTER (LEN = 100) :: token
      
      INTEGER :: error, comment_index, space_index, data_array_index
      
      CHARACTER (LEN = 1000), dimension(10)  :: string_array
      INTEGER, DIMENSION(0:1000) :: data_array
      
      PRINT * , "Hello ", filename
      
      data_array_index = 0
      OPEN(11, file=input_filename, status='old')
      DO
        READ (11, '(A)', IOSTAT = error) line_read
        IF (error > 0) THEN
          EXIT
        ELSE
          WRITE (*, '(A)') TRIM(line_read)
          
          ! Remove any comments (any text following a '#' symbol)
          comment_index = INDEX(line_read, '#')
          IF (comment_index > 0) THEN
            WRITE (*, '(A)') TRIM(line_read(:comment_index - 1))
            line_read = line_read(:comment_index - 1)
          END IF
          line_read = TRIM(line_read)
          
          WRITE (*, '(A)') line_read
          
          DO WHILE (LEN(TRIM(line_read)) > 0)
            space_index = INDEX(line_read, ' ')
            IF (space_index > 0) THEN
              token = TRIM(line_read(:space_index - 1))
              line_read = TRIM(line_read(space_index + 1:))              
            ELSE
              token = TRIM(line_read)
            END IF            
            
            IF (LEN(token) > 0) THEN
              WRITE (*, '(A)') token
            ELSE
              EXIT
            END IF
          END DO
        END IF
      END DO
      ! Either KEEP or DELETE file when closed
      CLOSE(11, status="KEEP")
    END SUBROUTINE PARSE_FILE

    SUBROUTINE EXECUTE_CODE(code, code_length)
      IMPLICIT NONE
      INTEGER, DIMENSION(0:100) :: code
      INTEGER code_length, code_index, a, b, c
      CHARACTER(1) :: output_char, input_char
      
      code_index = 0
      DO WHILE (code_index >= 0)
        IF (code_index + 2 > code_length) THEN
          PRINT *, 'Code over-run'
          STOP
        END IF
        a = code(code_index)
        code_index = code_index + 1
        b = code(code_index)
        code_index = code_index + 1
        c = code(code_index)
        code_index = code_index + 1
        IF (a < 0) THEN
          PRINT *, 'Input not yet supported'
          STOP        
        ELSE IF (b < 0) THEN
          IF (a < 0) THEN
            PRINT *, 'Memory under-run when outputting'
            STOP
          ELSE IF (a > code_length) THEN
            PRINT *, 'Memory over-run when outputting'
            STOP
          ELSE
            output_char = char(code(a))
            write(*, "(2a)", advance="no") output_char
          END IF
        ELSE
          IF ((a < 0) .OR. (b < 0)) THEN
            PRINT *, 'Memory under-run'
            STOP
          ELSE IF ((a > code_length) .OR. (b > code_length)) THEN
            PRINT *, 'Memory over-run'
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

