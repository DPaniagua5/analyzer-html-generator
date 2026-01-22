MODULE error
IMPLICIT NONE


    type :: Err
        character(len=100) :: ultimo_token
        character(len=100) :: token_esperado
        integer :: linea
        integer :: columna
    end type Err

    type(Err), ALLOCATABLE :: error_array(:)

    contains
        
    subroutine agregar_error(ultimo_token, token_esperado, linea, columna)
        character(len=*), intent(in) :: ultimo_token
        character(len=*), intent(in) :: token_esperado
        integer :: linea
        integer :: columna
        type(Err) :: nuevo_error
        integer :: n, i
        type(Err), ALLOCATABLE :: temp_array(:)

        nuevo_error%ultimo_token = ultimo_token
        nuevo_error%token_esperado = token_esperado
        nuevo_error%linea = linea
        nuevo_error%columna = columna

            if(.NOT. ALLOCATED(error_array))then
                ALLOCATE(error_array(1))
                error_array(1) = nuevo_error
            else
                n = size(error_array)
                ALLOCATE(temp_array(n+1))
                temp_array(:n) = error_array
                temp_array(n+1) = nuevo_error
                deallocate(error_array)
                ALLOCATE(error_array(n+1))
                error_array = temp_array
        end if
    end subroutine agregar_error

    subroutine agregar_error_lex(ultimo_token, Descripcion, linea, columna)
        CHARACTER(LEN=*), INTENT(IN) :: ultimo_token
        CHARACTER(LEN=*), INTENT(IN) :: Descripcion
        integer :: linea
        integer :: columna
        type(Err) :: nuevo_error
        integer :: n
        integer :: T_Errores
        type(Err), ALLOCATABLE ::  temp_array(:)
        
        !Inicializo los datos del nuevo error
        
        nuevo_error%ultimo_token = ultimo_token
        nuevo_error%token_esperado = Descripcion
        nuevo_error%linea = linea
        nuevo_error%columna = columna
        ! Agrego el nuevo error a la lista de errores
        if (.NOT. ALLOCATED(error_array)) then !Si esta vacia
            ALLOCATE(error_array(1)) ! Se le asigna memoria para un error de la lista
            error_array(1) =  nuevo_error !Se convierte en el error nuevo
        else
            n = size(error_array)
            ALLOCATE(temp_array(n+1))
            temp_array(:n) = error_array !Reservo memoria
            temp_array(n+1) = nuevo_error
            DEALLOCATE(error_array) !Libero memoria
            ALLOCATE(error_array(n+1)) !Reservo memoria de nuevo
            error_array = temp_array
        end if
    end subroutine agregar_error_lex

    subroutine imprimir_errores()
        integer :: i
        CHARACTER(len=20) :: str_linea, str_columna, tk_aux
    
        do i = 1, size(error_array)
            tk_aux = ''
                !write(str_error, '(A)') trim(error_array(i)%ultimo_token)
                !write(str_esperado, '(A)') error_array(i)%token_esperado
                write(str_linea, '(I0)')  error_array(i)%linea
                write(str_columna, '(I0)') error_array(i)%columna
         

                if ( TRIM(error_array(i)%token_esperado) &
                == 'tk_pyc' ) then
                    WRITE(*,*) TRIM('ErrorSintactico')," ",trim(str_linea)," ", &
                    TRIM(str_columna)," ",TRIM(error_array(i)%ultimo_token)," ", &
                    TRIM('SeEsperabaElToken";"')
                elseif ( TRIM(error_array(i)%token_esperado) &
                     == 'tk_punto' ) then
                    tk_aux = '.'
                    WRITE(*,*) TRIM('ErrorSintactico')," ",trim(str_linea)," ", &
                    TRIM(str_columna)," ",TRIM(error_array(i)%ultimo_token)," ", &
                    TRIM('SeEsperabaElToken"'), TRIM(tk_aux), TRIM('"') 
                    tk_aux = ''
                elseif ( TRIM(error_array(i)%token_esperado) &
                    == 'tk_coma' ) then
                    tk_aux = ','
                    WRITE(*,*) TRIM('ErrorSintactico')," ",trim(str_linea)," ", &
                    TRIM(str_columna)," ",TRIM(error_array(i)%ultimo_token)," ", &
                    TRIM('SeEsperabaElToken"'), TRIM(tk_aux), TRIM('"') 
                    tk_aux = ''
    
                elseif ( TRIM(error_array(i)%token_esperado) &
                    == 'tk_mayor') then
                    tk_aux = '>'
                    WRITE(*,*) TRIM('ErrorSintactico')," ",trim(str_linea)," ", &
                    TRIM(str_columna)," ",TRIM(error_array(i)%ultimo_token)," ",&
                    TRIM('SeEsperabaElToken"'), TRIM(tk_aux), TRIM('"') 
                    tk_aux = ''
                elseif ( TRIM(error_array(i)%token_esperado)&
                     == 'tk_menor') then
                    tk_aux = '<'
                    WRITE(*,*) TRIM('ErrorSintactico')," ",trim(str_linea)," ", &
                    TRIM(str_columna)," ",TRIM(error_array(i)%ultimo_token)," ", &
                    TRIM('SeEsperabaElToken"'), TRIM(tk_aux), TRIM('"') 
                    tk_aux = ''
                elseif ( TRIM(error_array(i)%token_esperado)&
                    == 'tk_par_izq') then
                    tk_aux = '('
                    WRITE(*,*) TRIM('ErrorSintactico')," ",trim(str_linea)," ", &
                    TRIM(str_columna)," ",TRIM(error_array(i)%ultimo_token)," ", &
                    TRIM('SeEsperabaElToken"'), TRIM(tk_aux), TRIM('"') 
                    tk_aux = ''
                elseif ( TRIM(error_array(i)%token_esperado) &
                    == 'tk_par_der') then
                    tk_aux = ')'
                    WRITE(*,*) TRIM('ErrorSintactico')," ",trim(str_linea)," ", &
                    TRIM(str_columna)," ",TRIM(error_array(i)%ultimo_token)," ", &
                    TRIM('SeEsperabaElToken"'), TRIM(tk_aux), TRIM('"') 
                    tk_aux = ''
                elseif ( TRIM(error_array(i)%token_esperado) &
                    == 'tk_guion') then
                    tk_aux = '-'
                    WRITE(*,*) TRIM('ErrorSintactico')," ",trim(str_linea)," ", &
                    TRIM(str_columna)," ",TRIM(error_array(i)%ultimo_token)," ", &
                    TRIM('SeEsperabaElToken"'), TRIM(tk_aux), TRIM('"') 
                    tk_aux = ''
                elseif ( TRIM(error_array(i)%token_esperado) &
                    == 'tk_exp') then
                    tk_aux = '!'
                    WRITE(*,*) TRIM('ErrorSintactico')," ",trim(str_linea)," ", &
                    TRIM(str_columna)," ",TRIM(error_array(i)%ultimo_token)," ", &
                    TRIM('SeEsperabaElToken"'), TRIM(tk_aux), TRIM('"') 
                    tk_aux = ''  
                elseif ( TRIM(error_array(i)%token_esperado) &
                    == 'tk_barra') then
                    tk_aux = '/'
                    WRITE(*,*) TRIM('ErrorSintactico')," ",trim(str_linea)," ", &
                    TRIM(str_columna)," ",TRIM(error_array(i)%ultimo_token)," ", &
                    TRIM('SeEsperabaElToken"'), TRIM(tk_aux), TRIM('"')
                    tk_aux = ''
                elseif(error_array(i)%token_esperado == &
                    'Error Lexico')then
                     write(*,*) TRIM("ErrorLexico")," ",trim(str_linea) ," ", &
                     TRIM(str_columna)," ",TRIM(error_array(i)%ultimo_token)," ",&
                     "ElToken",TRIM(error_array(i)%ultimo_token), "NoPerteneceAlLenguaje."  
                else
                    write(*,*) TRIM('ErrorSintactico')," ",trim(str_linea)," ", &
                    TRIM(str_columna)," ",TRIM(error_array(i)%ultimo_token)," ", &
                    TRIM('"'), TRIM(error_array(i)%token_esperado), &
                    TRIM('"')
                    end if    
        end do
        
    end subroutine imprimir_errores

END MODULE error