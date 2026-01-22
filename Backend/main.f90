PROGRAM Analizador
     use error
     use token
     use a_texto
     use botones
     use check
     use clave
     use contenedor
     use etiqueta
     use radio_btn
     use texto
     use Salida 
     

IMPLICIT NONE
    integer :: len, linea, columna, estado, puntero, ios, numErrores
    character(len=50000) :: contenido, buffer
    character(len=1) :: char
    character(len=100) :: aux_tkn
    character(len=1), dimension(9) :: S 
    character(len=1), dimension(26) :: A 
    character(len=1), dimension(26) :: M
    
    contenido = ""
    do
        read(*, '(A)', IOSTAT=ios) buffer
        if (ios /= 0) exit 
        contenido = trim(contenido) // trim(buffer) // new_line('a') ! concatenamos el 
        !contenido mas lo que viene en el buffer y como leemos por el salto de linea al final
    end do
   
    A = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']
    M = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']
    S = [';','-','.','(',')',',','<','>','!']
    estado = 0
    puntero = 1
    columna = 0
    linea = 1
    aux_tkn = ""
    numErrores = 0

    len = len_trim(contenido)
    
    do while(puntero <= len)
        char = contenido(puntero:puntero)
        select case (estado)
        case (0)
            if(ichar(char) == 47)then
                estado = 7
                columna = columna + 1
            elseif(any(char == S))then
                estado = 1
                columna = columna + 1
            elseif(any(char == A) .OR. any(char == M))then
                estado = 2
            elseif(char >= '0' .AND. char <= '9')then
                estado = 3
                elseif (char == '"')  then
                    aux_tkn = trim(aux_tkn) // char
                    columna = columna + 1
                    puntero = puntero + 1 
                    estado = 4       
                elseif (ichar(char) == 10) then ! Actualizo la posicion
                    ! Salto de línea
                    columna = 0
                    linea = linea + 1
                    puntero = puntero + 1
                elseif (ichar(char) == 9) then
                    ! Tabulación
                    columna = columna + 4
                    puntero = puntero + 1
                elseif (ichar(char) == 32) then
                    ! Espacio en blanco
                    columna = columna + 1
                    puntero = puntero + 1  
                else
                    ! Reporta un error si el carácter no es válido
                    CALL agregar_error_lex(char, 'Error Lexico', linea, columna)
                    columna = columna + 1
                    puntero = puntero + 1
                    estado = 0
            end if
        case(1)
            if ( char == ';' ) then
                call agregar_token(char, 'tk_pyc', linea, columna)
                
            elseif ( char == '.' ) then
                call agregar_token(char, 'tk_punto', linea, columna)

            elseif ( char == ',' ) then
                call agregar_token(char, 'tk_coma', linea, columna)

            elseif ( char == '>') then
                call agregar_token(char, 'tk_mayor', linea, columna)

            elseif ( char == '<') then
                call agregar_token(char, 'tk_menor', linea, columna)

            elseif ( char == '(') then
                call agregar_token(char, 'tk_par_izq', linea, columna)

            elseif ( char == ')') then
                call agregar_token(char, 'tk_par_der', linea, columna)         
            
            elseif ( char == '-') then
                call agregar_token(char, 'tk_guion', linea, columna)
            
            elseif ( char == '!') then
                call agregar_token(char, 'tk_exp', linea, columna) 
            end if
            puntero = puntero + 1
            estado = 0
        case(2)
            if (any(char == A) .OR. any(char == M) .OR. any(char == ['0','1','2','3','4','5','6','7','8','9'])) then
                aux_tkn = trim(aux_tkn) // char
                columna = columna + 1
                puntero = puntero + 1
            else
                if(aux_tkn == 'Controles')then
                    call agregar_token(aux_tkn, 'tk_controles', linea, columna)
                elseif(aux_tkn == 'propiedades')then
                    call agregar_token(aux_tkn, 'tk_propiedades', linea, columna)
                elseif(aux_tkn == 'Colocacion')then
                    call agregar_token(aux_tkn, 'tk_colocacion', linea, columna)
                elseif(aux_tkn == 'Contenedor')then
                    call agregar_token(aux_tkn, 'tk_contenedor', linea, columna)
                elseif(aux_tkn == 'Etiqueta')then
                    call agregar_token(aux_tkn, 'tk_etiqueta', linea, columna)
                elseif(aux_tkn == 'Texto')then
                    call agregar_token(aux_tkn, 'tk_texto', linea, columna)
                elseif(aux_tkn == 'Boton')then
                    call agregar_token(aux_tkn, 'tk_boton', linea, columna)
                elseif(aux_tkn == 'AreaTexto')then
                    call agregar_token(aux_tkn, 'tk_areaTexto', linea, columna)
                elseif(aux_tkn == 'Clave')then
                    call agregar_token(aux_tkn, 'tk_clave', linea, columna)
                elseif(aux_tkn == 'RadioBoton')then
                    call agregar_token(aux_tkn, 'tk_radiobtn', linea, columna)
                elseif(aux_tkn == 'Check')then
                    call agregar_token(aux_tkn, 'tk_check', linea, columna)
                elseif(aux_tkn == 'setAncho')then
                    call agregar_token(aux_tkn, 'tk_setAncho', linea, columna)
                elseif(aux_tkn == 'setAlto')then
                    call agregar_token(aux_tkn, 'tk_setAlto', linea, columna)
                elseif(aux_tkn == 'setColorFondo')then
                    call agregar_token(aux_tkn, 'tk_setColorFondo', linea, columna)
                elseif(aux_tkn == 'setColorLetra')then
                    call agregar_token(aux_tkn, 'tk_setColorLetra', linea, columna)
                elseif(aux_tkn == 'setTexto')then
                    call agregar_token(aux_tkn, 'tk_setTexto', linea, columna)
                elseif(aux_tkn == 'setPosicion')then
                    call agregar_token(aux_tkn, 'tk_setPosicion', linea, columna)
                elseif(aux_tkn == 'setMarcada')then
                    call agregar_token(aux_tkn, 'tk_setMarcada', linea, columna)
                elseif(aux_tkn == 'this')then
                    call agregar_token(aux_tkn, 'tk_this', linea, columna)
                elseif(aux_tkn == 'add')then
                    call agregar_token(aux_tkn, 'tk_add', linea, columna)
                else
                    call agregar_token(aux_tkn, 'tk_id', linea, columna)
                end if
                aux_tkn = ""
                estado = 0
            end if
        case(3)
            if(char >= '0' .AND. char <='9')then
                aux_tkn = trim(aux_tkn) // char
                columna = columna + 1
                puntero = puntero + 1
            else
                call agregar_token(aux_tkn, 'tk_num', linea, columna)
                aux_tkn = ""
                estado = 0
            end if
        case(4)
           if(ichar(char)>= 0 .AND. ichar(char)<= 255 &
           .AND. char .NE. '"')then
           aux_tkn = trim(aux_tkn) // char
           columna = columna + 1    
           puntero = puntero + 1
           estado = 6
        
           elseif(char == '"')then
            estado = 5
           else
            call agregar_error_lex(aux_tkn, 'Error Lexico', linea, columna)
            aux_tkn = ""
            estado = 0
            end if
        case(5)
            aux_tkn = trim(aux_tkn) // char
            columna = columna + 1
            puntero = puntero + 1
            
            call agregar_token(aux_tkn, 'tk_cadena', linea, columna)
            
            aux_tkn = ""
            estado = 0

        case(6)
            if((any(char == A) .OR. any(char == M) .OR. any(char == S))&
            .AND. char .NE. '"')then
            aux_tkn = trim(aux_tkn) // char
            columna = columna + 1
            puntero = puntero + 1
            elseif(char == '"')then
                estado = 5
            else 
                call agregar_error_lex(aux_tkn, 'Error Lexico', linea, columna)
                aux_tkn = ""
                estado = 0
            end if
        case(7) 
            if(ichar(char)==47)then
                estado = 7
                puntero = puntero + 1
                columna = columna + 1
            elseif(ichar(char)==10)then
                estado = 0
                puntero = puntero + 1
                columna = columna + 1
                linea = linea + 1
            elseif(ichar(char)==42)then
                estado = 8
                puntero = puntero + 1
                columna = columna + 1
            else 
                estado = 7
                puntero = puntero + 1
                columna = columna + 1
            end if
        case(8)
            if(ichar(char)==42)then
                estado = 9
                puntero = puntero + 1
                columna = columna + 1
            elseif(ichar(char)==10)then
                linea = linea + 1
                puntero = puntero + 1
                columna = 0
                estado = 8 
            else
                puntero = puntero + 1
                columna = columna + 1
                estado = 8
            end if
        case (9)
            if(ichar(char)==47)then
                estado = 0
                puntero = puntero + 1
                columna = columna + 1
            else
                 call agregar_error_lex(char, 'tk_barra', linea, columna)
                 estado = 0
            end if
        end select
    end do
    call parser
END PROGRAM Analizador