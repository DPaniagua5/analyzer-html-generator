MODULE token
    use error
    use etiqueta
    use contenedor
    use texto
    use botones
    use a_texto
    use clave
    use check
    use radio_btn
    use Salida

IMPLICIT NONE
    type    :: Tkn
        CHARACTER(LEN = 100) :: Lexema
        CHARACTER(LEN = 200) :: Tipo
        integer :: Linea
        integer :: Columna
    End type Tkn

    type (Tkn), ALLOCATABLE :: token_array(:)

contains

subroutine agregar_token(lexema, tipo, linea, columna)
    CHARACTER(LEN =*), INTENT(IN) :: lexema
    CHARACTER(LEN =*), INTENT(IN) :: tipo
    integer :: linea
    integer :: columna
    type(Tkn) :: nuevo_token
    integer :: N
    type(Tkn), ALLOCATABLE :: temp_array(:)

    !Datos del nuevo token
    nuevo_token%Lexema = lexema
    nuevo_token%Tipo = tipo
    nuevo_token%Linea = linea
    nuevo_token%Columna = columna

    !Agregar el nuevo token a la lista de tokens

    if(.NOT. ALLOCATED(token_array))then
        ALLOCATE(token_array(1))
        token_array(1) = nuevo_token
    else    
        n = size(token_array)
        ALLOCATE(temp_array(n+1))
        temp_array(:n) = token_array
        temp_array(n+1) = nuevo_token
        DEALLOCATE(token_array)
        ALLOCATE(token_array(n+1))
        token_array = temp_array
    end if
end subroutine agregar_token

subroutine parser()
    integer :: i, numErrores
        do i=1, size(token_array)
            !Inicia Etiqueta
            if(token_array(i)%tipo == 'tk_etiqueta')then
                if(token_array(i+1)%tipo == 'tk_id')then
                    if(token_array(i+2)%tipo == 'tk_pyc')then
                        call agregar_etiqueta(token_array(i+1)%lexema)
                    else
                        call agregar_error(token_array(i+2)%lexema, 'tk_pyc'&
                        , token_array(i+2)%linea, token_array(i+2)%columna)
                    end if 
                else
                    call agregar_error(token_array(i+1)%lexema, 'tk_id'&
                    ,token_array(i+1)%linea, token_array(i+1)%columna)
                end if
            elseif(token_array(i)%tipo == 'tk_contenedor')then
                if(token_array(i+1)%tipo == 'tk_id')then
                    if(token_array(i+2)%tipo == 'tk_pyc')then
                    call agregar_contendor(token_array(i+1)%lexema)
                else
                    call agregar_error(token_array(i+2)%lexema, 'tk_pyc', token_array(i+2)%linea, token_array(i+2)%columna)
                end if
            else
                call agregar_error(token_array(i+1)%lexema, 'tk_id', token_array(i+1)%linea, token_array(i+1)%columna) 
            end if
        elseif(token_array(i)%tipo == 'tk_texto')then
            if(token_array(i+1)%tipo == 'tk_id')then
                if(token_array(i+2)%tipo == 'tk_pyc')then
                call agregar_texto(token_array(i+1)%lexema)
            else
                call agregar_error(token_array(i+2)%lexema, 'tk_pyc', token_array(i+2)%linea, token_array(i+2)%columna)
            end if
        else
            call agregar_error(token_array(i+1)%lexema, 'tk_id', token_array(i+1)%linea, token_array(i+1)%columna) 
        endif
    elseif (token_array(i)%tipo == 'tk_boton')then
        if(token_array(i+1)%tipo == 'tk_id')then
            if(token_array(i+2)%tipo == 'tk_pyc')then
            call agregar_boton(token_array(i+1)%lexema)
        else
            call agregar_error(token_array(i+2)%lexema, 'tk_pyc', token_array(i+2)%linea, token_array(i+2)%columna)
        end if
    else
        call agregar_error(token_array(i+1)%lexema, 'tk_id', token_array(i+1)%linea, token_array(i+1)%columna)
    endif
elseif (token_array(i)%tipo == 'tk_areaTexto')then
    if(token_array(i+1)%tipo == 'tk_id')then
    if(token_array(i+2)%tipo == 'tk_pyc')then
        call agregar_a_texto(token_array(i+1)%lexema)
    else
        call agregar_error(token_array(i+2)%lexema, 'tk_pyc', token_array(i+2)%linea, token_array(i+2)%columna)
    end if
else
    call agregar_error(token_array(i+1)%lexema, 'tk_id', token_array(i+1)%linea, token_array(i+1)%columna)
endif
    elseif(token_array(i)%tipo == 'tk_clave')then
        if(token_array(i+1)%tipo == 'tk_id')then
        if(token_array(i+2)%tipo == 'tk_pyc')then
            call agregar_clave(token_array(i+1)%lexema)
        else
            call agregar_error(token_array(i+2)%lexema, 'tk_pyc', token_array(i+2)%linea, token_array(i+2)%columna)
        end if 
    else
        call agregar_error(token_array(i+1)%lexema, 'tk_id', token_array(i+1)%linea, token_array(i+1)%columna)
    end if
    elseif(token_array(i)%tipo == 'tk_check')then
        if(token_array(i+1)%tipo == 'tk_id')then
        if(token_array(i+2)%tipo == 'tk_pyc')then
            call agregar_check(token_array(i+1)%lexema)
        else
            call agregar_error(token_array(i+2)%lexema, 'tk_pyc', token_array(i+2)%linea, token_array(i+2)%columna)
        end if 
    else
        call agregar_error(token_array(i+1)%lexema, 'tk_id', token_array(i+1)%linea, token_array(i+1)%columna)
    end if
    elseif(token_array(i)%tipo == 'tk_radiobtn')then
    if(token_array(i+1)%tipo == 'tk_id')then
    if(token_array(i+2)%tipo == 'tk_pyc')then
        call agregar_radio(token_array(i+1)%lexema)
    else
        call agregar_error(token_array(i+2)%lexema, 'tk_pyc', token_array(i+2)%linea, token_array(i+2)%columna)
    end if 
else
    call agregar_error(token_array(i+1)%lexema, 'tk_id', token_array(i+1)%linea, token_array(i+1)%columna)
end if
        end if

            if (token_array(i)%tipo == 'tk_id' .and. token_array(i+1)%tipo == 'tk_punto' ) then
                if(token_array(i+2)%tipo == 'tk_setAncho') then
                    if (token_array(i+3)%tipo .ne. 'tk_par_izq')then
                        call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                    elseif(token_array(i+4)%tipo .NE. 'tk_num')then
                        call agregar_error(token_array(i+4)%lexema, 'tk_num', token_array(i+4)%linea, token_array(i+4)%columna)
                    elseif(token_array(i+5)%tipo .NE. 'tk_par_der')then
                        call agregar_error(token_array(i+5)%lexema, 'tk_par_der', token_array(i+5)%linea, token_array(i+5)%columna)
                    elseif(token_array(i+6)%tipo .NE. 'tk_pyc')then
                        call agregar_error(token_array(i+6)%lexema, 'tk_pyc', token_array(i+6)%linea, token_array(i+6)%columna)        
                    else
                        call etiqueta_set_Ancho(token_array(i)%lexema, token_array(i+4)%lexema)
                    end if
                end if
            
            if(token_array(i+2)%tipo == 'tk_setAlto')then
                if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                    call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                else if(token_array(i+4)%tipo .NE. 'tk_num')then
                    call agregar_error(token_array(i+4)%lexema, 'tk_num', token_array(i+4)%linea, token_array(i+4)%columna)
                else if(token_array(i+5)%tipo .NE. 'tk_par_der')then
                    call agregar_error(token_array(i+5)%lexema, 'tk_par_der', token_array(i+5)%linea, token_array(i+5)%columna)
                else if(token_array(i+6)%tipo .NE. 'tk_pyc')then
                    call agregar_error(token_array(i+6)%lexema, 'tk_pyc', token_array(i+6)%linea, token_array(i+6)%columna)
                else
                    call etiqueta_set_Alto(token_array(i)%lexema, token_array(i+4)%lexema)
                end if
            end if
            
            if(token_array(i+2)%tipo == 'tk_setTexto')then
                if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                    call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                elseif(token_array(i+4)%tipo .NE. 'tk_cadena')then
                    call agregar_error(token_array(i+4)%lexema, 'tk_cadena', token_array(i+4)%linea, token_array(i+4)%columna)
                elseif(token_array (i+5)%tipo .NE. 'tk_par_der')then
                    call agregar_error(token_array(i+5)%lexema, 'tk_par_der', token_array(i+5)%linea, token_array(i+5)%columna)    
                elseif(token_array(i+6)%tipo .NE. 'tk_pyc')then
                    call agregar_error(token_array(i+6)%lexema, 'tk_pyc', token_array(i+6)%linea, token_array(i+6)%columna)
                else
                    call etiqueta_set_Texto(token_array(i)%lexema, token_array(i+4)%lexema)
                end if
            endif

            if(token_array(i+2)%tipo == 'tk_setColorLetra')then
                if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                    call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                elseif(token_array(i+4)%tipo .NE. 'tk_num')then
                    call agregar_error(token_array(i+4)%lexema, 'tk_num', token_array(i+4)%linea, token_array(i+4)%columna)
                elseif(token_array(i+5)%tipo .NE. 'tk_coma')then
                    call agregar_error(token_array(i+5)%lexema, 'tk_coma', token_array(i+5)%linea, token_array(i+5)%columna)
                elseif(token_array(i+6)%tipo .NE. 'tk_num')then
                    call agregar_error(token_array(i+6)%lexema, 'tk_num', token_array(i+6)%linea, token_array(i+6)%columna)
                elseif(token_array(i+7)%tipo .NE. 'tk_coma')then
                    call agregar_error(token_array(i+7)%lexema, 'tk_coma', token_array(i+7)%linea, token_array(i+7)%columna)
                elseif(token_array(i+8)%tipo .NE. 'tk_num')then
                    call agregar_error(token_array(i+8)%lexema, 'tk_num', token_array(i+8)%linea, token_array(i+8)%columna)
                elseif(token_array(i+9)%tipo .NE. 'tk_par_der')then
                    call agregar_error(token_array(i+9)%lexema, 'tk_par_der', token_array(i+9)%linea, token_array(i+9)%columna)
                elseif(token_array(i+10)%tipo .NE. 'tk_pyc')then
                    call agregar_error(token_array(i+10)%lexema, 'tk_pyc', token_array(i+10)%linea, token_array(i+10)%columna)
                else
                    call etiqueta_set_color_texto(token_array(i)%lexema, token_array(i+4)%lexema,&
                     token_array(i+6)%lexema, token_array(i+8)%lexema)
                end if 
                end if
        
            if(token_array(i+2)%tipo == 'tk_setPosicion')then
                if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                    call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                elseif(token_array(i+4)%tipo .NE. 'tk_num')then
                    call agregar_error(token_array(i+4)%lexema, 'tk_num', token_array(i+4)%linea, token_array(i+4)%columna)
                elseif(token_array(i+5)%tipo .NE. 'tk_coma')then
                    call agregar_error(token_array(i+5)%lexema, 'tk_coma', token_array(i+5)%linea, token_array(i+5)%columna)
                elseif(token_array(i+6)%tipo .NE. 'tk_num')then
                    call agregar_error(token_array(i+6)%lexema, 'tk_num', token_array(i+6)%linea, token_array(i+6)%columna)
                elseif(token_array(i+7)%tipo .NE. 'tk_par_der')then
                    call agregar_error(token_array(i+7)%lexema, 'tk_par_der', token_array(i+7)%linea, token_array(i+7)%columna)
                elseif(token_array(i+8)%tipo .NE. 'tk_pyc')then
                    call agregar_error(token_array(i+8)%lexema, 'tk_pyc', token_array(i+8)%linea, token_array(i+8)%columna)
                else
                    call etiqueta_set_posicion(token_array(i)%lexema, token_array(i+4)%lexema, token_array(i+6)%lexema)
                endif
            endif
            if(token_array(i+2)%tipo == 'tk_add')then
                if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                    call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                    elseif(token_array(i+4)%tipo .NE. 'tk_id')then
                        call agregar_error(token_array(i+4)%lexema, 'tk_id', token_array(i+4)%linea, token_array(i+4)%columna)
                    elseif(token_array (i+5)%tipo .NE. 'tk_par_der')then
                        call agregar_error(token_array(i+5)%lexema, 'tk_par_der', token_array(i+5)%linea, token_array(i+5)%columna)    
                    elseif(token_array(i+6)%tipo .NE. 'tk_pyc')then
                        call agregar_error(token_array(i+6)%lexema, 'tk_pyc', token_array(i+6)%linea, token_array(i+6)%columna)
                    else
                        call etiqueta_set_grupo(token_array(i+4)%lexema, token_array(i)%lexema)
                    endif
                endif

        endif

        !Inicia Contenedor
     

        if (token_array(i)%tipo == 'tk_id' .and. token_array(i+1)%tipo == 'tk_punto' ) then
            if(token_array(i+2)%tipo == 'tk_setAncho') then
                if (token_array(i+3)%tipo .ne. 'tk_par_izq')then
                    call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                elseif(token_array(i+4)%tipo .NE. 'tk_num')then
                    call agregar_error(token_array(i+4)%lexema, 'tk_num', token_array(i+4)%linea, token_array(i+4)%columna)
                elseif(token_array(i+5)%tipo .NE. 'tk_par_der')then
                    call agregar_error(token_array(i+5)%lexema, 'tk_par_der', token_array(i+5)%linea, token_array(i+5)%columna)
                elseif(token_array(i+6)%tipo .NE. 'tk_pyc')then
                    call agregar_error(token_array(i+6)%lexema, 'tk_pyc', token_array(i+6)%linea, token_array(i+6)%columna)        
                else
                    call contenedor_set_Ancho(token_array(i)%lexema, token_array(i+4)%lexema)
                end if
            end if
            if(token_array(i+2)%tipo == 'tk_setAlto')then
                if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                    call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                else if(token_array(i+4)%tipo .NE. 'tk_num')then
                    call agregar_error(token_array(i+4)%lexema, 'tk_num', token_array(i+4)%linea, token_array(i+4)%columna)
                else if(token_array(i+5)%tipo .NE. 'tk_par_der')then
                    call agregar_error(token_array(i+5)%lexema, 'tk_par_der', token_array(i+5)%linea, token_array(i+5)%columna)
                else if(token_array(i+6)%tipo .NE. 'tk_pyc')then
                    call agregar_error(token_array(i+6)%lexema, 'tk_pyc', token_array(i+6)%linea, token_array(i+6)%columna)
                else
                    call contenedor_set_Alto(token_array(i)%lexema, token_array(i+4)%lexema)
                end if
            end if
            if(token_array(i+2)%tipo == 'tk_setColorFondo')then
                if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                    call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                elseif(token_array(i+4)%tipo .NE. 'tk_num')then
                    call agregar_error(token_array(i+4)%lexema, 'tk_num', token_array(i+4)%linea, token_array(i+4)%columna)
                elseif(token_array(i+5)%tipo .NE. 'tk_coma')then
                    call agregar_error(token_array(i+5)%lexema, 'tk_coma', token_array(i+5)%linea, token_array(i+5)%columna)
                elseif(token_array(i+6)%tipo .NE. 'tk_num')then
                    call agregar_error(token_array(i+6)%lexema, 'tk_num', token_array(i+6)%linea, token_array(i+6)%columna)
                elseif(token_array(i+7)%tipo .NE. 'tk_coma')then
                    call agregar_error(token_array(i+7)%lexema, 'tk_coma', token_array(i+7)%linea, token_array(i+7)%columna)
                elseif(token_array(i+8)%tipo .NE. 'tk_num')then
                    call agregar_error(token_array(i+8)%lexema, 'tk_num', token_array(i+8)%linea, token_array(i+8)%columna)
                elseif(token_array(i+9)%tipo .NE. 'tk_par_der')then
                    call agregar_error(token_array(i+9)%lexema, 'tk_par_der', token_array(i+9)%linea, token_array(i+9)%columna)
                elseif(token_array(i+10)%tipo .NE. 'tk_pyc')then
                    call agregar_error(token_array(i+10)%lexema, 'tk_pyc', token_array(i+10)%linea, token_array(i+10)%columna)
                else
                    call contenedor_set_color_fondo(token_array(i)%lexema, token_array(i+4)%lexema,&
                     token_array(i+6)%lexema, token_array(i+8)%lexema)
                end if 
            end if
            if(token_array(i+2)%tipo == 'tk_setPosicion')then
                if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                    call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                elseif(token_array(i+4)%tipo .NE. 'tk_num')then
                    call agregar_error(token_array(i+4)%lexema, 'tk_num', token_array(i+4)%linea, token_array(i+4)%columna)
                elseif(token_array(i+5)%tipo .NE. 'tk_coma')then
                    call agregar_error(token_array(i+5)%lexema, 'tk_coma', token_array(i+5)%linea, token_array(i+5)%columna)
                elseif(token_array(i+6)%tipo .NE. 'tk_num')then
                    call agregar_error(token_array(i+6)%lexema, 'tk_num', token_array(i+6)%linea, token_array(i+6)%columna)
                elseif(token_array(i+7)%tipo .NE. 'tk_par_der')then
                    call agregar_error(token_array(i+7)%lexema, 'tk_par_der', token_array(i+7)%linea, token_array(i+7)%columna)
                elseif(token_array(i+8)%tipo .NE. 'tk_pyc')then
                    call agregar_error(token_array(i+8)%lexema, 'tk_pyc', token_array(i+8)%linea, token_array(i+8)%columna)
                else
                    call contenedor_set_posicion(token_array(i)%lexema, token_array(i+4)%lexema, token_array(i+6)%lexema)
                endif
            endif
            if(token_array(i+2)%tipo == 'tk_add')then
                if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                    call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                    elseif(token_array(i+4)%tipo .NE. 'tk_id')then
                        call agregar_error(token_array(i+4)%lexema, 'tk_id', token_array(i+4)%linea, token_array(i+4)%columna)
                    elseif(token_array (i+5)%tipo .NE. 'tk_par_der')then
                        call agregar_error(token_array(i+5)%lexema, 'tk_par_der', token_array(i+5)%linea, token_array(i+5)%columna)    
                    elseif(token_array(i+6)%tipo .NE. 'tk_pyc')then
                        call agregar_error(token_array(i+6)%lexema, 'tk_pyc', token_array(i+6)%linea, token_array(i+6)%columna)
                    else
                        call contenedor_set_grupo(token_array(i+4)%lexema, token_array(i)%lexema)
                    endif
                endif
        endif

        !Inicia Texto
        
        if (token_array(i)%tipo == 'tk_id' .and. token_array(i+1)%tipo == 'tk_punto' ) then
            if(token_array(i+2)%tipo == 'tk_setTexto')then
                if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                    call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                elseif(token_array(i+4)%tipo .NE. 'tk_cadena')then
                    call agregar_error(token_array(i+4)%lexema, 'tk_cadena', token_array(i+4)%linea, token_array(i+4)%columna)
                elseif(token_array (i+5)%tipo .NE. 'tk_par_der')then
                    call agregar_error(token_array(i+5)%lexema, 'tk_par_der', token_array(i+5)%linea, token_array(i+5)%columna)    
                elseif(token_array(i+6)%tipo .NE. 'tk_pyc')then
                    call agregar_error(token_array(i+6)%lexema, 'tk_pyc', token_array(i+6)%linea, token_array(i+6)%columna)
                else
                    call texto_set_texto(token_array(i)%lexema, token_array(i+4)%lexema)
                end if
            endif
            if(token_array(i+2)%tipo == 'tk_setPosicion')then
                if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                    call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                elseif(token_array(i+4)%tipo .NE. 'tk_num')then
                    call agregar_error(token_array(i+4)%lexema, 'tk_num', token_array(i+4)%linea, token_array(i+4)%columna)
                elseif(token_array(i+5)%tipo .NE. 'tk_coma')then
                    call agregar_error(token_array(i+5)%lexema, 'tk_coma', token_array(i+5)%linea, token_array(i+5)%columna)
                elseif(token_array(i+6)%tipo .NE. 'tk_num')then
                    call agregar_error(token_array(i+6)%lexema, 'tk_num', token_array(i+6)%linea, token_array(i+6)%columna)
                elseif(token_array(i+7)%tipo .NE. 'tk_par_der')then
                    call agregar_error(token_array(i+7)%lexema, 'tk_par_der', token_array(i+7)%linea, token_array(i+7)%columna)
                elseif(token_array(i+8)%tipo .NE. 'tk_pyc')then
                    call agregar_error(token_array(i+8)%lexema, 'tk_pyc', token_array(i+8)%linea, token_array(i+8)%columna)
                else
                    call texto_set_posicion(token_array(i)%lexema, token_array(i+4)%lexema, token_array(i+6)%lexema)
                endif
            endif
            if(token_array(i+2)%tipo == 'tk_add')then
                if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                    call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                    elseif(token_array(i+4)%tipo .NE. 'tk_id')then
                        call agregar_error(token_array(i+4)%lexema, 'tk_id', token_array(i+4)%linea, token_array(i+4)%columna)
                    elseif(token_array (i+5)%tipo .NE. 'tk_par_der')then
                        call agregar_error(token_array(i+5)%lexema, 'tk_par_der', token_array(i+5)%linea, token_array(i+5)%columna)    
                    elseif(token_array(i+6)%tipo .NE. 'tk_pyc')then
                        call agregar_error(token_array(i+6)%lexema, 'tk_pyc', token_array(i+6)%linea, token_array(i+6)%columna)
                    else
                        call texto_set_grupo(token_array(i+4)%lexema, token_array(i)%lexema)
                    endif
                endif
    endif

    !Inicia Boton
    
    if (token_array(i)%tipo == 'tk_id' .and. token_array(i+1)%tipo == 'tk_punto' ) then
        if(token_array(i+2)%tipo == 'tk_setTexto')then
            if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
            elseif(token_array(i+4)%tipo .NE. 'tk_cadena')then
                call agregar_error(token_array(i+4)%lexema, 'tk_cadena', token_array(i+4)%linea, token_array(i+4)%columna)
            elseif(token_array (i+5)%tipo .NE. 'tk_par_der')then
                call agregar_error(token_array(i+5)%lexema, 'tk_par_der', token_array(i+5)%linea, token_array(i+5)%columna)    
            elseif(token_array(i+6)%tipo .NE. 'tk_pyc')then
                call agregar_error(token_array(i+6)%lexema, 'tk_pyc', token_array(i+6)%linea, token_array(i+6)%columna)
            else
                call boton_set_texto(token_array(i)%lexema, token_array(i+4)%lexema)
            end if
        endif
        if(token_array(i+2)%tipo == 'tk_setPosicion')then
            if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
            elseif(token_array(i+4)%tipo .NE. 'tk_num')then
                call agregar_error(token_array(i+4)%lexema, 'tk_num', token_array(i+4)%linea, token_array(i+4)%columna)
            elseif(token_array(i+5)%tipo .NE. 'tk_coma')then
                call agregar_error(token_array(i+5)%lexema, 'tk_coma', token_array(i+5)%linea, token_array(i+5)%columna)
            elseif(token_array(i+6)%tipo .NE. 'tk_num')then
                call agregar_error(token_array(i+6)%lexema, 'tk_num', token_array(i+6)%linea, token_array(i+6)%columna)
            elseif(token_array(i+7)%tipo .NE. 'tk_par_der')then
                call agregar_error(token_array(i+7)%lexema, 'tk_par_der', token_array(i+7)%linea, token_array(i+7)%columna)
            elseif(token_array(i+8)%tipo .NE. 'tk_pyc')then
                call agregar_error(token_array(i+8)%lexema, 'tk_pyc', token_array(i+8)%linea, token_array(i+8)%columna)
            else
                call boton_set_posicion(token_array(i)%lexema, token_array(i+4)%lexema, token_array(i+6)%lexema)
            endif
        endif
        if(token_array(i+2)%tipo == 'tk_add')then
            if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                elseif(token_array(i+4)%tipo .NE. 'tk_id')then
                    call agregar_error(token_array(i+4)%lexema, 'tk_id', token_array(i+4)%linea, token_array(i+4)%columna)
                elseif(token_array (i+5)%tipo .NE. 'tk_par_der')then
                    call agregar_error(token_array(i+5)%lexema, 'tk_par_der', token_array(i+5)%linea, token_array(i+5)%columna)    
                elseif(token_array(i+6)%tipo .NE. 'tk_pyc')then
                    call agregar_error(token_array(i+6)%lexema, 'tk_pyc', token_array(i+6)%linea, token_array(i+6)%columna)
                else
                    call boton_set_grupo(token_array(i+4)%lexema, token_array(i)%lexema)
                endif
            endif
        endif

        !Inicia Area de Texto 
 
    if (token_array(i)%tipo == 'tk_id' .and. token_array(i+1)%tipo == 'tk_punto' ) then
        if(token_array(i+2)%tipo == 'tk_setTexto')then
            if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
            elseif(token_array(i+4)%tipo .NE. 'tk_cadena')then
                call agregar_error(token_array(i+4)%lexema, 'tk_cadena', token_array(i+4)%linea, token_array(i+4)%columna)
            elseif(token_array (i+5)%tipo .NE. 'tk_par_der')then
                call agregar_error(token_array(i+5)%lexema, 'tk_par_der', token_array(i+5)%linea, token_array(i+5)%columna)    
            elseif(token_array(i+6)%tipo .NE. 'tk_pyc')then
                call agregar_error(token_array(i+6)%lexema, 'tk_pyc', token_array(i+6)%linea, token_array(i+6)%columna)
            else
                call a_texto_set_texto(token_array(i)%lexema, token_array(i+4)%lexema)
            end if
        endif
        if(token_array(i+2)%tipo == 'tk_setPosicion')then
            if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
            elseif(token_array(i+4)%tipo .NE. 'tk_num')then
                call agregar_error(token_array(i+4)%lexema, 'tk_num', token_array(i+4)%linea, token_array(i+4)%columna)
            elseif(token_array(i+5)%tipo .NE. 'tk_coma')then
                call agregar_error(token_array(i+5)%lexema, 'tk_coma', token_array(i+5)%linea, token_array(i+5)%columna)
            elseif(token_array(i+6)%tipo .NE. 'tk_num')then
                call agregar_error(token_array(i+6)%lexema, 'tk_num', token_array(i+6)%linea, token_array(i+6)%columna)
            elseif(token_array(i+7)%tipo .NE. 'tk_par_der')then
                call agregar_error(token_array(i+7)%lexema, 'tk_par_der', token_array(i+7)%linea, token_array(i+7)%columna)
            elseif(token_array(i+8)%tipo .NE. 'tk_pyc')then
                call agregar_error(token_array(i+8)%lexema, 'tk_pyc', token_array(i+8)%linea, token_array(i+8)%columna)
            else
                call a_texto_set_posicion(token_array(i)%lexema, token_array(i+4)%lexema, token_array(i+6)%lexema)
            endif
        endif
        if(token_array(i+2)%tipo == 'tk_add')then
            if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                elseif(token_array(i+4)%tipo .NE. 'tk_id')then
                    call agregar_error(token_array(i+4)%lexema, 'tk_id', token_array(i+4)%linea, token_array(i+4)%columna)
                elseif(token_array (i+5)%tipo .NE. 'tk_par_der')then
                    call agregar_error(token_array(i+5)%lexema, 'tk_par_der', token_array(i+5)%linea, token_array(i+5)%columna)    
                elseif(token_array(i+6)%tipo .NE. 'tk_pyc')then
                    call agregar_error(token_array(i+6)%lexema, 'tk_pyc', token_array(i+6)%linea, token_array(i+6)%columna)
                else
                    call a_texto_set_grupo(token_array(i+4)%lexema, token_array(i)%lexema)
                endif
            endif
        endif

        !Inicia Clave
        
        if (token_array(i)%tipo == 'tk_id' .and. token_array(i+1)%tipo == 'tk_punto' ) then
            if(token_array(i+2)%tipo == 'tk_setTexto')then
                if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                    call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                elseif(token_array(i+4)%tipo .NE. 'tk_cadena')then
                    call agregar_error(token_array(i+4)%lexema, 'tk_cadena', token_array(i+4)%linea, token_array(i+4)%columna)
                elseif(token_array (i+5)%tipo .NE. 'tk_par_der')then
                    call agregar_error(token_array(i+5)%lexema, 'tk_par_der', token_array(i+5)%linea, token_array(i+5)%columna)    
                elseif(token_array(i+6)%tipo .NE. 'tk_pyc')then
                    call agregar_error(token_array(i+6)%lexema, 'tk_pyc', token_array(i+6)%linea, token_array(i+6)%columna)
                else
                    call clave_set_texto(token_array(i)%lexema, token_array(i+4)%lexema)
                end if
            endif
            if(token_array(i+2)%tipo == 'tk_setPosicion')then
                if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                    call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                elseif(token_array(i+4)%tipo .NE. 'tk_num')then
                    call agregar_error(token_array(i+4)%lexema, 'tk_num', token_array(i+4)%linea, token_array(i+4)%columna)
                elseif(token_array(i+5)%tipo .NE. 'tk_coma')then
                    call agregar_error(token_array(i+5)%lexema, 'tk_coma', token_array(i+5)%linea, token_array(i+5)%columna)
                elseif(token_array(i+6)%tipo .NE. 'tk_num')then
                    call agregar_error(token_array(i+6)%lexema, 'tk_num', token_array(i+6)%linea, token_array(i+6)%columna)
                elseif(token_array(i+7)%tipo .NE. 'tk_par_der')then
                    call agregar_error(token_array(i+7)%lexema, 'tk_par_der', token_array(i+7)%linea, token_array(i+7)%columna)
                elseif(token_array(i+8)%tipo .NE. 'tk_pyc')then
                    call agregar_error(token_array(i+8)%lexema, 'tk_pyc', token_array(i+8)%linea, token_array(i+8)%columna)
                else
                    call clave_set_posicion(token_array(i)%lexema, token_array(i+4)%lexema, token_array(i+6)%lexema)
                endif
            endif
            if(token_array(i+2)%tipo == 'tk_add')then
                if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                    call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                    elseif(token_array(i+4)%tipo .NE. 'tk_id')then
                        call agregar_error(token_array(i+4)%lexema, 'tk_id', token_array(i+4)%linea, token_array(i+4)%columna)
                    elseif(token_array (i+5)%tipo .NE. 'tk_par_der')then
                        call agregar_error(token_array(i+5)%lexema, 'tk_par_der', token_array(i+5)%linea, token_array(i+5)%columna)    
                    elseif(token_array(i+6)%tipo .NE. 'tk_pyc')then
                        call agregar_error(token_array(i+6)%lexema, 'tk_pyc', token_array(i+6)%linea, token_array(i+6)%columna)
                    else
                        call clave_set_grupo(token_array(i+4)%lexema, token_array(i)%lexema)
                    endif
                endif
            endif

            !Inicia Check
            
            if (token_array(i)%tipo == 'tk_id' .and. token_array(i+1)%tipo == 'tk_punto' ) then
                if(token_array(i+2)%tipo == 'tk_setTexto')then
                    if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                        call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                    elseif(token_array(i+4)%tipo .NE. 'tk_cadena')then
                        call agregar_error(token_array(i+4)%lexema, 'tk_cadena', token_array(i+4)%linea, token_array(i+4)%columna)
                    elseif(token_array (i+5)%tipo .NE. 'tk_par_der')then
                        call agregar_error(token_array(i+5)%lexema, 'tk_par_der', token_array(i+5)%linea, token_array(i+5)%columna)    
                    elseif(token_array(i+6)%tipo .NE. 'tk_pyc')then
                        call agregar_error(token_array(i+6)%lexema, 'tk_pyc', token_array(i+6)%linea, token_array(i+6)%columna)
                    else
                        call check_set_texto(token_array(i)%lexema, token_array(i+4)%lexema)
                    end if
                endif
                if(token_array(i+2)%tipo == 'tk_setMarcada')then
                    if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                        call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                    elseif(token_array(i+4)%tipo .NE. 'tk_cadena')then
                        call agregar_error(token_array(i+4)%lexema, 'tk_cadena', token_array(i+4)%linea, token_array(i+4)%columna)
                    elseif(token_array (i+5)%tipo .NE. 'tk_par_der')then
                        call agregar_error(token_array(i+5)%lexema, 'tk_par_der', token_array(i+5)%linea, token_array(i+5)%columna)    
                    elseif(token_array(i+6)%tipo .NE. 'tk_pyc')then
                        call agregar_error(token_array(i+6)%lexema, 'tk_pyc', token_array(i+6)%linea, token_array(i+6)%columna)
                    else
                        call check_set_marcado(token_array(i)%lexema, token_array(i+4)%lexema)
                    end if
                endif

                if(token_array(i+2)%tipo == 'tk_setPosicion')then
                    if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                        call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                    elseif(token_array(i+4)%tipo .NE. 'tk_num')then
                        call agregar_error(token_array(i+4)%lexema, 'tk_num', token_array(i+4)%linea, token_array(i+4)%columna)
                    elseif(token_array(i+5)%tipo .NE. 'tk_coma')then
                        call agregar_error(token_array(i+5)%lexema, 'tk_coma', token_array(i+5)%linea, token_array(i+5)%columna)
                    elseif(token_array(i+6)%tipo .NE. 'tk_num')then
                        call agregar_error(token_array(i+6)%lexema, 'tk_num', token_array(i+6)%linea, token_array(i+6)%columna)
                    elseif(token_array(i+7)%tipo .NE. 'tk_par_der')then
                        call agregar_error(token_array(i+7)%lexema, 'tk_par_der', token_array(i+7)%linea, token_array(i+7)%columna)
                    elseif(token_array(i+8)%tipo .NE. 'tk_pyc')then
                        call agregar_error(token_array(i+8)%lexema, 'tk_pyc', token_array(i+8)%linea, token_array(i+8)%columna)
                    else
                        call check_set_posicion(token_array(i)%lexema, token_array(i+4)%lexema, token_array(i+6)%lexema)
                    endif
                endif
                if(token_array(i+2)%tipo == 'tk_add')then
                    if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                        call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                        elseif(token_array(i+4)%tipo .NE. 'tk_id')then
                            call agregar_error(token_array(i+4)%lexema, 'tk_id', token_array(i+4)%linea, token_array(i+4)%columna)
                        elseif(token_array (i+5)%tipo .NE. 'tk_par_der')then
                            call agregar_error(token_array(i+5)%lexema, 'tk_par_der',&
                             token_array(i+5)%linea, token_array(i+5)%columna)    
                        elseif(token_array(i+6)%tipo .NE. 'tk_pyc')then
                            call agregar_error(token_array(i+6)%lexema, 'tk_pyc', token_array(i+6)%linea, token_array(i+6)%columna)
                        else
                            call check_set_grupo(token_array(i+4)%lexema, token_array(i)%lexema)
                        endif
                    endif
                endif


                !Inicia RadioBoton
                
            if (token_array(i)%tipo == 'tk_id' .and. token_array(i+1)%tipo == 'tk_punto' ) then
                if(token_array(i+2)%tipo == 'tk_setTexto')then
                    if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                        call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                    elseif(token_array(i+4)%tipo .NE. 'tk_cadena')then
                        call agregar_error(token_array(i+4)%lexema, 'tk_cadena', token_array(i+4)%linea, token_array(i+4)%columna)
                    elseif(token_array (i+5)%tipo .NE. 'tk_par_der')then
                        call agregar_error(token_array(i+5)%lexema, 'tk_par_der', token_array(i+5)%linea, token_array(i+5)%columna)    
                    elseif(token_array(i+6)%tipo .NE. 'tk_pyc')then
                        call agregar_error(token_array(i+6)%lexema, 'tk_pyc', token_array(i+6)%linea, token_array(i+6)%columna)
                    else
                        call radio_set_texto(token_array(i)%lexema, token_array(i+4)%lexema)
                    end if
                endif
                if(token_array(i+2)%tipo == 'tk_setMarcada')then
                    if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                        call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                    elseif(token_array(i+4)%tipo .NE. 'tk_cadena')then
                        call agregar_error(token_array(i+4)%lexema, 'tk_cadena', token_array(i+4)%linea, token_array(i+4)%columna)
                    elseif(token_array (i+5)%tipo .NE. 'tk_par_der')then
                        call agregar_error(token_array(i+5)%lexema, 'tk_par_der', token_array(i+5)%linea, token_array(i+5)%columna)    
                    elseif(token_array(i+6)%tipo .NE. 'tk_pyc')then
                        call agregar_error(token_array(i+6)%lexema, 'tk_pyc', token_array(i+6)%linea, token_array(i+6)%columna)
                    else
                        call radio_set_marcado(token_array(i)%lexema, token_array(i+4)%lexema)
                    end if
                endif

                if(token_array(i+2)%tipo == 'tk_setPosicion')then
                    if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                        call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                    elseif(token_array(i+4)%tipo .NE. 'tk_num')then
                        call agregar_error(token_array(i+4)%lexema, 'tk_num', token_array(i+4)%linea, token_array(i+4)%columna)
                    elseif(token_array(i+5)%tipo .NE. 'tk_coma')then
                        call agregar_error(token_array(i+5)%lexema, 'tk_coma', token_array(i+5)%linea, token_array(i+5)%columna)
                    elseif(token_array(i+6)%tipo .NE. 'tk_num')then
                        call agregar_error(token_array(i+6)%lexema, 'tk_num', token_array(i+6)%linea, token_array(i+6)%columna)
                    elseif(token_array(i+7)%tipo .NE. 'tk_par_der')then
                        call agregar_error(token_array(i+7)%lexema, 'tk_par_der', token_array(i+7)%linea, token_array(i+7)%columna)
                    elseif(token_array(i+8)%tipo .NE. 'tk_pyc')then
                        call agregar_error(token_array(i+8)%lexema, 'tk_pyc', token_array(i+8)%linea, token_array(i+8)%columna)
                    else
                        call radio_set_posicion(token_array(i)%lexema, token_array(i+4)%lexema, token_array(i+6)%lexema)
                    endif
                endif
                if(token_array(i+2)%tipo == 'tk_add')then
                    if(token_array(i+3)%tipo .NE. 'tk_par_izq')then
                        call agregar_error(token_array(i+3)%lexema, 'tk_par_izq', token_array(i+3)%linea, token_array(i+3)%columna)
                        elseif(token_array(i+4)%tipo .NE. 'tk_id')then
                            call agregar_error(token_array(i+4)%lexema, 'tk_id', token_array(i+4)%linea, token_array(i+4)%columna)
                        elseif(token_array (i+5)%tipo .NE. 'tk_par_der')then
                            call agregar_error(token_array(i+5)%lexema, 'tk_par_der',&
                             token_array(i+5)%linea, token_array(i+5)%columna)    
                        elseif(token_array(i+6)%tipo .NE. 'tk_pyc')then
                            call agregar_error(token_array(i+6)%lexema, 'tk_pyc', token_array(i+6)%linea, token_array(i+6)%columna)
                        else
                            call radio_set_grupo(token_array(i+4)%lexema, token_array(i)%lexema)
                        endif
                    endif
                endif
    end do

    numErrores = size(error_array)
    !print *, "Numero de errores: ", numErrores
    if(numErrores > 2 )then
        call imprimir_errores
    else
        call imprimir_tokens
        call imprimir
    endif 
end subroutine parser

subroutine imprimir_tokens()
    integer :: i, ios, file_unit
    CHARACTER(len=20) :: str_linea, str_columna, str_no_T

    if(.NOT. ALLOCATED(token_array))then
        print*, "No hay tokens"
    else
        open(unit=file_unit, file="..\Out\tokens.html", status="replace", action="write", iostat=ios)
            if (ios /= 0) then
                print *, "Error al crear el archivo HTML Tokens."
            else
                write(file_unit, '(A)') '<!DOCTYPE html>' // new_line('a')
                write(file_unit, '(A)') '<html><head>' &
                // '<title>Tokens</title><style>' // new_line('a')
                write(file_unit, '(A)') 'body {Background-color: #d8d3d3 }' // new_line('a')
                write(file_unit, '(A)') 'h2 { color: #000000; text-align: center; }' // new_line('a')                
                write(file_unit, '(A)') 'table { font-family: Arial, sans-serif;'
                write(file_unit, '(A)') 'border-collapse: collapse; width: 100%; }' // new_line('a')
                write(file_unit, '(A)') 'td, th { border: 2px solid #000000; text-align: left; padding: 8px; }' // new_line('a')
                write(file_unit, '(A)') 'th {background-color: #5B9BD4}' // new_line('a')
                write(file_unit, '(A)') 'tr:nth-child(odd) { background-color: #DEEBF6 ; }' // new_line('a')
                write(file_unit, '(A)') 'tr:nth-child(even) { background-color: #FFFFFF; }' // new_line('a')
                write(file_unit, '(A)') '</style></head><body><h2>Tabla de Tokens</h2>' // new_line('a')
                write(file_unit, '(A)') '<table><tr><th>No.</th><th>Lexema</th><th>Tipo' 
                write(file_unit, '(A)') '</th><th>Columna</th><th>Fila</th></tr>' // new_line('a')
                do i = 1, size(token_array)
                    !write(str_descripcion, '(A)') trim(tokens(i)%descripcion_T)
                    write(str_columna, '(I0)') token_array(i)%columna
                    write(str_linea, '(I0)')  token_array(i)%linea
                    write(str_no_T, '(I10)') i
                    ! Escribir cada fila directamente al archivo
                    write(file_unit, '(A)') '<tr><td>' // str_no_T // '</td><td>'&
                     // TRIM(token_array(i)%lexema) // &
                     '</td><td>' // trim(token_array(i)%tipo) // & 
                    '</td><td>' // trim(str_columna) // '</td><td>'&
                     // trim(str_linea) // '</td></tr>' // new_line('a')
                end do

                ! Cerrar la tabla y el HTML
                write(file_unit, '(A)') '</table></body></html>'
                close(file_unit)
   
            endif
    endif
    
    write(*,*) TRIM("tokens")
end subroutine imprimir_tokens


END MODULE token