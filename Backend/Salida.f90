MODULE Salida
    use a_texto
    use botones
    use check
    use clave
    use contenedor
    use etiqueta
    use radio_btn
    use texto
IMPLICIT NONE

contains
subroutine imprimir
    integer :: i, file_unit, ios, j, z

    open (unit=file_unit, file="..\Out\Salida.html", status="replace", action="write", iostat=ios)
    if(ios /= 0)then
        print *, "Errar al abrir el archivo"
    else
        write(file_unit, '(A)') '<!DOCTYPE html>' // NEW_LINE('A')
        write(file_unit, '(A)') '<html><head>' &
         // '<title>Salida</title>' &
         // '<link rel="stylesheet" type="text/css" href="Salida.css">' &
         // '</head><body>' // NEW_LINE('A')

            DO i = 1, size(contenedor_array)
                !Si el grupo del contenedor es el grupo body, se crea el div en el body    
                if(contenedor_array(i)%grupo == 'body')then
                    write(file_unit, '(A)') '<div id="' &
                    // trim(contenedor_array(i)%id) // '">'&
                    // NEW_LINE('A')
                !COntenedores
                    DO j = 1, size(contenedor_array) 
                    if(contenedor_array(i)%id == &
                    contenedor_array(j)%grupo)then
                        write(file_unit, '(A)') '<div id="'&
                        // trim(contenedor_array(j)%id) &
                        // '">' // NEW_LINE('A')
                        DO z = 1, size(etiqueta_array)
                            if(contenedor_array(j)%id == &
                            etiqueta_array(z)%grupo)then
                                write(file_unit, '(A)') '<label id="' &
                                // trim(etiqueta_array(z)%id) // '">' & 
                                // trim(etiqueta_array(z)%texto) & 
                                // '</label>' // NEW_LINE('A')
                            end if
                            if(contenedor_array(j)%id == &
                            texto_array(z)%grupo)then
                                write(file_unit, '(A)') '<input type="text" id="' &
                                // trim(texto_array(z)%id) &
                                // '" value = "' // texto_array(z)%texto &
                                ! // '" style = "text-align:'&
                                ! // texto_array(z)%alineacion &
                                //'"/>' // NEW_LINE('A')
                            end if
                            if(contenedor_array(j)%id == &  
                            clave_array(z)%grupo)then
                                write(file_unit, '(A)') '<input type = "password" id="' &
                                // trim(clave_array(z)%id) &
                                // '" value="'&
                                // trim(clave_array(z)%texto) //'"' &
                                ! // 'style="text-align:' &
                                ! // clave_array(z)%alineacion &
                                // '"/>' // NEW_LINE('A')
                            end if
                            if(contenedor_array(j)%id == &
                             boton_array(z)%grupo)then
                                write(file_unit, '(A)') '<input type="submit" id="'&
                                // trim(boton_array(z)%id) &
                                // '" value=' // trim(boton_array(z)%texto) &
                                 // ' style = "text-allign:'&
                                 // trim(boton_array(z)%alineacion) &
                                // '"/>' // NEW_LINE('A')

                            endif   
                        END DO
                    if(contenedor_array(i)%id == &
                            etiqueta_array(j)%grupo   )then
                            write(file_unit, '(A)') '<label id="' &
                            // trim(etiqueta_array(j)%id) // '">' &
                            // trim(etiqueta_array(j)%texto) &
                            // '</label>' // NEW_LINE('A')  
                        end if
                        write(file_unit, '(A)') '</div>' // NEW_LINE('A')
                    end if
                    !Espacio para los elementos del contenedor
                   END DO
                write(file_unit, '(A)') '</div>' // NEW_LINE('A')
            endif    
            END DO
        write(file_unit, '(A)') '</body></html>'
        close(file_unit)
        call imprimir_estilo
    endif
end subroutine imprimir

subroutine imprimir_estilo
    integer :: i, file_unit, ios
        file_unit = 20
    open (unit=file_unit, file="..\Out\Salida.css", status="replace", action="write", iostat=ios)
        if(ios /= 0)then
            print *, "Errar al abrir el archivo"
        else
            !CONTENEDORES
            DO i = 1, size(contenedor_array)
                write(file_unit, '(A)') '#' // trim(contenedor_array(i)%id)// '{' // NEW_LINE('A')
                write(file_unit, '(A)') 'background-color:rgb('&
                        // trim(contenedor_array(i)%color_fondo_r)// ',' &
                        // trim(contenedor_array(i)%color_fondo_g)// ',' &
                        // trim(contenedor_array(i)%color_fondo_b)// ');' // NEW_LINE('A') 
                write(file_unit, '(A)') 'height:' // trim(contenedor_array(i)%alto) // 'px;' // NEW_LINE('A')
                write(file_unit, '(A)') 'width:' // trim(contenedor_array(i)%ancho) // 'px;' // NEW_LINE('A')
                write(file_unit, '(A)') 'position:absolute;' // NEW_LINE('A')
                write(file_unit, '(A)') 'top:' // trim(contenedor_array(i)%posicion_y) // 'px;' // NEW_LINE('A')
                write(file_unit, '(A)') 'left:' // trim(contenedor_array(i)%posicion_x) // 'px;' // NEW_LINE('A')
                write(file_unit, '(A)') '}'
            END DO
                !Etiquetas
            DO i = 1, size(etiqueta_array)
                write(file_unit, '(A)') '#' // trim(etiqueta_array(i)%id) // '{' // NEW_LINE('A')
                write(file_unit, '(A)') 'position:absolute;' // NEW_LINE('A')
                write(file_unit, '(A)') 'top:' // trim(etiqueta_array(i)%posicion_y) // 'px;' // NEW_LINE('A')
                write(file_unit, '(A)') 'left:' // trim(etiqueta_array(i)%posicion_x) // 'px;' // NEW_LINE('A')
                write(file_unit, '(A)') 'width:' // trim(etiqueta_array(i)%ancho) // 'px;' // NEW_LINE('A')
                write(file_unit, '(A)') 'height:' // trim(etiqueta_array(i)%alto) // 'px;' // NEW_LINE('A')
                write(file_unit, '(A)') 'color:rgb('&
                        // trim(etiqueta_array(i)%color_texto_r)// ',' &
                        // trim(etiqueta_array(i)%color_texto_g)// ',' &
                        // trim(etiqueta_array(i)%color_texto_b)// ');' // NEW_LINE('A')
                write(file_unit, '(A)') 'font-size: 12px;' // NEW_LINE('A')
                        write(file_unit, '(A)') '}'
             END DO
             !CLAVE
         DO i = 1, size(clave_array)
             write(file_unit, '(A)') '#' // trim(clave_array(i)%id) // '{' // NEW_LINE('A')
             write(file_unit, '(A)') 'position:absolute;' // NEW_LINE('A')
             write(file_unit, '(A)') 'top:' // trim(clave_array(i)%posicion_y) // 'px;' // NEW_LINE('A')
             write(file_unit, '(A)') 'left:' // trim(clave_array(i)%posicion_x) // 'px;' // NEW_LINE('A')
             write(file_unit, '(A)') 'width:' // trim(clave_array(i)%ancho) // 'px;' // NEW_LINE('A')
             write(file_unit, '(A)') 'height:' // trim(clave_array(i)%alto) // 'px;' // NEW_LINE('A')
             write(file_unit, '(A)') 'font-size: 12px;' // NEW_LINE('A')
             write(file_unit, '(A)') '}'
          END DO
          !TEXTOS
          DO i = 1, size(texto_array)
            write(file_unit, '(A)') '#' // trim(texto_array(i)%id) // '{' // NEW_LINE('A')
            write(file_unit, '(A)') 'position:absolute;' // NEW_LINE('A')
            write(file_unit, '(A)') 'top:' // trim(texto_array(i)%posicion_y) // 'px;' // NEW_LINE('A')
            write(file_unit, '(A)') 'left:' // trim(texto_array(i)%posicion_x) // 'px;' // NEW_LINE('A')
            write(file_unit, '(A)') 'width:' // trim(texto_array(i)%ancho) // 'px;' // NEW_LINE('A')
            write(file_unit, '(A)') 'height:' // trim(texto_array(i)%alto) // 'px;' // NEW_LINE('A')
            write(file_unit, '(A)') 'font-size: 12px;' // NEW_LINE('A')
            write(file_unit, '(A)') '}'
         END DO
            !BOTONES
         DO i = 1, size(boton_array)
            write(file_unit, '(A)') '#' // trim(boton_array(i)%id) // '{' // NEW_LINE('A')
            write(file_unit, '(A)') 'position:absolute;' // NEW_LINE('A')
            write(file_unit, '(A)') 'top:' // trim(boton_array(i)%posicion_y) // 'px;' // NEW_LINE('A')
            write(file_unit, '(A)') 'left:' // trim(boton_array(i)%posicion_x) // 'px;' // NEW_LINE('A')
            write(file_unit, '(A)') 'width:' // trim(boton_array(i)%ancho) // 'px;' // NEW_LINE('A')
            write(file_unit, '(A)') 'height:' // trim(boton_array(i)%alto) // 'px;' // NEW_LINE('A')
            write(file_unit, '(A)') 'font-size: 12px;' // NEW_LINE('A')
            write(file_unit, '(A)') '}'
         END DO
            close(file_unit)
        endif
end subroutine imprimir_estilo

END MODULE Salida