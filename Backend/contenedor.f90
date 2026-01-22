MODULE contenedor
    IMPLICIT NONE

    type :: Tag
        CHARACTER(LEN=50) :: id
        CHARACTER(LEN=20) :: tipo
        CHARACTER(LEN=20) :: alto 
        CHARACTER(LEN=20) :: ancho
        CHARACTER(LEN=50) :: color_fondo_r
        CHARACTER(LEN=50) :: color_fondo_g
        CHARACTER(LEN=50) :: color_fondo_b
        CHARACTER(LEN=50) :: posicion_x
        CHARACTER(LEN=50) :: posicion_y
        CHARACTER(LEN=50) :: grupo
    END TYPE Tag

    type(Tag), ALLOCATABLE :: contenedor_array(:)

    contains

    subroutine agregar_contendor(id)
        CHARACTER(LEN=*), INTENT(IN) :: id

        Type(Tag) :: nuevo_contenedor
        integer :: n
        type(Tag), ALLOCATABLE :: temp_array(:)
        
        nuevo_contenedor%id = id
        nuevo_contenedor%tipo = 'Contenedor'
        nuevo_contenedor%alto = ""
        nuevo_contenedor%ancho = ""
        nuevo_contenedor%color_fondo_r = ""
        nuevo_contenedor%color_fondo_g = ""
        nuevo_contenedor%color_fondo_b = ""
        nuevo_contenedor%posicion_x = ""
        nuevo_contenedor%posicion_y = ""
        nuevo_contenedor%grupo = "body"
        if(.NOT. ALLOCATED(contenedor_array))then
            ALLOCATE(contenedor_array(1))
            contenedor_array(1) = nuevo_contenedor
        else
            n = SIZE(contenedor_array)
            ALLOCATE(temp_array(n+1))
            temp_array(:n) = contenedor_array
            temp_array(n+1) = nuevo_contenedor
            DEALLOCATE(contenedor_array)
            ALLOCATE(contenedor_array(n+1))
            contenedor_array = temp_array
        endif

    end subroutine agregar_contendor

    subroutine contenedor_set_alto(id, alto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: alto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(contenedor_array)) then
            return
        else
            DO i = 1, size(contenedor_array)
                if (trim(contenedor_array(i)%id) == id) then
                    contenedor_array(i)%alto = alto
                end if
            END DO
        end if

    end subroutine contenedor_set_alto

    subroutine contenedor_set_ancho(id, ancho)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: ancho
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(contenedor_array)) then
            return
        else
            DO i = 1, size(contenedor_array)
                if (trim(contenedor_array(i)%id) == id) then
                    contenedor_array(i)%ancho = ancho
                end if
            END DO
        end if

    end subroutine contenedor_set_ancho

    subroutine contenedor_set_color_fondo(id, color_fondo_r, color_fondo_g, color_fondo_b)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: color_fondo_r
        CHARACTER(LEN=*), INTENT(IN) :: color_fondo_g
        CHARACTER(LEN=*), INTENT(IN) :: color_fondo_b
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(contenedor_array)) then
            return
        else
            DO i = 1, size(contenedor_array)
                if (trim(contenedor_array(i)%id) == id) then
                    contenedor_array(i)%color_fondo_r = color_fondo_r
                    contenedor_array(i)%color_fondo_g = color_fondo_g
                    contenedor_array(i)%color_fondo_b = color_fondo_b
                end if
            END DO
        end if

    end subroutine contenedor_set_color_fondo

    subroutine contenedor_set_posicion(id, posicion_x, posicion_y)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: posicion_x
        CHARACTER(LEN=*), INTENT(IN) :: posicion_y
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(contenedor_array)) then
            return
        else
            DO i = 1, size(contenedor_array)
                if (trim(contenedor_array(i)%id) == id) then
                    contenedor_array(i)%posicion_x = posicion_x
                    contenedor_array(i)%posicion_y = posicion_y
                end if
            END DO
        end if

    end subroutine contenedor_set_posicion

    subroutine imprimir_contenedores()
        integer :: i, file_unit, ios
        
        file_unit = 10
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
                    write(file_unit, '(A)') '<div id = "'&
                    // trim(contenedor_array(i)%id) // &
                    '">' //NEW_LINE('A') 
                    write(file_unit, '(A)') '</div>'
             END DO
            ! call imprimir_area
            ! call imprimir_botones
            ! call imprimir_checks
            ! call imprimir_claves
            ! call imprimir_errores
            ! call imprimir_etiquetas
            ! call imprimir_textos
            write(file_unit, '(A)') '</body></html>'
            close(file_unit)
            call imprimir_contenedores_estilo
        endif
    end subroutine imprimir_contenedores

    subroutine imprimir_contenedores_estilo
        integer :: i, file_unit, ios
        file_unit = 20
    open (unit=file_unit, file="..\Out\Salida.css", status="replace", action="write", iostat=ios)
        if(ios /= 0)then
            print *, "Errar al abrir el archivo"
        else
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
            ! call imprimir_area
            ! call imprimir_botones
            ! call imprimir_checks
            ! call imprimir_claves
            ! call imprimir_errores
            ! call imprimir_etiquetas
            ! call imprimir_textos
            close(file_unit)
        endif
    end subroutine imprimir_contenedores_estilo

    subroutine contenedor_set_grupo(id, grupo)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: grupo
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(contenedor_array)) then
            return
        else
            DO i = 1, size(contenedor_array)
                if (trim(contenedor_array(i)%id) == id) then
                    contenedor_array(i)%grupo = grupo
                end if
            END DO
        end if

    end subroutine contenedor_set_grupo


    FUNCTION buscar_contenedor_por_id(id) RESULT(encontrado)
        CHARACTER(LEN=*), INTENT(IN) :: id
        logical :: encontrado
        integer :: i

        encontrado = .FALSE.

        if (.NOT. ALLOCATED(contenedor_array)) then
            return
        else
            DO i = 1, size(contenedor_array)
                if (trim(contenedor_array(i)%id) == trim(id)) then
                    encontrado = .TRUE.
                    return
                end if
            END DO
        end if
    END FUNCTION buscar_contenedor_por_id

END MODULE contenedor

