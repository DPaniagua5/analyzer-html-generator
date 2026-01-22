MODULE etiqueta
    IMPLICIT NONE

    type :: Tag
        CHARACTER(LEN=50) :: id
        CHARACTER(LEN=20) :: tipo
        CHARACTER(LEN=20) :: alto 
        CHARACTER(LEN=20) :: ancho
        CHARACTER(LEN=200) :: texto 
        CHARACTER(LEN=50) :: color_texto_r
        CHARACTER(LEN=50) :: color_texto_g
        CHARACTER(LEN=50) :: color_texto_b
        CHARACTER(LEN=50) :: posicion_x
        CHARACTER(LEN=50) :: posicion_y
        CHARACTER(LEN=50) :: grupo
    END TYPE Tag

    type(Tag), ALLOCATABLE :: etiqueta_array(:)

    contains

    subroutine agregar_etiqueta(id)
        CHARACTER(LEN=*), INTENT(IN) :: id

        Type(Tag) :: nuevo_etiqueta
        integer :: n
        type(Tag), ALLOCATABLE :: temp_array(:)
        
        nuevo_etiqueta%id = id
        nuevo_etiqueta%tipo = 'Etiqueta'
        nuevo_etiqueta%alto = ""
        nuevo_etiqueta%ancho = ""
        nuevo_etiqueta%texto = ""
        nuevo_etiqueta%color_texto_r = ""
        nuevo_etiqueta%color_texto_g = ""
        nuevo_etiqueta%color_texto_b = ""
        nuevo_etiqueta%posicion_x = ""
        nuevo_etiqueta%posicion_y = ""
        nuevo_etiqueta%grupo = ""


        if(.NOT. ALLOCATED(etiqueta_array))then
            ALLOCATE(etiqueta_array(1))
            etiqueta_array(1) = nuevo_etiqueta
        else
            n = SIZE(etiqueta_array)
            ALLOCATE(temp_array(n+1))
            temp_array(:n) = etiqueta_array
            temp_array(n+1) = nuevo_etiqueta
            DEALLOCATE(etiqueta_array)
            ALLOCATE(etiqueta_array(n+1))
            etiqueta_array = temp_array
        endif

    end subroutine agregar_etiqueta

    subroutine imprimir_etiquetas()
        integer :: i, file_unit, ios
        
    open (unit=file_unit, file="..\Out\etiquetas.html", status="replace", action="write", iostat=ios)
        if(ios /= 0)then
            print *, "Errar al abrir el archivo"
        else
            write(file_unit, '(A)') '<!DOCTYPE html>' // NEW_LINE('A')
            write(file_unit, '(A)') '<html><head>' &
             // '<title>Etiquetas</title></head><body>' // NEW_LINE('A')
                DO i = 1, size(etiqueta_array)
                write(file_unit, '(A)') '<h2>ID: ', trim(etiqueta_array(i)%id), '</h2>'
                write(file_unit, '(A)') '<h2>Tipo: ', trim(etiqueta_array(i)%tipo), '</h2>'
                write(file_unit, '(A)') '<h2>Alto: ', trim(etiqueta_array(i)%alto), '</h2>'
                write(file_unit, '(A)') '<h2>Ancho: ', trim(etiqueta_array(i)%ancho), '</h2>'
                write(file_unit, '(A)') '<h2>Texto: ', trim(etiqueta_array(i)%texto), '</h2>'
                write(file_unit, '(A)') '<h2>Color Texto R: ', trim(etiqueta_array(i)%color_texto_r), '</h2>'
                write(file_unit, '(A)') '<h2>Color Texto G: ', trim(etiqueta_array(i)%color_texto_g), '</h2>'
                write(file_unit, '(A)') '<h2>Color Texto B: ', trim(etiqueta_array(i)%color_texto_b), '</h2>'
                write(file_unit, '(A)') '<h2>Posicion X: ', trim(etiqueta_array(i)%posicion_x), '</h2>'
                write(file_unit, '(A)') '<h2>Posicion Y: ', trim(etiqueta_array(i)%posicion_y), '</h2>'
                write(file_unit, '(A)') '<h2>Grupo: ', trim(etiqueta_array(i)%grupo), '</h2>'
            END DO
            write(file_unit, '(A)') '</body></html>'
            close(file_unit)
        endif
    end subroutine imprimir_etiquetas

    subroutine etiqueta_set_alto(id, alto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: alto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(etiqueta_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == id) then
                    etiqueta_array(i)%alto = alto
                end if
            END DO
        end if

    end subroutine etiqueta_set_alto

    subroutine etiqueta_set_ancho(id, ancho)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: ancho
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(etiqueta_array)) then
            return
        else
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == id) then
                    etiqueta_array(i)%ancho = ancho
                end if
            END DO
        end if

    end subroutine etiqueta_set_ancho

    subroutine etiqueta_set_texto(id, texto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: texto
        CHARACTER(LEN=100) :: aux_texto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(etiqueta_array)) then
            return
        else
        if (len_trim(texto) > 2) then
            aux_texto = texto(2:len_trim(texto)-1)
        else
            aux_texto = ""
        end if
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == id) then
                    etiqueta_array(i)%texto = aux_texto
                end if
            END DO
        end if

    end subroutine etiqueta_set_texto

    subroutine etiqueta_set_color_texto(id, color_texto_r, color_texto_g, color_texto_b)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: color_texto_r
        CHARACTER(LEN=*), INTENT(IN) :: color_texto_g
        CHARACTER(LEN=*), INTENT(IN) :: color_texto_b
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(etiqueta_array)) then
            return
        else
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == id) then
                    etiqueta_array(i)%color_texto_r = color_texto_r
                    etiqueta_array(i)%color_texto_g = color_texto_g
                    etiqueta_array(i)%color_texto_b = color_texto_b
                end if
            END DO
        end if

    end subroutine etiqueta_set_color_texto

    subroutine etiqueta_set_posicion(id, posicion_x, posicion_y)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: posicion_x
        CHARACTER(LEN=*), INTENT(IN) :: posicion_y
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(etiqueta_array)) then
            return
        else
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == id) then
                    etiqueta_array(i)%posicion_x = posicion_x
                    etiqueta_array(i)%posicion_y = posicion_y
                end if
            END DO
        end if

    end subroutine etiqueta_set_posicion

    subroutine etiqueta_set_grupo(id, grupo)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: grupo
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(etiqueta_array)) then
            return
        else
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == id) then
                    etiqueta_array(i)%grupo = grupo
                end if
            END DO
        end if

    end subroutine etiqueta_set_grupo
    
    FUNCTION buscar_etiqueta_por_id(id) RESULT(encontrado)
        CHARACTER(LEN=*), INTENT(IN) :: id
        logical :: encontrado
        integer :: i

        encontrado = .FALSE.

        if (.NOT. ALLOCATED(etiqueta_array)) then
            return
        else
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == trim(id)) then
                    encontrado = .TRUE.
                    return
                end if
            END DO
        end if
    END FUNCTION buscar_etiqueta_por_id

END MODULE etiqueta