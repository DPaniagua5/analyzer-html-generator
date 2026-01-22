MODULE radio_btn
IMPLICIT NONE
type :: Tag
    CHARACTER(LEN=50) :: id
    CHARACTER(LEN=20) :: tipo
    CHARACTER(LEN=20) :: alto 
    CHARACTER(LEN=20) :: ancho
    CHARACTER(LEN=200) :: texto
    CHARACTER(LEN=50) :: marcado
    CHARACTER(LEN=50) :: alineacion
    CHARACTER(LEN=50) :: posicion_x
    CHARACTER(LEN=50) :: posicion_y
    CHARACTER(LEN=50) :: grupo
END TYPE Tag
type(Tag), ALLOCATABLE :: radio_array(:)

contains
subroutine agregar_radio(id)
    CHARACTER(LEN=*), INTENT(IN) :: id

    Type(Tag) :: nuevo_radio
    integer :: n
    type(Tag), ALLOCATABLE :: temp_array(:)
    
    nuevo_radio%id = id
    nuevo_radio%tipo = 'Check'
    nuevo_radio%alto = '25'
    nuevo_radio%ancho = '100'
    nuevo_radio%texto = ''
    nuevo_radio%alineacion = 'left'
    nuevo_radio%posicion_x = ''
    nuevo_radio%posicion_y = ''
    nuevo_radio%grupo = ''
    nuevo_radio%marcado = 'false'
    if(.NOT. ALLOCATED(radio_array))then
        ALLOCATE(radio_array(1))
        radio_array(1) = nuevo_radio
    else
        n = SIZE(radio_array)
        ALLOCATE(temp_array(n+1))
        temp_array(:n) = radio_array
        temp_array(n+1) = nuevo_radio
        DEALLOCATE(radio_array)
        ALLOCATE(radio_array(n+1))
        radio_array = temp_array
    endif

end subroutine agregar_radio

subroutine radio_set_texto(id, texto)
    CHARACTER(LEN=*), INTENT(IN) :: id
    CHARACTER(LEN=*), INTENT(IN) :: texto
    integer :: i

    ! Verifica si la memoria ha sido asignada para el arreglo
    if (.NOT. ALLOCATED(radio_array)) then
        return
    else
        DO i = 1, size(radio_array)
            if (trim(radio_array(i)%id) == id) then
                radio_array(i)%texto = texto
            end if
        END DO
    end if

end subroutine radio_set_texto

subroutine radio_set_posicion(id, posicion_x, posicion_y)
    CHARACTER(LEN=*), INTENT(IN) :: id
    CHARACTER(LEN=*), INTENT(IN) :: posicion_x
    CHARACTER(LEN=*), INTENT(IN) :: posicion_y
    integer :: i

    ! Verifica si la memoria ha sido asignada para el arreglo
    if (.NOT. ALLOCATED(radio_array)) then
        return
    else
        DO i = 1, size(radio_array)
            if (trim(radio_array(i)%id) == id) then
                radio_array(i)%posicion_x = posicion_x
                radio_array(i)%posicion_y = posicion_y
            end if
        END DO
    end if
end subroutine radio_set_posicion

subroutine radio_set_grupo(id, grupo)
    CHARACTER(LEN=*), INTENT(IN) :: id
    CHARACTER(LEN=*), INTENT(IN) :: grupo
    integer :: i

    ! Verifica si la memoria ha sido asignada para el arreglo
    if (.NOT. ALLOCATED(radio_array)) then
        return
    else
        DO i = 1, size(radio_array)
            if (trim(radio_array(i)%id) == id) then
                radio_array(i)%grupo = grupo
            end if
        END DO
    end if

end subroutine radio_set_grupo

subroutine radio_set_marcado(id, marcado)
    CHARACTER(LEN=*), INTENT(IN) :: id
    CHARACTER(LEN=*), INTENT(IN) :: marcado
    integer :: i

    ! Verifica si la memoria ha sido asignada para el arreglo
    if (.NOT. ALLOCATED(radio_array)) then
        return
    else
        DO i = 1, size(radio_array)
            if (trim(radio_array(i)%id) == id) then
                radio_array(i)%marcado = marcado
            end if
        END DO
    end if

end subroutine radio_set_marcado


subroutine imprimir_radios()
    integer :: i, file_unit, ios
    
open (unit=file_unit, file="..\Out\Radio.html", status="replace", action="write", iostat=ios)
    if(ios /= 0)then
        print *, "Errar al abrir el archivo"
    else
        write(file_unit, '(A)') '<!DOCTYPE html>' // NEW_LINE('A')
        write(file_unit, '(A)') '<html><head>' &
         // '<title>Checks</title></head><body>' // NEW_LINE('A')
            DO i = 1, size(radio_array)
            write(file_unit, '(A)') '<h2>ID: ', trim(radio_array(i)%id), '</h2>'
            write(file_unit, '(A)') '<h2>Tipo: ', trim(radio_array(i)%tipo), '</h2>'
            write(file_unit, '(A)') '<h2>Alto: ', trim(radio_array(i)%alto), '</h2>'
            write(file_unit, '(A)') '<h2>Ancho: ', trim(radio_array(i)%ancho), '</h2>'
            write(file_unit, '(A)') '<h2>Texto: ', trim(radio_array(i)%texto), '</h2>'
            write(file_unit, '(A)') '<h2>Posicion X: ', trim(radio_array(i)%posicion_x), '</h2>'
            write(file_unit, '(A)') '<h2>Posicion Y: ', trim(radio_array(i)%posicion_y), '</h2>'
            write(file_unit, '(A)') '<h2>Alineacion: ', trim(radio_array(i)%alineacion), '</h2>'
            write(file_unit, '(A)') '<h2>Marcado: ', trim(radio_array(i)%marcado), '</h2>'
            write(file_unit, '(A)') '<h2>Grupo: ', trim(radio_array(i)%grupo), '</h2>'
        END DO
        write(file_unit, '(A)') '</body></html>'
        close(file_unit)
    endif
end subroutine imprimir_radios

END MODULE radio_btn