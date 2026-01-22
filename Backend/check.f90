MODULE Check
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
type(Tag), ALLOCATABLE :: check_array(:)

contains
subroutine agregar_check(id)
    CHARACTER(LEN=*), INTENT(IN) :: id

    Type(Tag) :: nuevo_check
    integer :: n
    type(Tag), ALLOCATABLE :: temp_array(:)
    
    nuevo_check%id = id
    nuevo_check%tipo = 'Check'
    nuevo_check%alto = '25'
    nuevo_check%ancho = '100'
    nuevo_check%texto = ''
    nuevo_check%alineacion = 'left'
    nuevo_check%posicion_x = ''
    nuevo_check%posicion_y = ''
    nuevo_check%grupo = ''
    nuevo_check%marcado = 'false'
    if(.NOT. ALLOCATED(check_array))then
        ALLOCATE(check_array(1))
        check_array(1) = nuevo_check
    else
        n = SIZE(check_array)
        ALLOCATE(temp_array(n+1))
        temp_array(:n) = check_array
        temp_array(n+1) = nuevo_check
        DEALLOCATE(check_array)
        ALLOCATE(check_array(n+1))
        check_array = temp_array
    endif

end subroutine agregar_check

subroutine check_set_texto(id, texto)
    CHARACTER(LEN=*), INTENT(IN) :: id
    CHARACTER(LEN=*), INTENT(IN) :: texto
    integer :: i

    ! Verifica si la memoria ha sido asignada para el arreglo
    if (.NOT. ALLOCATED(check_array)) then
        return
    else
        DO i = 1, size(check_array)
            if (trim(check_array(i)%id) == id) then
                check_array(i)%texto = texto
            end if
        END DO
    end if

end subroutine check_set_texto

subroutine check_set_posicion(id, posicion_x, posicion_y)
    CHARACTER(LEN=*), INTENT(IN) :: id
    CHARACTER(LEN=*), INTENT(IN) :: posicion_x
    CHARACTER(LEN=*), INTENT(IN) :: posicion_y
    integer :: i

    ! Verifica si la memoria ha sido asignada para el arreglo
    if (.NOT. ALLOCATED(check_array)) then
        return
    else
        DO i = 1, size(check_array)
            if (trim(check_array(i)%id) == id) then
                check_array(i)%posicion_x = posicion_x
                check_array(i)%posicion_y = posicion_y
            end if
        END DO
    end if
end subroutine check_set_posicion

subroutine check_set_grupo(id, grupo)
    CHARACTER(LEN=*), INTENT(IN) :: id
    CHARACTER(LEN=*), INTENT(IN) :: grupo
    integer :: i

    ! Verifica si la memoria ha sido asignada para el arreglo
    if (.NOT. ALLOCATED(check_array)) then
        return
    else
        DO i = 1, size(check_array)
            if (trim(check_array(i)%id) == id) then
                check_array(i)%grupo = grupo
            end if
        END DO
    end if

end subroutine check_set_grupo

subroutine check_set_marcado(id, marcado)
    CHARACTER(LEN=*), INTENT(IN) :: id
    CHARACTER(LEN=*), INTENT(IN) :: marcado
    integer :: i

    ! Verifica si la memoria ha sido asignada para el arreglo
    if (.NOT. ALLOCATED(check_array)) then
        return
    else
        DO i = 1, size(check_array)
            if (trim(check_array(i)%id) == id) then
                check_array(i)%marcado = marcado
            end if
        END DO
    end if

end subroutine check_set_marcado


subroutine imprimir_checks()
    integer :: i, file_unit, ios
    
open (unit=file_unit, file="..\Out\Checks.html", status="replace", action="write", iostat=ios)
    if(ios /= 0)then
        print *, "Errar al abrir el archivo"
    else
        write(file_unit, '(A)') '<!DOCTYPE html>' // NEW_LINE('A')
        write(file_unit, '(A)') '<html><head>' &
         // '<title>Checks</title></head><body>' // NEW_LINE('A')
            DO i = 1, size(check_array)
            write(file_unit, '(A)') '<h2>ID: ', trim(check_array(i)%id), '</h2>'
            write(file_unit, '(A)') '<h2>Tipo: ', trim(check_array(i)%tipo), '</h2>'
            write(file_unit, '(A)') '<h2>Alto: ', trim(check_array(i)%alto), '</h2>'
            write(file_unit, '(A)') '<h2>Ancho: ', trim(check_array(i)%ancho), '</h2>'
            write(file_unit, '(A)') '<h2>Texto: ', trim(check_array(i)%texto), '</h2>'
            write(file_unit, '(A)') '<h2>Posicion X: ', trim(check_array(i)%posicion_x), '</h2>'
            write(file_unit, '(A)') '<h2>Posicion Y: ', trim(check_array(i)%posicion_y), '</h2>'
            write(file_unit, '(A)') '<h2>Alineacion: ', trim(check_array(i)%alineacion), '</h2>'
            write(file_unit, '(A)') '<h2>Marcado: ', trim(check_array(i)%marcado), '</h2>'
            write(file_unit, '(A)') '<h2>Grupo: ', trim(check_array(i)%grupo), '</h2>'
        END DO
        write(file_unit, '(A)') '</body></html>'
        close(file_unit)
    endif
end subroutine imprimir_checks

END MODULE Check