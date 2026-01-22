MODULE a_texto
    IMPLICIT NONE 
    type :: Tag
    CHARACTER(LEN=50) :: id
    CHARACTER(LEN=20) :: tipo
    CHARACTER(LEN=20) :: alto 
    CHARACTER(LEN=20) :: ancho
    CHARACTER(LEN=200) :: texto 
    CHARACTER(LEN=50) :: posicion_x
    CHARACTER(LEN=50) :: posicion_y                     
    CHARACTER(LEN=50) :: grupo
END TYPE Tag

type(Tag), ALLOCATABLE :: a_texto_array(:)

contains
subroutine agregar_a_texto(id)
    CHARACTER(LEN=*), INTENT(IN) :: id

    Type(Tag) :: nuevo_a_texto
    integer :: n
    type(Tag), ALLOCATABLE :: temp_array(:)
    
    nuevo_a_texto%id = id
    nuevo_a_texto%tipo = 'Texto'
    nuevo_a_texto%alto = "150"
    nuevo_a_texto%ancho = "150"
    nuevo_a_texto%texto = ""
    nuevo_a_texto%posicion_x = ""
    nuevo_a_texto%posicion_y = ""
    nuevo_a_texto%grupo = ""
    if(.NOT. ALLOCATED(a_texto_array))then
        ALLOCATE(a_texto_array(1))
        a_texto_array(1) = nuevo_a_texto
    else
        n = SIZE(a_texto_array)
        ALLOCATE(temp_array(n+1))
        temp_array(:n) = a_texto_array
        temp_array(n+1) = nuevo_a_texto
        DEALLOCATE(a_texto_array)
        ALLOCATE(a_texto_array(n+1))
        a_texto_array = temp_array
    endif

end subroutine agregar_a_texto

subroutine a_texto_set_texto(id, texto)
    CHARACTER(LEN=*), INTENT(IN) :: id
    CHARACTER(LEN=*), INTENT(IN) :: texto
    integer :: i

    ! Verifica si la memoria ha sido asignada para el arreglo
    if (.NOT. ALLOCATED(a_texto_array)) then
        return
    else
        DO i = 1, size(a_texto_array)
            if (trim(a_texto_array(i)%id) == id) then
                a_texto_array(i)%texto = texto
            end if
        END DO
    end if

end subroutine a_texto_set_texto

subroutine a_texto_set_posicion(id, posicion_x, posicion_y)
    CHARACTER(LEN=*), INTENT(IN) :: id
    CHARACTER(LEN=*), INTENT(IN) :: posicion_x
    CHARACTER(LEN=*), INTENT(IN) :: posicion_y
    integer :: i

    ! Verifica si la memoria ha sido asignada para el arreglo
    if (.NOT. ALLOCATED(a_texto_array)) then
        return
    else
        DO i = 1, size(a_texto_array)
            if (trim(a_texto_array(i)%id) == id) then
                a_texto_array(i)%posicion_x = posicion_x
                a_texto_array(i)%posicion_y = posicion_y
            end if
        END DO
    end if
end subroutine a_texto_set_posicion

subroutine a_texto_set_grupo(id, grupo)
    CHARACTER(LEN=*), INTENT(IN) :: id
    CHARACTER(LEN=*), INTENT(IN) :: grupo
    integer :: i

    ! Verifica si la memoria ha sido asignada para el arreglo
    if (.NOT. ALLOCATED(a_texto_array)) then
        return
    else
        DO i = 1, size(a_texto_array)
            if (trim(a_texto_array(i)%id) == id) then
                a_texto_array(i)%grupo = grupo
            end if
        END DO
    end if

end subroutine a_texto_set_grupo

subroutine imprimir_area()
    integer :: i, file_unit, ios
    
open (unit=file_unit, file="..\Out\Area.html", status="replace", action="write", iostat=ios)
    if(ios /= 0)then
        print *, "Errar al abrir el archivo"
    else
        write(file_unit, '(A)') '<!DOCTYPE html>' // NEW_LINE('A')
        write(file_unit, '(A)') '<html><head>' &
         // '<title>Area</title></head><body>' // NEW_LINE('A')
            DO i = 1, size(a_texto_array)
            write(file_unit, '(A)') '<h2>ID: ', trim(a_texto_array(i)%id), '</h2>'
            write(file_unit, '(A)') '<h2>Tipo: ', trim(a_texto_array(i)%tipo), '</h2>'
            write(file_unit, '(A)') '<h2>Alto: ', trim(a_texto_array(i)%alto), '</h2>'
            write(file_unit, '(A)') '<h2>Ancho: ', trim(a_texto_array(i)%ancho), '</h2>'
            write(file_unit, '(A)') '<h2>Texto: ', trim(a_texto_array(i)%texto), '</h2>'
            write(file_unit, '(A)') '<h2>Posicion X: ', trim(a_texto_array(i)%posicion_x), '</h2>'
            write(file_unit, '(A)') '<h2>Posicion Y: ', trim(a_texto_array(i)%posicion_y), '</h2>'
            write(file_unit, '(A)') '<h2>Grupo: ', trim(a_texto_array(i)%grupo), '</h2>'
        END DO
        write(file_unit, '(A)') '</body></html>'
        close(file_unit)
    endif
end subroutine imprimir_area
END MODULE a_texto