MODULE clave
    IMPLICIT NONE
    type :: Tag
    CHARACTER(LEN=50) :: id
    CHARACTER(LEN=20) :: tipo
    CHARACTER(LEN=20) :: alto 
    CHARACTER(LEN=20) :: ancho
    CHARACTER(LEN=200) :: texto 
    CHARACTER(LEN=50) :: alineacion
    CHARACTER(LEN=50) :: posicion_x
    CHARACTER(LEN=50) :: posicion_y
    CHARACTER(LEN=50) :: grupo
END TYPE Tag

type(Tag), ALLOCATABLE :: clave_array(:)

contains
subroutine agregar_clave(id)
    CHARACTER(LEN=*), INTENT(IN) :: id

    Type(Tag) :: nuevo_clave
    integer :: n
    type(Tag), ALLOCATABLE :: temp_array(:)
    
    nuevo_clave%id = id
    nuevo_clave%tipo = 'Clave'
    nuevo_clave%alto = "25"
    nuevo_clave%ancho = "100"
    nuevo_clave%texto = ""
    nuevo_clave%alineacion = "left"
    nuevo_clave%posicion_x = ""
    nuevo_clave%posicion_y = ""
    nuevo_clave%grupo = ""
    if(.NOT. ALLOCATED(clave_array))then
        ALLOCATE(clave_array(1))
        clave_array(1) = nuevo_clave
    else
        n = SIZE(clave_array)
        ALLOCATE(temp_array(n+1))
        temp_array(:n) = clave_array
        temp_array(n+1) = nuevo_clave
        DEALLOCATE(clave_array)
        ALLOCATE(clave_array(n+1))
        clave_array = temp_array
    endif

end subroutine agregar_clave

subroutine clave_set_texto(id, texto)
    CHARACTER(LEN=*), INTENT(IN) :: id
    CHARACTER(LEN=*), INTENT(IN) :: texto
    integer :: i

    ! Verifica si la memoria ha sido asignada para el arreglo
    if (.NOT. ALLOCATED(clave_array)) then
        return
    else
        DO i = 1, size(clave_array)
            if (trim(clave_array(i)%id) == id) then
                clave_array(i)%texto = texto
            end if
        END DO
    end if

end subroutine clave_set_texto

subroutine clave_set_posicion(id, posicion_x, posicion_y)
    CHARACTER(LEN=*), INTENT(IN) :: id
    CHARACTER(LEN=*), INTENT(IN) :: posicion_x
    CHARACTER(LEN=*), INTENT(IN) :: posicion_y
    integer :: i

    ! Verifica si la memoria ha sido asignada para el arreglo
    if (.NOT. ALLOCATED(clave_array)) then
        return
    else
        DO i = 1, size(clave_array)
            if (trim(clave_array(i)%id) == id) then
                clave_array(i)%posicion_x = posicion_x
                clave_array(i)%posicion_y = posicion_y
            end if
        END DO
    end if
end subroutine clave_set_posicion

subroutine clave_set_grupo(id, grupo)
    CHARACTER(LEN=*), INTENT(IN) :: id
    CHARACTER(LEN=*), INTENT(IN) :: grupo
    integer :: i

    ! Verifica si la memoria ha sido asignada para el arreglo
    if (.NOT. ALLOCATED(clave_array)) then
        return
    else
        DO i = 1, size(clave_array)
            if (trim(clave_array(i)%id) == id) then
                clave_array(i)%grupo = grupo
            end if
        END DO
    end if
end subroutine clave_set_grupo

subroutine imprimir_claves()
    integer :: i, file_unit, ios
    
open (unit=file_unit, file="..\Out\Claves.html", status="replace", action="write", iostat=ios)
    if(ios /= 0)then
        print *, "Errar al abrir el archivo"
    else
        write(file_unit, '(A)') '<!DOCTYPE html>' // NEW_LINE('A')
        write(file_unit, '(A)') '<html><head>' &
         // '<title>Claves</title></head><body>' // NEW_LINE('A')
            DO i = 1, size(clave_array)
            write(file_unit, '(A)') '<h2>ID: ', trim(clave_array(i)%id), '</h2>'
            write(file_unit, '(A)') '<h2>Tipo: ', trim(clave_array(i)%tipo), '</h2>'
            write(file_unit, '(A)') '<h2>Alto: ', trim(clave_array(i)%alto), '</h2>'
            write(file_unit, '(A)') '<h2>Ancho: ', trim(clave_array(i)%ancho), '</h2>'
            write(file_unit, '(A)') '<h2>Texto: ', trim(clave_array(i)%texto), '</h2>'
            write(file_unit, '(A)') '<h2>Posicion X: ', trim(clave_array(i)%posicion_x), '</h2>'
            write(file_unit, '(A)') '<h2>Posicion Y: ', trim(clave_array(i)%posicion_y), '</h2>'
            write(file_unit, '(A)') '<h2>Alineacion: ', trim(clave_array(i)%alineacion), '</h2>'
            write(file_unit, '(A)') '<h2>Grupo: ', trim(clave_array(i)%grupo), '</h2>'
        END DO
        write(file_unit, '(A)') '</body></html>'
        close(file_unit)
    endif
end subroutine imprimir_claves
END MODULE clave