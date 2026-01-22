MODULE texto
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

type(Tag), ALLOCATABLE :: texto_array(:)

contains
subroutine agregar_texto(id)
    CHARACTER(LEN=*), INTENT(IN) :: id

    Type(Tag) :: nuevo_texto
    integer :: n
    type(Tag), ALLOCATABLE :: temp_array(:)
    
    nuevo_texto%id = id
    nuevo_texto%tipo = 'Texto'
    nuevo_texto%alto = "25"
    nuevo_texto%ancho = "100"
    nuevo_texto%texto = ""
    nuevo_texto%alineacion = "left"
    nuevo_texto%posicion_x = ""
    nuevo_texto%posicion_y = ""
    nuevo_texto%grupo = ""
    if(.NOT. ALLOCATED(texto_array))then
        ALLOCATE(texto_array(1))
        texto_array(1) = nuevo_texto
    else
        n = SIZE(texto_array)
        ALLOCATE(temp_array(n+1))
        temp_array(:n) = texto_array
        temp_array(n+1) = nuevo_texto
        DEALLOCATE(texto_array)
        ALLOCATE(texto_array(n+1))
        texto_array = temp_array
    endif

end subroutine agregar_texto

subroutine texto_set_texto(id, texto)
    CHARACTER(LEN=*), INTENT(IN) :: id
    CHARACTER(LEN=*), INTENT(IN) :: texto
    integer :: i

    ! Verifica si la memoria ha sido asignada para el arreglo
    if (.NOT. ALLOCATED(texto_array)) then
        return
    else
        DO i = 1, size(texto_array)
            if (trim(texto_array(i)%id) == id) then
                texto_array(i)%texto = texto
            end if
        END DO
    end if

end subroutine texto_set_texto

subroutine texto_set_posicion(id, posicion_x, posicion_y)
    CHARACTER(LEN=*), INTENT(IN) :: id
    CHARACTER(LEN=*), INTENT(IN) :: posicion_x
    CHARACTER(LEN=*), INTENT(IN) :: posicion_y
    integer :: i

    ! Verifica si la memoria ha sido asignada para el arreglo
    if (.NOT. ALLOCATED(texto_array)) then
        return
    else
        DO i = 1, size(texto_array)
            if (trim(texto_array(i)%id) == id) then
                texto_array(i)%posicion_x = posicion_x
                texto_array(i)%posicion_y = posicion_y
            end if
        END DO
    end if
end subroutine texto_set_posicion

subroutine texto_set_grupo(id, grupo)
    CHARACTER(LEN=*), INTENT(IN) :: id
    CHARACTER(LEN=*), INTENT(IN) :: grupo
    integer :: i

    ! Verifica si la memoria ha sido asignada para el arreglo
    if (.NOT. ALLOCATED(texto_array)) then
        return
    else
        DO i = 1, size(texto_array)
            if (trim(texto_array(i)%id) == id) then
                texto_array(i)%grupo = grupo
            end if
        END DO
    end if

end subroutine texto_set_grupo

subroutine imprimir_textos()
    integer :: i, file_unit, ios
    
open (unit=file_unit, file="..\Out\Textos.html", status="replace", action="write", iostat=ios)
    if(ios /= 0)then
        print *, "Errar al abrir el archivo"
    else
        write(file_unit, '(A)') '<!DOCTYPE html>' // NEW_LINE('A')
        write(file_unit, '(A)') '<html><head>' &
         // '<title>Textos</title></head><body>' // NEW_LINE('A')
            DO i = 1, size(texto_array)
            write(file_unit, '(A)') '<h2>ID: ', trim(texto_array(i)%id), '</h2>'
            write(file_unit, '(A)') '<h2>Tipo: ', trim(texto_array(i)%tipo), '</h2>'
            write(file_unit, '(A)') '<h2>Alto: ', trim(texto_array(i)%alto), '</h2>'
            write(file_unit, '(A)') '<h2>Ancho: ', trim(texto_array(i)%ancho), '</h2>'
            write(file_unit, '(A)') '<h2>Texto: ', trim(texto_array(i)%texto), '</h2>'
            write(file_unit, '(A)') '<h2>Posicion X: ', trim(texto_array(i)%posicion_x), '</h2>'
            write(file_unit, '(A)') '<h2>Posicion Y: ', trim(texto_array(i)%posicion_y), '</h2>'
            write(file_unit, '(A)') '<h2>Alineacion: ', trim(texto_array(i)%alineacion), '</h2>'
            write(file_unit, '(A)') '<h2>Grupo: ', trim(texto_array(i)%grupo), '</h2>'
        END DO
        write(file_unit, '(A)') '</body></html>'
        close(file_unit)
    endif
end subroutine imprimir_textos

END MODULE texto