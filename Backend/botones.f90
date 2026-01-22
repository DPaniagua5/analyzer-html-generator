MODULE botones  
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

type(Tag), ALLOCATABLE :: boton_array(:)

contains
subroutine agregar_boton(id)
    CHARACTER(LEN=*), INTENT(IN) :: id

    Type(Tag) :: nuevo_boton
    integer :: n
    type(Tag), ALLOCATABLE :: temp_array(:)
    
    nuevo_boton%id = id
    nuevo_boton%tipo = 'Texto'
    nuevo_boton%alto = "25"
    nuevo_boton%ancho = "100"
    nuevo_boton%texto = ""
    nuevo_boton%alineacion = "left"
    nuevo_boton%posicion_x = ""
    nuevo_boton%posicion_y = ""
    nuevo_boton%grupo = ""
    if(.NOT. ALLOCATED(boton_array))then
        ALLOCATE(boton_array(1))
        boton_array(1) = nuevo_boton
    else
        n = SIZE(boton_array)
        ALLOCATE(temp_array(n+1))
        temp_array(:n) = boton_array
        temp_array(n+1) = nuevo_boton
        DEALLOCATE(boton_array)
        ALLOCATE(boton_array(n+1))
        boton_array = temp_array
    endif

end subroutine agregar_boton

subroutine boton_set_texto(id, texto)
    CHARACTER(LEN=*), INTENT(IN) :: id
    CHARACTER(LEN=*), INTENT(IN) :: texto
    integer :: i

    ! Verifica si la memoria ha sido asignada para el arreglo
    if (.NOT. ALLOCATED(boton_array)) then
        return
    else
        DO i = 1, size(boton_array)
            if (trim(boton_array(i)%id) == id) then
                boton_array(i)%texto = texto
            end if
        END DO
    end if

end subroutine boton_set_texto

subroutine boton_set_posicion(id, posicion_x, posicion_y)
    CHARACTER(LEN=*), INTENT(IN) :: id
    CHARACTER(LEN=*), INTENT(IN) :: posicion_x
    CHARACTER(LEN=*), INTENT(IN) :: posicion_y
    integer :: i

    ! Verifica si la memoria ha sido asignada para el arreglo
    if (.NOT. ALLOCATED(boton_array)) then
        return
    else
        DO i = 1, size(boton_array)
            if (trim(boton_array(i)%id) == id) then
                boton_array(i)%posicion_x = posicion_x
                boton_array(i)%posicion_y = posicion_y
            end if
        END DO
    end if
end subroutine boton_set_posicion

subroutine boton_set_grupo(id, grupo)
    CHARACTER(LEN=*), INTENT(IN) :: id
    CHARACTER(LEN=*), INTENT(IN) :: grupo
    integer :: i

    ! Verifica si la memoria ha sido asignada para el arreglo
    if (.NOT. ALLOCATED(boton_array)) then
        return
    else
        DO i = 1, size(boton_array)
            if (trim(boton_array(i)%id) == id) then
                boton_array(i)%grupo = grupo
            end if
        END DO
    end if

end subroutine boton_set_grupo

subroutine imprimir_botones()
    integer :: i, file_unit, ios
    
open (unit=file_unit, file="..\Out\botones.html", status="replace", action="write", iostat=ios)
    if(ios /= 0)then
        print *, "Errar al abrir el archivo"
    else
        write(file_unit, '(A)') '<!DOCTYPE html>' // NEW_LINE('A')
        write(file_unit, '(A)') '<html><head>' &
         // '<title>Botones</title></head><body>' // NEW_LINE('A')
            DO i = 1, size(boton_array)
            write(file_unit, '(A)') '<h2>ID: ', trim(boton_array(i)%id), '</h2>'
            write(file_unit, '(A)') '<h2>Tipo: ', trim(boton_array(i)%tipo), '</h2>'
            write(file_unit, '(A)') '<h2>Alto: ', trim(boton_array(i)%alto), '</h2>'
            write(file_unit, '(A)') '<h2>Ancho: ', trim(boton_array(i)%ancho), '</h2>'
            write(file_unit, '(A)') '<h2>Texto: ', trim(boton_array(i)%texto), '</h2>'
            write(file_unit, '(A)') '<h2>Posicion X: ', trim(boton_array(i)%posicion_x), '</h2>'
            write(file_unit, '(A)') '<h2>Posicion Y: ', trim(boton_array(i)%posicion_y), '</h2>'
            write(file_unit, '(A)') '<h2>Alineacion: ', trim(boton_array(i)%alineacion), '</h2>'
            write(file_unit, '(A)') '<h2>Grupo: ', trim(boton_array(i)%grupo), '</h2>'
        END DO
        write(file_unit, '(A)') '</body></html>'
        close(file_unit)
    endif
end subroutine imprimir_botones
END MODULE botones