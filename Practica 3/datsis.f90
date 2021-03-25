subroutine datsis(n, a, b)
    use mod_clreal 
    implicit none

    integer, intent(in) :: n
    integer :: i
    real (kind = clreal), intent(out) :: a(n, n)
    real (kind = clreal), intent(out) :: b(n)

    character(len = 10) :: floats = '(100e12.4)' ! Hasta 100 floats en notación científica con 4 decimales

    print *, n
    print *, 'Introduzca la matriz A: '
    do i = 1, n! Leer matriz A
        read *, a(i, :)
        print floats, a(i, :)
    end do
    
    print *, 'Introduzca el vector B: '
    read *, b ! Leer vector B
    print floats, b

end subroutine datsis