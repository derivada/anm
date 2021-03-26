subroutine datsis(n, a, b)
    use mod_clreal
    use mod_formatos 
    implicit none

    integer, intent(in) :: n
    integer :: i
    real (kind = clreal), intent(out) :: a(n, n)
    real (kind = clreal), intent(out) :: b(n)

    print *, 'Introduzca la matriz A: '
    do i = 1, n! Leer matriz A
        read *, a(i, :)
        print floats, a(i, :)
    end do
    print *

    print *, 'Introduzca el vector B: '
    read *, b ! Leer vector B
    print floats, b
    print *
end subroutine datsis