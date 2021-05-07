subroutine initer (n, a, b, u, eps, nitmax)

    ! Lectura de datos p ara los métodos de Jacobi, Gauss-Seidel y Relajación
    
    use mod_clreal
    use mod_formatos
    implicit none

    ! Argumentos

    integer, intent(in) :: n ! Orden de la matriz, número de iteracciones máximas
    integer, intent(out) :: nitmax
    real(kind = clreal), intent(out), dimension(n, n) :: a ! Matriz A
    real(kind = clreal), intent(out), dimension(n) :: b, u ! Término independiente b, iterate inicial u

    real(kind = clreal), intent(out) :: eps ! Test de parada
    integer :: i
    
    print*
    print*, 'La matriz de coeficientes A es: '
    do i=1, n
        read*, a(i, :)
        print floats, a(i, :)
    end do

    print*
    print*, 'El término independiente b es: '
    read*, b
    print floats, b

    print*
    print*, 'El iterante inicial u es: '
    read*, u
    print floats, u

    print*
    print*, 'El parámetro eps para el test de parada es:'
    read*, eps
    print*, eps

    print*
    print*, 'El número máximo de iteracciones permitidas es: '
    read*, nitmax
    print entero, nitmax

end subroutine initer