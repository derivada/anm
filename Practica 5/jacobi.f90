subroutine jacobi(n, a, b, u, eps, nitmax)
    
    use mod_formatos
    use mod_clreal 
    implicit none
    
    integer, intent(in) :: n, nitmax ! Orden de la matriz, número de iteracciones máximas
    real(kind = clreal), intent(in), dimension(n, n) :: a ! Matriz de coeficientes
    real(kind = clreal), intent(inout), dimension(n) :: b , u ! Término independiente, iterante, entra el inicial, sale solución
    real(kind = clreal), intent(out) :: eps ! Test de parada, error de la iteración
    
    ! Formatos tabla
    CHARACTER(LEN=64), PARAMETER :: LINEA1 = "(I4, '|', 100e12.4)"
    CHARACTER(LEN=64), PARAMETER :: LINEA2 = "('-------------------------------------')"

    ! Variables locales
    integer :: i, iter
    real (kind = clreal) :: error
    real (kind = clreal), dimension(n) :: uold ! Variable usada para almacenar la iteracción previa
    
    print*

    ! Interrupcion del método si el elemento diagonal i-ésimo es nulo
    do i=1,n
        if(abs(a(i,i)) < 1.e-12)then
            print*, 'El elemento diagonal', i, ' <= 0,'
            print*, 'Se interrumpió el algoritmo!'
            stop
        endif
    enddo

    ! Cálculo de los iterantes
    do iter = 1, nitmax
        uold = u
        do i = 1, n
            u(i) = (b(i) - sum(a(i, 1:i-1) * uold(1:i-1)) & ! el & es para escribir en 2 líneas una larga
            - sum(a(i, i+1:n) * uold(i+1:n))) / a(i, i)
        end do
        
        ! Test de error absoluto en norma infinito
        ! error = maxval(abs(u-uold))

        ! Test de error absoluto en norma 1
        error = 0.
        do i = 1, n
            error = error + abs(u(i) - uold(i))
        end do

        print LINEA1, iter, u
        print *,  'Diferencia en norma 1: ', error

        print LINEA2
        if(error < eps)then
            print*
            print*, 'Test de parada activado en la iteración: ', iter
            return
        endif
    end do
    print*
    print*, 'Efectuadas ', nitmax, ' iteracciones sin que se cumpla el test de parada'
end subroutine jacobi