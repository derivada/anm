subroutine cholesky(n, a, deter)
        
    ! compilación: gfortran -o cholesky.exe cholesky.f95 cholesky_ppal.f95 datsissim.f95 residuosim.f95 sistl.f95 sistusim.f95
    ! ejecución: .\cholesky.exe < cholesky.txt > cholesky.out

    ! Programa para realizar la factorización de Cholesky 

    use mod_clreal
    implicit none

    ! Declaración de varibles
    integer, intent(in) :: n
    real(kind = clreal), intent(inout) :: a(n, n)
    real(kind = clreal), intent(out) :: deter
    integer ::  i, j ! Variables locales
    
    deter = 1.

    do j = 1, n
        ! Elemento diagonal de B
        a(j, j) = a(j, j) - sum(a(j, 1:j - 1) * a(j, 1:j - 1))
        
        ! La matriz tiene que ser definida positiva
        if (a(j, j) < 1.e-12) then
            print*
            print*, 'Radicando ', j, '<=0 en la matriz B, '
            print*, 'La matriz del sistema no es definida positiva!'
            stop
        end if
        
        ! Si llega aquí es que el radicando es mayor que 0
        a(j, j) = sqrt(a(j, j))
        ! Bucle de filas
        do i = j+1, n
            a(i, j) = a(i, j) - sum(a(i, 1:j-1) * a(j, 1:j-1))
            a(i, j) = a(i, j) / a(j, j)
        end do
        
        deter = deter * a(j, j)
    end do
    
    ! Fin del cálculo del determinante
    deter = deter**2

end subroutine cholesky

















