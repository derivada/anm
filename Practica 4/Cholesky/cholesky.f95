subroutine cholesky(n, a, deter)

    !Programa para realizar la factorización de Cholesky

    use mod_clreal

    implicit none

    integer :: k, i, j
    real(kind = clreal) :: piv, factor

    !Declaración de varibles
    integer, intent(in) :: n
    real(kind = clreal), intent(inout) :: a(n, n)
    real(kind = clreal), intent(out) :: deter




    !La matriz tiene que ser definida positiva
    deter = 1.
    do j = 1, n
        !elemeto diagonal de B
        a(j, j) = a(j, j) - sum(a(j, 1:j - 1) * a(j, 1:j - 1))
        if (a(j, j) < 1.e-12) then
            print*
            print*, '** Radicando ', j, '<=0 en la matriz B, '
            print*, 'La matriz del sistema no es definida positiva!'

            stop
        end if
        
        !Si llega aqui es que el radicando es mayor que 0
        a(j,j) = sqrt(a(j,j))
        !bucle de filas
        do i=j+1,n
            a(i,j) = a(i,j)-sum(a(i, 1:j-1) * a(j,1:j-1))
            a(i,j) = a(i,j)/a(j,j)
        end do
        
        deter=deter*a(j,j)
    end do
    !fin del calculo del determinante
    deter = deter**2


end subroutine cholesky

















