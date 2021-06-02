subroutine richardson(n, a, b, u, eps, nitmax, alfa)

    use mod_formatos
    use mod_clreal
    implicit none
    integer, intent(in) :: n, nitmax
    real (kind = clreal), intent(in) :: a(n, n), b(n)
    real (kind=clreal), intent(out) :: u(n)
    real (kind=clreal), intent(in) :: eps, alfa

    integer :: k, i, j
    real (kind = clreal) :: aux, uold(n)

    ! u0 = u
    ! Iteracciones del método de Richardson
    do k = 1, nitmax
        uold = u
        do i = 1, n
            aux = 0.
            do j = 1, n
                aux = aux + a(i, j) * u(j)
            end do
            u(i) = u(i) + alfa * (b(i) - aux)
        end do

        ! Test de error absoluto en norma infinito  
        if(abs(maxval(uold - u)) < eps) then
            print *
            print *, 'Se satisface el test de parada por epsilon tras ', k, ' iteracciones!'
            return
        end if  
    end do
    
    if(k .EQ. nitmax) then
        print *
        print *, 'Alcanzado el número de iteracciones máximas: ', nitmax
        return
    end if
end subroutine