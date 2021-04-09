subroutine lupp(n, a, ip, deter)
    use mod_clreal
    implicit none
    integer, intent(in) :: n
    integer :: i, j, k, cont, ipiv, ipk, ipi
    integer, intent(inout) :: ip(n)
    real (kind = clreal), intent(inout) :: a(n, n)
    real (kind = clreal), intent(out) ::  deter
    real (kind = clreal) :: piv

    deter = 1. ! Inicialización del determinante
    ip = (/(i, i = 1, n)/) ! Inicializacion de la permutacion de filas
    cont = 0 ! Inicializacion del contador de cambios de filas

    ! Etapa k-ésima de eliminación
    do k = 1, n-1
        ! Búsqueda del pivote y de la fila en que se encuentra
        piv = a(ip(k), k)
        ipiv = k
        do i = k+1, n
            if(abs(piv) < abs(a(ip(i), k))) then
                piv = a(ip(i),k)
                ipiv = i
            end if
        end do

        if(abs(piv) < 1.e-12) then ! Si el pivote es nulo terminar
            print*, 'Pivote nulo en la etapa: ', n
            print*, 'La matriz del sistema es singular!'
            stop
        end if
        
        ! Poner al día la permutación y el contador de cambios de filas,
        ! si el pivote no está en la fila k
        if(ipiv /= k) then
            ipk = ip(ipiv)
            ip(ipiv) = ip(k)
            ip(k) = ipk
            cont = cont+1
        else
            ipk = ip(k)
        end if

        deter = deter*piv ! Actualización del determinante

        ! Eliminación gaussiana
        do i = k+1, n
            ipi = ip(i)
            a(ipi, k) = a(ipi, k)/piv
            do j = k+1, n
                a(ipi, j) = a(ipi, j)-a(ipi, k)*a(ipk, j)
            end do
        end do
    end do

    ! Comprobación de que el último pivote no es nulo
    piv = a(ip(n), n)
    if(abs(piv) < 1.e-12) then
        print*, 'Pivote nulo en la etapa: ', n
        print*, 'La matriz del sistema es singular!'
        stop
    end if

    ! Fin del cálculo del determinante
    deter = deter*piv*(-1)**cont

end subroutine lupp