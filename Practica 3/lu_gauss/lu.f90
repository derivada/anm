subroutine lu(n, a, deter)
use mod_clreal
implicit none
integer, intent(in) :: n
integer :: i, j, k
real (kind = clreal), intent(inout) :: a(n, n)
real (kind = clreal), intent(out) ::  deter
real (kind = clreal) :: piv

deter=1. ! Inicialización del determinante

    ! Etapa k-ésima de eliminación
    do k = 1, n-1
        piv = a(k, k)
        if(abs(piv) < 1.e-12) then ! Si el pivote es nulo terminar
            print *, 'Pivote nulo en la etapa: ', k
            stop
        end if

        deter = deter * piv ! Actualización del determinante

        ! Eliminación gaussiana
        do i = k+1, n
           a(i, k) = a(i, k) / piv ! Se guarda ya el factor en a(i, k) para LU
            do j = k+1 ,n
                a(i,j) = a(i,j) - a(i, k) * a(k, j)
            end do
        end do

        ! Eliminación gaussiana más eficiente
        ! a(k+1:n, k) = a(k+1:n, k) / piv
        ! do j=k+1,n
        !     a(k+1:n, j) = a(k+1:n, j) - a(k+1:n, k) * a(k, j)
        ! end do
    end do

    ! Comprobación de que el último pivote no es nulo
    if(abs(a(n, n)) < 1.e-12) then
        print*, 'Pivote nulo en la etapa: ', n
        stop
    end if

    ! Fin del cálculo del determinante
    deter = deter * a(n, n)

end subroutine lu