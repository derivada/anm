subroutine sistupf2(n, a, b, u, ip)

! Subrutina para resolver un sistema de ecuaciones triangular superior Au = b
! salvo permutación de filas ip

use mod_clreal
implicit none

integer, intent(in) :: n
integer, intent(in) :: ip(n)
integer :: i, j

real (kind = clreal), intent(in) :: a(n,n), b(n)
real (kind = clreal), intent(out) :: u(n)

real (kind = clreal) :: aux

do i = n, 1, -1
    aux = 0.
    do j = i+1, n
        aux = aux + a(ip(i), j) * u(j)
    end do
    u(i) = (b(i) - aux) / a(ip(i), i)
end do

end subroutine sistupf2