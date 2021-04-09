subroutine sistlpf1(n, a, b, u, ip)

! Subrutina para resolver un sistema de ecuaciones triangular inferior 
! Au = b con diagonal de 1s salvo la permutación de filas ip

use mod_clreal
implicit none

integer, intent(in) :: n
integer, intent(in) :: ip(n)
integer :: i, j

real (kind = clreal), intent(in) :: a(n,n)
real (kind = clreal), intent(inout) :: b(n)
real (kind = clreal), intent(out) :: u(n)
real (kind = clreal) :: aux

u(1) = b(ip(1))

do i = 2, n
    aux = 0.
    do j = 1, i-1
        aux = aux + a(ip(i), j) * u(j)
    end do
    u(i) = b(ip(i)) - aux
end do

end subroutine sistlpf1