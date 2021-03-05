subroutine sistu_sub(n, a, b, u)

! Subrutina para resolver un sistema de ecuaciones triangular superior Au = b

use mod_clreal
implicit none

integer,intent(in) :: n
integer :: i,j

real (kind = clreal), intent(in) :: a(n,n), b(n)
real (kind = clreal), intent(out) :: u(n)
real (kind = clreal):: aux


u(n) = b(n) / a(n,n)

! Resuelve desde n hasta 1 cada uno de los elementos de la solución
do i = n-1, 1, -1
    aux = 0.
    do j = i+1, n
        aux = aux + a(i,j) * u(j)
    end do
    u(i) = (b(i) - aux) / a(i,i)
end do

end subroutine sistu_sub