subroutine sistu(n, a, b, u)

! Subrutina para resolver un sistema de ecuaciones triangular superior Au = b

use mod_clreal
implicit none

integer,intent(in) :: n
integer :: i, j

real (kind = clreal), intent(in) :: a(n,n)
real (kind = clreal), intent(in) :: b(n)
real (kind = clreal), intent(out) :: u(n)
real (kind = clreal) :: aux

do i = n-1, 1, -1
    aux = 0.
    do j = i+1, n
        aux = aux + a(i,j) * u(j)
    end do
    u(i) = (b(i) - aux) / a(i,i)
end do

end subroutine sistu