subroutine sistu(n, a, b, u)

! Subrutina para resolver un sistema de ecuaciones triangular superior Au = b

use mod_clreal
implicit none

integer,intent(in) :: n
integer :: i

real (kind = clreal), intent(in) :: a(n,n)
real (kind = clreal), intent(inout) :: b(n)
real (kind = clreal), intent(out) :: u(n)

do i = n, 1, -1
    u(i) = b(i) / a(i, i)
    b(1:i-1) = b(1:i-1) - a(1:i-1, i) * u(i)
end do

end subroutine sistu