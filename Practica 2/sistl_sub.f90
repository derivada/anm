subroutine sistl_sub(n, a, b, u)

! Subrutina para resolver un sistema de ecuaciones triangular superior Au = b

use mod_clreal
implicit none

integer,intent(in) :: n
integer :: i!,j

real (kind = clreal), intent(in) :: a(n,n)
real (kind = clreal), intent(inout) :: b(n)
real (kind = clreal), intent(out) :: u(n)
!real (kind = clreal):: aux

! u(1) = b(1) / a(1,1)

! Resuelve desde 2 hasta n cada uno de los elementos de la solución
!do i = 2, n
!    aux = 0.
!    do j = 1, i-1
!        aux = aux + a(i,j) * u(j)
!    end do
!    u(i) = (b(i) - aux) / a(i,i)
!end do

! Versión 2 de la subrutina, más eficiente en cuanto a tiempo de cálculo
do i = 1, n
    u(i) = b(i) / a(i, i)
    b(i+1:n) = b(i+1:n) - a(i+1:n, i) * u(i)
end do
end subroutine sistl_sub