subroutine sistl1(n, a, b, u)

! Subrutina para resolver un sistema de ecuaciones triangular inferior 
! Au = b con diagonal de 1s

use mod_clreal
implicit none

integer,intent(in) :: n
integer :: i

real (kind = clreal), intent(in) :: a(n,n)
real (kind = clreal), intent(inout) :: b(n)
real (kind = clreal), intent(out) :: u(n)

do i = 1, n
    u(i) = b(i) !/ a(i, i) 
                ! esto no es por razones de eficiencia, sino porque en la matriz A
                ! la diagonal principal es parte de U y no de L
    b(i+1:n) = b(i+1:n) - a(i+1:n, i) * u(i)
end do
end subroutine sistl1