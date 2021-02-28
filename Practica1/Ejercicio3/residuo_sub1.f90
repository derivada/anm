subroutine residuo(m, n, a, b, u, r)

! Subrutina para calcular el residuo r = Au-b del sistema lineal Au = b
! Versión 1 - Transcripción a Fortran de las fórmulas matemáticas de forma simple

implicit none

integer,intent(in) :: m, n
real, intent(in) :: a(m,n), b(m), u(n)
real, intent(out) :: r(m)

integer :: i,j
real :: aux

! Doble bucle sobre cada elemento
do i = 1, m
    aux = 0
    do j = 1, n
        aux = aux + a(i,j) * u(j)
    end do
    r(i) = aux - b(i) ! Cálculo del residuo r = Au - b
end do

end subroutine residuo