subroutine residuo(m, n, a, b, u, r)

! Subrutina para calcular el residuo r = Au-b del sistema lineal Au = b
! Versión 2 - Más eficiente en el tiempo de cálculo, ya que las operaciones se hacen
! sobre un bucle en las columnas de A

implicit none

integer,intent(in) :: m, n
real, intent(in) :: a(m,n), b(m), u(n)
real, intent(out) :: r(m)

integer :: j
real :: aux(m)

! Aquí se usa de nuevo la sintaxis para referirse a varias filas o columnas de cada vector
aux = 0. ! aux = (0, 0, 0...)

do j = 1,n
    aux = aux + a(:,j) * u(j) ! Suma a aux el producto de una columna de A por un escalar u(j)
end do

r = aux - b ! Resta de vectores

end subroutine residuo