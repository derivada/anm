subroutine residuo(m, n, a, b, u)

! Subrutina para calcular el residuo r = Au-b del sistema lineal Au = b
! Versión 3 - Más eficiente en el tiempo de cálculo
! Se emiten además los vectores aux y r

implicit none

integer,intent(in) :: m, n
real, intent(in) :: a(m,n),  u(n)
real, intent(inout) :: b(m) ! El vector b se usa ahora también para almacenar el resultado

integer :: j

! Aquí se usa de nuevo la sintaxis para referirse a varias filas o columnas de cada vector

do j = 1, n ! Bucle sobre las columnas de A
    b(:) = b(:) - a(:,j) * u(j) ! Explicación: r = Au - b ==> -r = b - Au !!! ==> -b' = b - Au
end do

b = -b ! Invertimos el signo para obtener el resultado final, y ya lo tenemos b' = r = Au - b

end subroutine residuo