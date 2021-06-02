program richardson_ppal
    
    use mod_clreal
    use mod_formatos
    implicit none

    real (kind = clreal), allocatable :: a(:, :), b(:), u(:), r(:), aux(:)
    real (kind = clreal) :: eps, alfa
    integer :: n, nitmax

    integer :: i

    print *, 'Introduzca el tamaño de la matriz del sistema Au = b: '
    read *, n
    print *, n

    allocate(a(n, n), b(n), u(n), r(n), aux(n))
    
    print *
    print *, 'Introduzca la matriz A del sistema: '
    do i = 1, n
        read *, a(i, :)
    end do
    print *, 'A = '
    do i = 1, n
        print floats, a(i, :)
    end do
    
    print *, 'Introduzca el vector b: '
    read *, b
    print *, 'b = '
    print floats, b

    
    print *, 'Introduzca el épsilon del test de parada: '
    read *, eps
    print *, eps

    print *, 'Introduzca el número de iteracciones máximas: '
    read *, nitmax
    print *, nitmax

    print *, 'Introduzca el alfa del método: '
    read *, alfa
    print *, alfa

    ! u0 = (0, 0, 0, ..., 0)
    u = 0.

    ! Datos leidos, llamar metodo
    
    call richardson(n, a, b, u, eps, nitmax, alfa)

    ! Imprimir solución
    print *
    print *, 'Solución del sistema: '
    print floats, u

    ! Calcular residuo
    aux = 0.
    do i = 1,n
        aux = aux + a(:, i) * u(i) ! Suma a aux el producto de una columna de A por un escalar u(j)
    end do
    r = aux - b
    
    print *
    print *, 'Residuo: '
    print floats, r

    deallocate(a, b, u, r, aux)
end program