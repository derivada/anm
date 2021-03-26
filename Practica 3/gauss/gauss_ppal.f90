program gauss_ppal

    ! compilacion: gfortran -o gauss.exe gauss_ppal.f90 gauss.f90 datsis.f90 sistu.f90 residuo.f90
    ! ejecucion: .\gauss.exe < gauss.txt > gauss.out

    use mod_clreal
    use mod_formatos
    implicit none
    
    real (kind = clreal), allocatable :: a(:, :), b(:), aa(:, :), bb(:), u(:), r(:)
    real (kind = clreal) :: deter
    integer :: n, i, j

    print *, '--- Resolución mediante el método de Gauss de un sistema de ecuaciones lineales ---'

    ! Leer tamaño de la matriz
    print *, "El orden n de la matriz A es: "
    read entero, n
    print entero, n
    print *
    allocate(a(n, n), b(n), aa(n, n), bb(n), u(n), r(n)) ! Alojamiento de memoria
   
    call datsis(n, a, b) ! Lectura de datos
    
    aa = a
    bb = b

    ! Copiar a y b para uso posterior
    call gauss(n, a, b, deter) ! Llamar a la subrutina que resuelve el sistema
    print *, 'Determinante: ', deter
    print *
    print *, 'La matriz triangular superior U es: '
    do i = 1, n
        print floats, (0., j = 1, i-1), a (i, i:n)
    end do
    print *
    print *, 'El término independiente transformado es: '
    print floats, b
    print *

    ! Cálculo de la solución del S.E.L. triangular superior equivalente
    call sistu(n, a, b, u)

    print *, 'La solución del sistema es: '
    print floats, u    
    print *

    call residuo(n, n, aa, bb, u, r)

    ! Escritura del residuo del ssitema

    print *, 'El residuo del sistema r = Au-b es: '
    print floats, r
    print *

    deallocate(a, b, aa, bb, u, r)

end program gauss_ppal