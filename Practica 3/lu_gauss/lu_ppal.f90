program lu_ppal

    ! compilacion: gfortran -o lu.exe lu_ppal.f90 lu.f90 datsis.f90 sistl1.f90 sistu.f90 residuo.f90
    ! ejecucion: .\lu.exe < lu.txt > lu.out

    use mod_clreal
    use mod_formatos
    implicit none
    
    real (kind = clreal), allocatable :: a(:, :), b(:), aa(:, :), bb(:), u(:), r(:)
    real (kind = clreal) :: deter
    integer :: n, i, j

    print *, '--- Cálculo de la factorización A = LU y resolución de un sistema de ecuaciones lineales ---'
    ! Leer tamaño de la matriz
    print *, 'El orden n de la matriz A es: '
    read entero, n
    print entero, n
    print *
    allocate(a(n, n), b(n), aa(n, n), bb(n), u(n), r(n)) ! Alojamiento de memoria
   
    call datsis(n, a, b) ! Lectura de datos
    
    aa = a
    bb = b

    ! Copiar a y b para uso posterior
    call lu(n, a, deter) ! Llamar a la subrutina que resuelve el sistema
    print *, 'Determinante: ', deter
    print *
    print *, 'La matriz triangular inferior L es: '
    do i = 1, n
        print floats, a (i, 1:i-1), 1., (0., j = i+1, n) 
    end do
    print *
    print *, 'La matriz triangular superior U es: '
    do i = 1, n
        print floats, (0., j = 1, i-1), a (i, i:n)
    end do
    print *

    ! Cálculo de la solución del S.E.L. mediante la resolución consecutiva del
    ! triangular inferior y el triangular superior
    
    ! Sea LUx = b ==> 1. Ly = b obtenemos y, 2. Ux = y obtenemos x solución final
    
    call sistl1(n, a, b, u) ! Ly = b
    call sistu(n, a, b, u) ! Ux = y

    print *, 'La solución del sistema es: '
    print floats, u    
    print *

    call residuo(n, n, aa, bb, u, r)

    ! Escritura del residuo del sistema

    print *, 'El residuo del sistema r = Au-b es: '
    print floats, r
    print *

    deallocate(a, b, aa, bb, u, r)

end program lu_ppal