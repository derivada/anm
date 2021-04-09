program lupp_ppal

    ! compilacion: gfortran -o lupp.exe lupp_ppal.f90 lupp.f90 datsis.f90 sistlpf1.f90 sistupf2.f90 residuo.f90
    ! ejecucion: .\lupp.exe < lupp.txt > lupp.out

    use mod_clreal
    use mod_formatos
    implicit none
    
    real (kind = clreal), allocatable :: a(:, :), b(:), aa(:, :), x(:), y(:), r(:)
    real (kind = clreal) :: deter
    integer :: n, i, j
    integer, allocatable :: ip(:)

    print *, '--- Cálculo de la factorización PA = LU y resolución de un sistema de ecuaciones lineales ---'
    
    ! Leer tamaño de la matriz
    print *, 'El orden n de la matriz A es: '
    read entero, n
    print entero, n
    print *
    allocate(a(n, n), b(n), aa(n, n), x(n), y(n), ip(n), r(n)) ! Alojamiento de memoria
   
    call datsis(n, a, b) ! Lectura de datos
    
    ! Copiar a para uso posterior
    aa = a

    call lupp(n, a, ip, deter) ! Llamar a la subrutina que resuelve el sistema
    
    print *, 'Determinante: ', deter
    print *
    print *, 'La matriz triangular inferior L es: '
    do i = 1, n
        print floats, a (ip(i), 1:i-1), 1., (0., j = i+1, n) 
    end do
    print *
    print *, 'La matriz triangular superior U es: '
    do i = 1, n
        print floats, (0., j = 1, i-1), a (ip(i), i:n)
    end do
    print *, 'El vector ip es: '
    print enteros, ip
    print *

    ! Cálculo de la solución del S.E.L. mediante la resolución consecutiva del
    ! triangular inferior y el triangular superior
    
    ! Sea LUx = Pb ==> 1. Ly = Pb obtenemos y, 2. Ux = y obtenemos x solución final
    
    call sistlpf1(n, a, b, y, ip) ! Ly = Pb
    call sistupf2(n, a, y, x, ip) ! Ux = y

    print *, 'La solución del sistema es: '
    print floats, x   
    print *

    call residuo(n, n, aa, b, x, r)

    ! Escritura del residuo del sistema

    print *, 'El residuo del sistema r = Ax-b es: '
    print floats, r
    print *

    deallocate(a, b, aa, x, y, r, ip)

end program lupp_ppal