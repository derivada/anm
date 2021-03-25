program gauss_ppal

    ! compilacion: gfortran -o gauss.exe gauss_ppal.f90 gauss.f90 datsis.f90 sistu.f90 residuo.f90
    ! ejecucion: .\gauss.exe < gauss.txt > gauss.out

    use mod_clreal
    implicit none
    
    real (kind = clreal), allocatable :: a(:, :), b(:), aa(:, :), bb(:), u(:), r(:)
    real (kind = clreal) :: deter
    integer :: n, i, j
    character(len = 10) :: floats = '(100e12.4)' ! Hasta 100 floats en notación científica con 4 decimales

    ! Leer tamaño de la matriz
    print *, "El orden n de la matriz A es: "
    read *, n
    print *, n

    allocate(a(n, n), b(n), aa(n, n), bb(n), u(n), r(n)) ! Alojamiento de memoria
   
    call datsis(n, a, b) ! Lectura de datos
    
    aa = a
    bb = b

    ! Copiar a y b para uso posterior
    call gauss(n, a, b, deter) ! Llamar a la subrutina que resuelve el sistema
    print *, 'Determinante: ', deter

    print *, 'La matriz triangular superior U es: '
    do i = 1, n
        print floats, (0., j = 1, i-1), a (i, i:n)
    end do

    print *, 'El término independiente transformado es: '
    print floats, b


    ! Cálculo de la solución del S.E.L. triangular superior equivalente
    call sistu(n, a, b, u)

    print *, 'La solución del sistema es: '
    print floats, u    

    call residuo(n, n, aa, bb, u, r)

    ! Escritura del residuo del ssitema

    print *, 'El residuo del sistema r = Au-b es: '
    print floats, r


    deallocate(a, b, aa, bb, u, r)

end program gauss_ppal