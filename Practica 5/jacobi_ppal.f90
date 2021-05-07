program jacobi_ppal

    ! Programa principal para el método iterativo de Jacobi para la resolución de S.E.L.
    ! compilacion: gfortran -o jacobi.exe jacobi_ppal.f90 jacobi.f90 initer.f90 residuo.f90
    ! ejecucion: .\jacobi.exe < jacobi.txt > jacobi.out

    use mod_clreal
    use mod_formatos
    implicit none
    
    integer :: n ! Orden de la matriz, número de iteracciones máximas
    integer :: nitmax
    ! Matriz A, vector B, iterante inicial u, vector de residuo r
    real(kind = clreal), allocatable :: a(:,:), b(:), u(:), r(:) 
    real(kind = clreal) :: eps ! Test de parada

    print *, '--- Cálculo de la factorización de Jacobi para la resolución iterative un S.E.L. ---'
    
    ! Leer tamaño de la matriz
    print *, 'El orden n de la matriz A es: '
    read entero, n
    print entero, n
    
    allocate(a(n, n), b(n), u(n), r(n)) ! Alojamiento de memoria
    
    call initer(n, a, b, u, eps, nitmax) ! Lectura de datos
    
    call jacobi(n, a, b, u, eps, nitmax) ! Llamar a la subrutina que resuelve el sistema
    print*
    print*, 'La solución aproximada del sistema es: '
    print floats, u

   
    call residuo(n, n, a, b, u, r)

    ! Escritura del residuo del sistema
    print*
    print*, 'El residuo del sistema r = Ax-b es: '
    print floats, r
    print*

    deallocate(a, b, u, r)
end program
