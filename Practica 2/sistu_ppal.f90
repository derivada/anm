program sistu_ppal
    
    ! compilacion: gfortran -o sistu.exe sistu_ppal.f90 sistu_sub.f90
    ! ejecucion: .\sistu.exe < sistu.txt > sistu.out

    use mod_clreal 
    implicit none
    
    ! especificadores de formato para I/O
    character(len = 10) :: entero = '(i4)' ! 1 enteros de hasta 4 cifras
    character(len = 10) :: floats = '(100e12.4)' ! Hasta 100 floats en notación científica con 4 decimales

    real (kind = clreal), allocatable :: a(:,:), b(:), u(:) ! La matriz A (mxn), los vectores b y u y el residuo a calcular r

    integer :: n ! La dimensión de la matriz cuadrada A
    integer :: i, j

    ! Lectura de datos
    print *, 'Introduzca la dimensión de la matriz:'
    read entero, n
    print entero, n
    
    allocate(a(n,n), b(n), u(n))

    print *, "Introduzca la matriz triangular superior A (no hace falta meter los 0s):"

    ! vamos a leer desde la diagonal hacia delante
    do i = 1, n
        read *, a(i, i:n)
        if (abs(a(i,i)) < 1.e-12) then
            print *, 'Matriz singular no válida'
            stop
        end if
        print floats, (0._clreal, j=1, i-1), a(i, i:n)  
    end do

    print *, "Introduzca el vector de términos independientes b:"
    read *, b 
    print floats, b

    ! Llamada a la subrutina que resuelve el sistema triangular
    call sistu_sub(n, a, b, u) 

    ! Impresión del resultado
    print *
    print *, 'El resultado u del sistema Au = b es:'
    print floats, u

    deallocate(a, b, u)

end program sistu_ppal