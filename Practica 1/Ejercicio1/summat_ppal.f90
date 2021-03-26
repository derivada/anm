program summat_ppal

    ! compilacion: gfortran -o summat.exe summat_ppal.f90
    ! ejecucion: .\summat.exe < summat.txt > summat.out

    implicit none
    
    real, allocatable :: a(:, :), b(:, :), c(: , :) ! las matrices a y b
    integer :: n, m, i, j ! dimensiones de las matrices
    print*, "El numero de filas es: "
    read *, n
    print *, n

    print*, "El numero de columnas es: "
    read *, m
    print*, m
    
    if(n /= m) then
        print *, "Matrices no adecuadas (no cuadradas)"
        stop
    end if

    allocate(a(n,m), b(n,m), c(n,m))

    print*, "Introduzca la matriz a:"
    do i = 1, n
        read*,a(i, :)
        print*,a(i, :)
    end do

    print*, "Introduzca la matriz b:"
    do i = 1, n
        read*,b(i, :)
        print*,b(i, :)
    end do

    ! suma
    do i = 1, n
        do j = 1, m
            c(i,j) = a(i,j) + b(i,j)
        end do
    end do

    ! imprimir suma  
    print*, "Suma de las matrices a+b: "
    do i = 1, n
        print*,c(i, :)
    end do
    
    ! una forma más rápida (tratando matrices como escalares!)

    a = a + b ! almacenar en a la suma a+b elemento a elemento
    ! o también
    ! a = a + 1. (suma 1 a cada elemento)
    ! a = a * b (producto no matricial, sino elemento a elemento)

    deallocate(a, b, c)

end program
