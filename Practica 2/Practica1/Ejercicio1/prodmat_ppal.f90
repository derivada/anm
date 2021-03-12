program prodmat_ppal

    ! compilacion: gfortran -o prodmat.exe prodmat_ppal.f90
    ! ejecucion: .\prodmat.exe < prodmat.txt > prodmat.out

    implicit none
    
    real, allocatable :: a(:, :), b(:, :), c(: , :) ! las matrices a y b
    integer :: n, m, p, i, j, k ! dimensiones de las matrices

    ! especificadores de formato para I/O
    character(len = 4)::formato1='(i4)' ! entero de 4 digitos
    character(len = 10)::formato2='(100e12.4)' ! hasta un máximo de 100 números reales con 12 espacios (cifras o espacio blanco) de las cuales 4 son decimales


    print*, "El numero de filas de A es: "
    read formato1, n
    print formato1, n

    print*, "El numero de columnas de A y filas de B es: "
    read formato1, m
    print formato1, m

    print*, "El numero de columnas de B es: "
    read formato1, p
    print formato1, p

    allocate(a(m,n), b(n,p), c(m,p))

    print*, "Introduzca la matriz a:"
    do i = 1, m
        read *, a(i, :)
        print formato2, a(i, :)
    end do

    print*, "Introduzca la matriz b:"
    do i = 1, n
        read *, b(i, :)
        print formato2, b(i, :)
    end do

    ! producto matricial (procedimiento propio)
    do i = 1, m
        do j = 1, p
            do k = 1, n
                c(i,j) = c(i,j) + (a(i,k) * b(k,j))
            end do
        end do
    end do

    print *, "Producto matricial axb usando procedimiento propio: "
    do i = 1, m
        print formato2, c(i, :)
    end do
    
    c = matmul(a,b)
    print *, "Producto matricial axb usando funcion MATMUL: "
    do i = 1, m
        print formato2, c(i, :)
    end do

    ! una forma más rápida (tratando matrices como escalares!)

    a = a + b ! almacenar en a la suma a+b elemento a elemento
    ! o también
    ! a = a + 1. (suma 1 a cada elemento)
    ! a = a * b (producto no matricial, sino elemento a elemento)

    deallocate(a, b, c)

end program
