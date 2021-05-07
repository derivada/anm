program cholesky_tri
    ! Programa principal relativo a la factorización de Cholesky - caso tridiagonal

    ! compilacion: gfortran -o cholesky_tri.exe cholesky_tri.f90
    ! ejecucion: .\cholesky_tri.exe < cholesky_tri.txt > cholesky_tri.out

    use mod_clreal
    use mod_formatos

    implicit none

    ! Declaración de variables
    integer :: n, i, j, j1

    ! ad = diagonal principal (aad su copia)
    ! as = subdiagonal inferior (aas su copia)
    ! La matriz B se convierte en ad, as
    real(kind = clreal), allocatable :: ad(:), aad(:), as(:), aas(:), b(:), x(:), y(:), r(:)
    real(kind = clreal) :: deter
    
    print *, 'Cálculo de la factorización de Cholesky - Caso tridiagonal'
    print *

    ! Leer orden de la matriz
    print *, 'Orden de A: '
    read *, n
    print *, n
    
    ! Asignación de memoria dinámica
    allocate(ad(n), aad(n), as(n-1), aas(n-1), b(n), x(n), y(n), r(n))
    
    ! Lectura de datos
    if(n < 2) then
        print *, 'La matriz ha de tener, al menos, orden 2'
        stop
    endif
    
    print *, 'Introduzca la diagonal principal ad:'
    read *, ad
    print floats, ad
    print *, 'Introduzca la subdiagonal as:'
    read *, as
    print floats, as

    print *, 'Introduzca el vector b:'
    read *, b
    print floats, b

    ! Copias de las diagonales para futuro calculo del residuo
    aad = ad
    aas = as

    if(ad(1) < 1.e-12) then
        print *
        print *,'Radicando nº1 es <=0 en la matriz B'
        print *,'la matriz del sistema no es definida positiva'
        stop
    endif


    ! Actualización del determiante
    deter = ad(1)

    ! Obtención del elemento diagonal
    ad(1) = sqrt(ad(1))

    do j=1, n-1 
        
        ! obtencion del elemento subdiagonal
        as(j) = as(j) / ad(j)

        j1 = j+1
        ad(j1) = ad(j1) - as(j)**2

        if(ad(j1) < 1.e-12) then
            print *
            print *, 'Radicando nº', j1, '<=0 en la matriz B'
            print *, 'la matriz del sistema no es definida positiva'
            stop
        end if

        ! Actualización del determinante
        deter = deter * ad(j1)
        
        ! Obtención del elemento diagonal
        ad(j1) = sqrt(ad(j1))

    end do

    print *
    print *, 'En la factorización A=BBt'
    print *, 'La diagonal principal de B es:'
    print floats, ad
    print *, 'La subdiagonal de B es:'
    print floats, as
    print *
    print *, 'El determinante de la matriz A es:'
    print *, deter

    ! Cálculo de la solucion del S.E.L. triangular inferior
    y(1) = b(1) / ad(1)

    do i=2, n
        y(i) = (b(i) - as(i-1) * y(i-1)) / ad(i)
    end do

    ! Cálculo de la solución del S.E.L. triangular superior
    x(n) = y(n) / ad(n)

    do i=n-1, 1, -1
        x(i) = (y(i) - as(i) * x(i+1)) / ad(i)
    end do

    ! Escritura de la solución
    print *
    print *, 'La solución es:'
    print floats, x

    ! Cálculo del residuo de la solución
    r(1) = aad(1) * x(1) + aas(1) * x(2) - b(1)

    do i=2, n-1
        r(i) = aas(i-1) * x(i-1) + aad(i) * x(i) + aas(i) * x(i+1) - b(i)
    end do

    r(n) = aas(n-1) * x(n-1) + aad(n) * x(n) - b(n)

    ! Escritura del residuo de la solución
    print *
    print *, 'El residuo del sistema, r = Au - b, es:'
    print floats, r

    deallocate(ad, aad, as, aas, b, x, y, r)

end program cholesky_tri