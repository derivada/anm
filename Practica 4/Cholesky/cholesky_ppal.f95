program cholesky_ppal

    ! Programa principal relativo a la factorización de Cholesky
    use mod_clreal
    use mod_formatos
    implicit none

    ! Declaración de variables
    integer :: n, i, j
    real(kind=clreal), allocatable :: a(:,:), b(:), aa(:,:), bb(:), x(:), y(:), r(:)
    real(kind=clreal) :: deter, norm ! Determinante de A y norma del residuo
    
    print*, 'Cálculo de la factorización de Cholesky'
    
    ! Leer orden de la matriz
    print*, 'Orden de A: '
    read*, n
    print*, n
    
    ! Asignación de memoria dinámica
    allocate(a(n, n), b(n), x(n), bb(n), aa(n, n), r(n), y(n))
    
    ! Lectura de datos
    call datsissim(n, a, b)
    
    ! Copias de a y b para calcular el residuo, una vez hallada la solución,
    ! se guarda la copia de a por columnas
    do j=1, n
        aa(j:n, j) = a(j:n, j)
    end do
    bb = b
    
    call cholesky(n, a, deter)
    
    print*, 'La matriz B es: '
    do i=1,n
        print*, a(i,1:i)
    end do
    
    ! Imprimimos el determinante
    print*, 'El determinante de B es: '
    print*, deter
    
    ! Resolución del sistema triangular inferior
    call sistl(n, a, b, y)
    
    print*
    print*, 'La solución del sistema triangular inferior es:'
    print*, y
    
    !Resolución del sistema triangular superior
    call sistusim(n, a, y, x)
    
    print*
    print*, 'La solución es:'
    print*, x
    
    !Calculo y escritura del residuo
    call residuosim(n, aa, bb, x, r, norm)
    
    print*
    print*, 'El residuo del sistema es:'
    print*, r

    print *
    print*, 'La norma 2 del residuo es:'
    print*, norm

    deallocate(a, aa, b, bb, x, y, r)

end program cholesky_ppal