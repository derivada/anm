program jacobi_ex
    
    use mod_formatos
    use mod_clreal 
    implicit none
    
    integer :: n, nitmax ! Orden de la matriz, número de iteracciones máximas

    ! Vectores de la matriz tridiagonal, TI, result y anterior iterante
    real(kind = clreal), allocatable:: a(:), c(:), d(:), b(:), u(:), uold(:)

    real(kind = clreal) :: eps ! Test de parada, error de la iteración
    
    ! Formatos tabla
    CHARACTER(LEN=64), PARAMETER :: LINEA1 = "(I4, '|', 100e12.4)"
    CHARACTER(LEN=64), PARAMETER :: LINEA2 = "('-------------------------------------')"

    ! Variables locales
    integer :: i, iter
    real (kind = clreal) :: error 
    
    print *, 'El tamaño de la matriz es: '
    read *, n
    print *, n
    
    ! Alojamiento de memoria
    allocate(a(n), c(n-1), d(n-1), b(n), u(n), uold(n))

    ! Lectura de datos

    print*, 'La diagonal a es: '
    read*, a
    print floats, a
    print*

    print*
    print*, 'La diagonal c es: '
    read*, c
    print floats, c
    print*


    print*, 'La diagonal d es: '
    read*, d
    print floats, d
    print*

    print*, 'El término independiente b es: '
    read*, b
    print floats, b

    print*
    print*, 'El iterante inicial u es: '
    read*, u
    print floats, u

    print*
    print*, 'El parámetro eps para el test de parada es:'
    read*, eps
    print*, eps

    print*
    print*, 'El número máximo de iteracciones permitidas es: '
    read*, nitmax
    print entero, nitmax
    
    ! Comprobación de diagonal no nulo
    do i = 1, n
        if(abs(a(i)) < 1.e-12) then
            print *, 'Elemento en la diagonal nulo en la posicion ', i
            stop
        end if
    end do

    ! Cálculo de los iterantes y test de para eps en norma 2
    do iter = 1, nitmax
        uold = u
        u(1) = (b(1) - d(1) * uold(2)) / a(1)

        do i = 2, n-1
            u(i) = (b(i) - uold(i-1) * c(i-1) - uold(i+1) * d(i) ) / a(i) 
        end do

        u(n) = (b(n) - c(n-1) * uold(n-1)) / a(n)

        ! Test de error absoluto en norma infinito
        ! error = maxval(abs(u-uold))

        ! Test de error absoluto en norma 2
        error = 0.
        do i = 1, n
            error = error + (u(i) - uold(i))**2
        end do
        error = sqrt(error)

        print LINEA1, iter, u
        print *,  'Diferencia en norma 2: ', error

        print LINEA2

        if(error < eps)then
            print*
            print*, 'Test de parada activado en la iteración: ', iter
            return
        endif

    end do
    print*
    print*, 'Efectuadas ', nitmax, ' iteracciones sin que se cumpla el test de parada'

    deallocate(a, c, d, b, u, uold)
end program jacobi_ex