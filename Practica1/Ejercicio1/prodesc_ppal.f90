program prodesc_ppal

    ! compilacion: gfortran -o prodesc.exe prodesc_ppal.f90
    ! ejecucion: .\prodesc.exe < prodesc.txt > prodesc.out

    implicit none
    real,allocatable :: a(:), b(:) ! los vectores a y b
    integer :: n, i ! el tamaño de los vectores
    real :: c, d ! el producto escalar a*n

    print*, "El tamaño de los vectores es: "
    read *, n
    allocate(a(n), b(n))

    ! lectura de los vectores
    print*, "El vector a es:"
    read*, a
    print*, "a = {", a, "}" 

    print*,"El vector b es:"
    read*, b
    print*, "b = {", b, "}" 

    ! dot-product usando la funcion de la libreria de FORTRAN
    c = dot_product(a, b)
    print'(A30, F10.5)', 'a * b (funcion dot_product) = ', c
    
    ! dot-product por definicion
    d = 0
    do i = 1, n
        d = d + a(i)*b(i)
    end do
    print '(A30, F10.5)', 'a * b (por definicion) = ', d


    deallocate(a,b)
end program

