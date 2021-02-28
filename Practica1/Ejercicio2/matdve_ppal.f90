program matdve_ppal

    ! compilacion: gfortran -o matdve.exe matdve_ppal.f90
    ! ejecucion: .\matdve.exe < matdve.txt > matdve.out

    implicit none

    ! especificadores de formato para I/O
    character(len = 10):: entero = '(i4)' ! Un entero de hasta 4 cifras
    character(len = 10):: floats = '(100e12.4)' ! Hasta 100 floats en notación científica con 4 decimales

    real, allocatable :: al(:), ad(:), au(:), v(:), w(:) ! las diagonales al, ad, au, el vector de entrada v y el vector resultado w
    integer :: n
    print*, "La longitud de la diagonal principal y del vector es: "
    read entero, n
    print entero, n
    
    allocate(al(n-1), ad(n), au(n-1), v(n), w(n))

    print*, "Introduzca la diagonal al:"

    read *, al ! por alguna razón el formato "floats" no funciona al leer (Fortran runtime error: Bad value during floating point read)
    print floats, al

    print*, "Introduzca la diagonal ad:"
    read *, ad
    print floats, ad

    print*, "Introduzca la diagonal au:"
    read *, au
    print floats, au

    print *, "Introduzca el vector v: "
    read *, v
    print floats, v

    ! producto matriz tridiagonal por vector 
    w(1) = ad(1)*v(1) + au(1)*v(2)
    w(n) = al(n-1)*v(n-1) + ad(n)*v(n)
    
    ! Calcular los valores del w(2)...w(n-1) mediante un bucle clásico:
    !do i = 2, n-1
    !    w(i) = al(i-1) * v(i-1) + ad(i) * v(i) + au(i) * v(i+1)
    !end do
    
    ! Y otra forma más compacta de F90 (bucles implícitos)
    w(2:n-1) = al(1:n-2) * v(1:n-2) + ad(2:n-1) * v(2:n-1) + au(2:n-1) * v(3:n)

    print *
    print *, 'El vector producto w = Av es:'
    print floats, w

    deallocate(al, ad, au, v, w)

end program matdve_ppal
