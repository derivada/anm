program matriz_ppal

    ! compilacion: gfortran -o matriz.exe matriz_ppal.f90
    ! ejecucion: .\matriz.exe < matriz.txt > matriz.out

    implicit none
    
    real, allocatable :: a(:, :) ! la matriz a
    integer :: n, m, i, reserva ! dimensiones de la matriz

    ! especificadores de formato para I/O
    character(len = 4)::formato1='(i4)' ! entero de 4 digitos
    character(len = 10)::formato2='(100e12.4)' ! hasta un máximo de 100 números reales con 12 espacios (cifras o espacio blanco) de las cuales 4 son decimales

    print*, "El numero de filas es: "
    read formato1, n
    print formato1, n

    print*, "El numero de columnas es: "
    read formato1, m
    print formato1, m

    allocate(a(n,m), stat = reserva) ! reserva nos dirá si se ha podido reservar suficiente memoria para a

    if(reserva /= 0) then
        print *, "No se pudo reservar memoria para la matriz a"
        stop
    end if

    print*, "Introduzca la matriz a:"
    do i = 1, n
        read *, a(i, :)
        print formato2, a(i, :)
    end do

    ! Funciones para imprimir los tamaños de cada dimensión
    ! print*, "Tamaños de la matriz a:"
    ! print*, "1º dim: ", SIZE(a, 1)
    ! print*, "2º dim: ", SIZE(a, 2)
    ! print*, "Forma: ", SHAPE(a) ! todas las dim

    ! print*, a
    ! sin ninguna indicacion escribe la matriz por columnas
    ! esto es por la representacion en memoria:   
    ! MEMORIA: a11 a21 a31 a12 a22 a32 ... 
    ! por lo tanto siempre es mejor hacer operaciones por columnas para una mayor eficiencia

    deallocate(a)

end program
