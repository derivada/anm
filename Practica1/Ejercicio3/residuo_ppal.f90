program residuo_ppal
    
    ! Por defecto: usar subrutina versión 3
    ! compilacion: gfortran -o residuo.exe residuo_ppal.f90 residuo_sub3.f90
    ! ejecucion: .\residuo.exe < residuo.txt > residuo.out

    ! Compilación con otras versiones de la subrutina: cambiar el número de residuo_sub.f90
    ! y descomentar el código de este programa principal para admitir r
    use mod_clreal 
    implicit none
    
    ! especificadores de formato para I/O
    character(len = 10) :: dos_enteros = '(2i4)' ! 2 enteros de hasta 4 cifras
    character(len = 10):: floats = '(100e12.4)' ! Hasta 100 floats en notación científica con 4 decimales

    real (kind=clreal), allocatable :: a(:,:), b(:), u(:) ! La matriz A (mxn), los vectores b y u y el residuo a calcular r
    !real, allocatable :: r(:) !Versiones 1 y 2 subrutina

    integer :: n, m ! Las dimensiones de A
    integer :: i

    ! Lectura de datos
    print *, "Las dimensiones nxm de la matriz A son: "
    read *, m, n
    print dos_enteros, m, n
    allocate(a(m,n), b(m), u(n))
    !allocate(r(m)) !Versiones 1 y 2 subrutina 

    print *, "Introduzca la matriz A:"
    do i = 1, m
        read *, a(i, :)
        print floats, a(i, :)
    end do

    print *, "Introduzca el vector b:"
    read *, b 
    print floats, b

    print *, "Introduzca el vector u:"
    read *, u
    print floats, u

    ! Llamada a la subrutina que calcula el residuo
    call residuo(m, n, a, b, u) ! Versión 3
    !call residuo(m, n, a, b, u, r) ! Versiones 1 y 2

    ! Impresión del resultado
    print *
    print *, 'El vector residuo del sistema lineal Au = b (r = Au-b) es:'
    print floats, b ! Versión 3 de la subrutina

    !print floats, r ! Versiones 1 y 2 de la subrutina

    deallocate(a, b, u)
    !deallocate(r) ! Versiones 1 y 2 de la subrutina

end program residuo_ppal
