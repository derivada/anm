program choleskymod2019

    ! Compilacion: gfortran -o choleskymod2019.exe choleskymod2019.f90
    ! Ejecucion: .\choleskymod2019.exe < choleskymod2019.txt > choleskymod2019.out
    use mod_formatos
    use mod_clreal
    implicit none

    integer :: n ! Orden del sistema
    real (kind = clreal) :: det ! Determinante del sistema
    real (kind = clreal), allocatable :: a(:), aa(:), s(:), ss(:), b(:), u(:), r(:) ! Matrices y vectores
    real (kind = clreal), allocatable :: bp(:), bs(:), v(:) ! Diagonal principal bp y diagonal secundaria bs de B y resultado intermedio v

    integer :: i ! Variables auxiliares
    real (kind = clreal) :: aux

    print *, 'Introduzca el orden del sistema: '
    read *, n
    print*, n

    ! Alojamiento de memoria
    allocate(a(n), aa(n), ss(n), s(n), b(n), u(n),  r(n), bp(n), bs(n), v(n))

    print*, 'Introduzca la diagonal principal del sistema:'
    read *, a
    print floats, a

    print *, 'Introduzca la diagonal sub-sub-principal:'
    read *, s
    print floats, s

    print *, 'Introduzca el vector B del sistema: '
    read *, b
    print floats, b
    
    ! Duplicado de a y s para el cálculo posterior del residuo
    aa = a
    ss = s

    ! Método de Cholesky modificado
    det = a(1) * a(2)
    bp(1) = sqrt(a(1))
    bp(2) = sqrt(a(2))

    do i = 3, n
        if(a(i) < 1.e-12) then
            print *, 'La matriz del sistema no es definida positiva, imposible continuar!'
            stop
        end if
        bs(i) = s(i-2) / bp(i-2)
        aux = a(i) - bs(i)**2
        det = det * aux
        bp(i) = sqrt(aux)
    end do
    
    print *, 'Diagonal principal B'
    print floats, bp
    print *, 'Diagonal secundaria B'
    print floats, bs

    ! T.I. Bv = b
    v(1) = b(1) / bp(1)
    v(2) = b(2) / bp(2)
    do i = 3, n
        v(i) = (b(i) - v(i-2) * bs(i)) / (bp(i))
    end do

    ! T.S. B*
    u(n) = v(n) / bp(n)
    u(n-1) = v(n-1) / bp(n-1)
    do i = n-2, 1, -1
        u(i) = (v(i) - bs(i+2) * u(i+2))/(bp(i))
    end do

    print *, 'Solucion: '
    print floats, u

    deallocate(a, aa, s, ss, b, u, r, bp, bs, v)
end program