subroutine datsissim(n,a,b)
    
    ! Subrutina de inicialización para el método de Cholesky
    
    ! Pide los datos de una matriz simetrica y definida positiva
    
    use mod_clreal    
    implicit none

    ! Declaración de variables
    integer,intent(in)::n ! Orden del S.E.L.
    real(kind=clreal), intent(out) :: a(n, n)
    real(kind=clreal), intent(out) :: b(n)
    integer :: i ! Variable local

    print*, 'La parte triangular inferior de la matriz simétrica A es: '
    do i=1, n
        read*, a(i, 1:i)
        print*, a(i, 1:i)
    end do
    
    print *, 'El término independiente b es'
    read*, b
    print*, b
   
end subroutine datsissim