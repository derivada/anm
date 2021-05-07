subroutine datsissim(n,a,b)
    
!    Subrutina de inicializacion para el metodo de cholesky
!    pide los datos de una matriz simetrica y definida positiva
    
    use mod_clreal
    
    implicit none

    integer::i
    
    !Declaración de varibles
    integer,intent(in)::n !orden del SEL
    real(kind=clreal),intent(out)::a(n,n)
    real(kind=clreal),intent(out)::b(n)
    
    print*, 'La parte triangular inferior de la matriz simétrica A es: '
    do i=1,n
        read*,a(i,1:i)
        print*, a(i,1:i)
    end do
    
    
    print *, 'El término independiente b es'
    read*, b
    print*, b
   

    
end subroutine datsissim