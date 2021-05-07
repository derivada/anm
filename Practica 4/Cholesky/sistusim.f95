subroutine sistusim(n,a,b,u)
    !Subrutina para el la resolucion del sitema triangular superior
    
    use mod_clreal
    
    implicit none
    
    !Declaración de varibles
    integer,intent(in)::n
    real(kind=clreal),intent(in)::a(n,n)
    real(kind=clreal),intent(inout)::b(n)
    !Vector solucion
    real(kind=clreal),intent(out)::u(n)
    
    real(kind=clreal)::aux
    integer::i, j
    
    u(n)=b(n)/a(n,n)
    
    do i=n-1,1,-1
        aux=0._clreal
        do j=i+1,n
            aux=aux+a(j,i)*u(j)
        end do
        u(i)=(b(i)-aux)/a(i,i)
    end do
    
end subroutine sistusim