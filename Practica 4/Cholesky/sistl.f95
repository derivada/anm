subroutine sistl(n,a,b,u)
    !Segunda implementación del cálculo del vector residuo
    
    use mod_clreal
    
    implicit none
    
    !Declaración de varibles
    integer,intent(in)::n
    real(kind=clreal),intent(in)::a(n,n)
    real(kind=clreal),intent(inout)::b(n)
    real(kind=clreal),intent(out)::u(n)
    real(kind=clreal)::aux
    integer::i,j
    
    u(1)=b(1)/a(1,1)
    
    do i=1,n
!        aux=0.
!        do j=1, i-1
!            aux=aux+a(i,j)*u(j)
!        end do
!        u(i)=(b(i)-aux)/a(i,i)
        u(i)=b(i)/a(i,i)
        b(i+1:n)=b(i+1:n)-a(i+1:n,i)*u(i)
    end do
    
end subroutine sistl