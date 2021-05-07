subroutine residuosim(n,a,b,u,r)
    !Subrutina para el calculo del vector residuo
    
    use mod_clreal
    
    implicit none
    
    !Declaración de varibles
    integer,intent(in)::n
    real(kind=clreal),intent(in)::a(n,n),u(n)
    real(kind=clreal),intent(out)::r(n)
    
    !Variables para la primera version
    real(kind=clreal)::aux
    integer::i,j
    real(kind=clreal),intent(in)::b(n)
    
    !Primera version
    do i=1,n
        aux= 0._clreal
        do j=1,i
            aux=a(i,j)*u(j)+aux
        end do
        do j=i+1,n
            aux=a(j,i)*u(j)+aux
        end do
        r(i)=aux-b(i)
    end do
    
    
end subroutine residuosim