subroutine residuosim(n,a,b,u,r,norm)
    !Subrutina para el calculo del vector residuo
    
    use mod_clreal
    
    implicit none
    
    !Declaración de varibles
    integer,intent(in)::n
    real(kind=clreal),intent(in)::a(n,n),u(n)
    real(kind=clreal),intent(out)::r(n), norm
    
    !Variables para la primera version
    real(kind=clreal)::aux, sumsqr
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

    norm = 0.
    sumsqr = 0.
    do j = 1, n
    ! norma 2
        sumsqr = sumsqr + r(j)**2
    end do
    norm = sqrt(sumsqr)
    
end subroutine residuosim