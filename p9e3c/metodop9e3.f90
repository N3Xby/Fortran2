real function richardson(f,x,h,n)
    implicit none
    real :: x,h,d(0:n,0:n)
    real, dimension(3) :: aux1,aux2
    integer :: i,k,n
    INTERFACE
        function f(x)
            implicit none
            real :: f(3),x
        end function
    END INTERFACE

    !Calculamos la primera columna con la aproximación centrada.
    do k=0,n
        aux1=f(x+(h/(2**k)))
        aux2=f(x-(h/(2**k)))
        d(k,0)=(aux1(1)-aux2(1))/(2*(h/(2**k)))
    end do

    !Calculamos el resto de columnas en un proceso iterativo en el que se usa la expresión general.
    do i=1,n
        d(0:i-1,i)=0
        d(i:n,i)=(/((1./((2**(2*i))-1))*((2**(2*i))*d(k,i-1)-d(k-1,i-1)),k=i,n)/)
    end do

    richardson=d(n,n)

end function