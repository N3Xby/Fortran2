function EulerHeun(a,b,c,h,fun)
    Implicit none
    Real::a,b,c,h,x(0:nint(abs((b-a)/h))),y(0:nint(abs((b-a)/h))),EulerHeun(3),explicito,centrado,heun
    Integer::i,m
    INTERFACE
        real function fun(x,y)
            Implicit none
            Real::x,y
            integer::i
        end function
    END INTERFACE
    m=nint(abs((b-a)/h)) !nint redondea al entero
    x(0)=a
    y(0)=c
    do i=0,m
        x(i+1)=x(i)+h
    end do
    do i=0,m
        y(i+1)=y(i)+h*fun(x(i),y(i))
    end do
    !Euler explícito
    y(0)=c
    y(1)=y(0)
    do i=0,m
        y(x(i)+h)=y(i)+h*fun(x(i),y(i)) !corregir
    end do
    EulerHeun(1)=y(x(i)+h)
    !Euler centrado
    centrado=y(0)
    do i=0,m
        centrado=centrado+2*h*fun(x(i+1),y(i+1))
    end do
    EulerHeun(2)=centrado
    !Heun
    heun=y(0)
    do i=0,m
        heun=heun+(h/2)*(fun(x(i),y(i))+fun(x(i+1),y(i+1)))
    end do
    EulerHeun(3)=heun
end function
