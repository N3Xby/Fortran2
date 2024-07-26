program p11e1
    implicit none
    Real::a1,b1,c1,h1,a2,b2,c2,h2,resultado1(3),resultado2(3),analitico1,analitico2
    Integer::i
    INTERFACE
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
        end function
        
        real function fun1(x,y)
            Implicit none
            Real::x,y
            integer::i
        end function
    
        real function fun2(x,y)
            Implicit none
            Real::x,y
            integer::i
        end function
    END INTERFACE
    Print*,"Dime la xi y la xf para la primera y para la segunda funcion"
    read(*,*)a1,b1,a2,b2
    Print*,"Dime la yi de ambas"
    read(*,*)c1,c2
    Print*,"Dime el paso h a aplicar de ambas"
    read(*,*)h1,h2
    resultado1=EulerHeun(a1,b1,c1,h1,fun1)
    analitico1=c1*exp(b1-(0.05*b1**2))
    Print*,"Para la primera funcion los valores de Euler explcito,Euler centrado y Runge-Kutta son"
    Print*,"Euler explicito:",resultado1(1),"Euler centrado:",resultado1(2),"Runge-Kutta:",resultado1(3)
    Print*,"Sus errores respectivos son:"
    Print*,(abs(resultado1(1)-analitico1)/analitico1)*100,(abs(resultado1(2)-analitico1)/analitico1)*100&
    &(abs(resultado1(3)-analitico1)/analitico1)*100
    resultado2=EulerHeun(a2,b2,c2,h2,fun2)
    analitico2=c2*exp(-b2/8000.)
    Print*,"Para la primera funcion los valores de Euler explcito,Euler centrado y Runge-Kutta son"
    Print*,"Euler explicito:",resultado2(1),"Euler centrado:",resultado2(2),"Runge-Kutta:",resultado2(3)
    Print*,"Sus errores respectivos son:"
    Print*,(abs(resultado2(1)-analitico2)/analitico2)*100,(abs(resultado2(2)-analitico2)/analitico2)*100&
    &(abs(resultado2(3)-analitico2)/analitico2)*100
end program
