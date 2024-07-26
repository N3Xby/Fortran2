program p9e1
    implicit none
    real::x1,x2,h1,h2,der1(4),der2(4),error1(4),error2(4),v1(3),v2(3)
    integer::i
    INTERFACE
    
    function derivadas(func,x,h)
        implicit none
        real::x,h,derivadas(4),vi(3),vf(3),v(3)
        INTERFACE
        function func(x)
            implicit none
            real::x,func(3)
        end function
        END INTERFACE
    end function
    
    function fun1(x)
        implicit none
        real::x,fun1(3)
    end function
    
    function fun2(x)
        implicit none
        real::x,fun2(3)
    end function
    
    END INTERFACE
    print*, "Dime el punto x en donde quieres obtener las derivadas y su paso h a aplicar de la primera funcion"
    read(*,*)x1,h1
    print*, "Ahora dime el punto x y el paso h de la segunda funcion"
    read(*,*)x2,h2
    
    der1=derivadas(fun1,x1,h1)
    print*,"Los valores para fun1 de las primeras derivadas (atrás, adelante y centrada) y la segunda derivada son"
    do i=1,4
        print*,der1(i)
    end do
    v1=fun1(x1)
    print*, "Ahora veremos los errores relativos cometidos"
    error1(1)=(abs(der1(1)-v1(2))/abs(v1(2)))*100
    print*, "El error relativo cometido con la primera derivada adelante es",error1(1),"%"
    error1(2)=(abs(der1(2)-v1(2))/abs(v1(2)))*100
    print*, "El error relativo cometido con la primera derivada atras es",error1(2),"%"
    error1(3)=(abs(der1(3)-v1(2))/abs(v1(2)))*100
    print*, "El error relativo cometido con la primera derivada centrada es",error1(3),"%"
    error1(4)=(abs(der1(4)-v1(3))/abs(v1(3)))*100
    print*, "El error relativo cometido con la segunda derivada es",error1(4),"%"
    
    der2=derivadas(fun2,x2,h2)
    print*,"Los valores para fun2 de las primeras derivadas (atrás, adelante y centrada) y la segunda derivada son"
    do i=1,4
        print*,der2(i)
    end do
    v2=fun2(x2)
    print*, "Ahora veremos los errores relativos cometidos"
    error2(1)=(abs(der2(1)-v2(2))/abs(v2(2)))*100
    print*, "El error relativo cometido con la primera derivada adelante es",error2(1),"%"
    error2(2)=(abs(der2(2)-v2(2))/abs(v2(2)))*100
    print*, "El error relativo cometido con la primera derivada atras es",error2(2),"%"
    error2(3)=(abs(der2(3)-v2(2))/abs(v2(2)))*100
    print*, "El error relativo cometido con la primera derivada centrada es",error2(3),"%"
    error2(4)=(abs(der2(4)-v2(3))/abs(v2(3)))*100
    print*, "El error relativo cometido con la segunda derivada es",error2(4),"%"
end program
