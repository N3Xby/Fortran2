program p4e3
    implicit none
    real::a1,b1,a2,b2,errx1,errf1,errx2,errf2,resultbisec1(2),resultbisec2(2),resultfals1(2),resultfals2(2)
    integer::ite1,ite2      !Declaración variables método 1 y 2
    real::c1,d1,c2,d2,erry1,erry2,raiznewton1,raiznewton2,raizsecante1,raizsecante2
    integer::nmax,nite1,nite2   !Declaración variables método 3 y 4
    INTERFACE 
        function bisec(func,a,b,errx,errf,ite)
            implicit none
            real::func,a,b,errx,errf,c
            real::fa,fb,fc,bisec(2) !imagenes de mis funciones de trabajo
            integer::ite,j
            INTERFACE
                function func(x)
                implicit none
                real::x
                end function
            END interface
        end function
        
        function falsaposicion(func,a,b,errx,errf,ite)
            implicit none
            real::func,a,b,errx,errf,c
            real::fa,fb,fc,falsaposicion(2) !imagenes de mis funciones de trabajo
            integer::ite,j
            INTERFACE
                function func(x)
                implicit none
                real::x
                end function
            END interface
        end function
        
        real function fun1(x)
            implicit none
            real::x
        end function

        real function fun2(x)
            implicit none
            real::x
        end function
    
        subroutine newton(func,a,facc,nmax,nite,raiz)
		    implicit none
		    real::a,facc,raiz,x,v(2),x1 !facc es la precisión
		    integer::nite,nmax
		    INTERFACE
		        function func(xm)
		        implicit none
		        real::xm,func(2)
		        end function
		    END INTERFACE
        end subroutine
        
        subroutine secante(func,a,b,facc,nmax,nite,raiz)
            implicit none
            real::a,b,x0,x1,x2,facc,raiz,fx0(2),fx1(2),fx2(2)
            integer::nmax,nite,i
            INTERFACE
                function func(xm)
                implicit none
                real::xm,func(2)
                end function
            END INTERFACE
        end subroutine
        
        function fun3(xm)
            implicit none
            real::xm,fun3(2)
        end function

        function fun4(xm)
            implicit none
            real::xm,fun4(2)
        end function
    END INTERFACE
    
    !Lectura para el método 1 y 2
    print*,"(Método 1 y 2) Dime cuantas iteraciones quieres usar para la primera y la segunda función, respectivamente"
    read(*,*)ite1,ite2
    print*,"(Método 1 y 2) Dime los valores extremos del intervalo incial"
    read(*,*)a1,b1
    print*,"(Método 1 y 2) Dime los errores en x e y de la funcion 1"
    read(*,*)errx1,errf1
    print*,"(Método 1 y 2) Dime los errores en x e y de la funcion 2"
    read(*,*)errx2,errf2
    
    !Lectura para el método 3 y 4
    print*,"(Método 3 y 4) Dime los puntos de arranque para cada función de forma (a1,b1,a2,b2)"
    read(5,*)c1,d1,c2,d2
    print*,"(Método 3 y 4) Dime la precisión de cero en y deseada para cada función" 
    read(5,*)erry1,erry2
    print*,"(Método 3 y 4) Dime el número máximo de iteraciones que se permiten para cada función"
    read(5,*)nmax
    print*,"(Método 3 y 4) Dime las iteraciones que va a usar el método para cada función"
    read(5,*)nite1,nite2
    
    !Método 1
    do while (fun1(a1)*fun1(b1).gt.0)
        write(6,*) "Dame otros valores"
        read(*,*)a1,b1
    end do
    
    do while (fun2(a1)*fun2(b1).gt.0)
        write(6,*) "Dame otros valores"
        read(*,*)a1,b1
    end do
    
    resultbisec1=bisec(fun1,a1,b1,errx1,errf1,ite1)
    if(resultbisec1(2).lt.ite1) then
    print*,"(Método 1) Se ha conseguido la precisión pedida (Han sobrado iteraciones)"
    Else
    print*,"(Método 1) No se ha conseguido la precisión deseada (Las iteraciones no han sido suficientes)"
    end if
    print*,"(Método 1) El valor aproximado de la raiz de la 1º función sería",resultbisec1(1)
    resultbisec2=bisec(fun2,a2,b2,errx2,errf2,ite2)
    if(resultbisec2(2).lt.ite2) then
    print*,"(Método 1) Se ha conseguido la precisión pedida (Han sobrado iteraciones)"
    Else
    print*,"(Método 1) No se ha conseguido la precisión deseada (Las iteraciones no han sido suficientes)"
    end if
    print*,"(Método 1) El valor aproximado de la raiz de la 2º función sería",resultbisec2(1)
    
    !Método 2
    resultfals1=falsaposicion(fun1,a1,b1,errx1,errf1,ite1)
    if(resultfals1(2).lt.ite1) then
    print*,"(Método 2) Se ha conseguido la precisión pedida (Han sobrado iteraciones)"
    Else
    print*,"(Método 2) No se ha conseguido la precisión deseada (Las iteraciones no han sido suficientes)"
    end if
    print*,"(Método 2) El valor aproximado de la raiz de la 1º función sería",resultfals1(1)
    resultfals2=falsaposicion(fun2,a2,b2,errx2,errf2,ite2)
    if(resultfals2(2).lt.ite2) then
    print*,"(Método 2) Se ha conseguido la precisión pedida (Han sobrado iteraciones)"
    Else
    print*,"(Método 2) No se ha conseguido la precisión deseada (Las iteraciones no han sido suficientes)"
    end if
    print*,"(Método 2) El valor aproximado de la raiz de la 2º función sería",resultfals2(1)
    
    !Método 3
    call newton(fun3,c1,erry1,nmax,nite1,raiznewton1)
    print*,"(Método 3) El valor de la raiz obtenida es",raiznewton1,"y el número de iteraciones usadas es",nite1
    if (nite1.le.nmax) then
        print*,"(Método 3) Se ha conseguido la precisión pedida"
    else
        print*,"(Método 3) NO se ha conseguido la precisión pedida"
    end if
    call newton(fun4,c2,erry2,nmax,nite2,raiznewton2)
    print*,"(Método 3) El valor de la raiz obtenida es",raiznewton2,"y el número de iteraciones usadas es",nite2
    if (nite2.le.nmax) then
        print*,"(Método 3) Se ha conseguido la precisión pedida"
    else
        print*,"(Método 3) NO se ha conseguido la precisión pedida"
    end if
    
    !Método 4
    call secante(fun3,c1,d1,erry1,nmax,nite1,raizsecante1)
    print*,"(Método 4) El valor de la raiz obtenida es",raizsecante1,"y el número de iteraciones usadas es",nite1
    if (nite1.le.nmax) then
        print*,"(Método 4) Se ha conseguido la precisión pedida"
    else
        print*,"(Método 4) NO se ha conseguido la precisión pedida"
    end if
    call secante(fun4,c2,d2,erry2,nmax,nite2,raizsecante2)
    print*,"(Método 4) El valor de la raiz obtenida es",raizsecante2,"y el número de iteraciones usadas es",nite2
    if (nite2.le.nmax) then
        print*,"(Método 4) Se ha conseguido la precisión pedida"
    else
        print*,"(Método 4) NO se ha conseguido la precisión pedida"
    end if
end program
