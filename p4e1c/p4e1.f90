program p4e1
    implicit none
    real::a1,a2,erry1,erry2,raiz1,raiz2
    integer::nmax,nite1,nite2
    INTERFACE
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
        
        function fun1(xm)
            implicit none
            real::xm,fun1(2)
        end function

        function fun2(xm)
            implicit none
            real::xm,fun2(2)
        end function
        
    END INTERFACE
    print*,"Dime el punto de aranque próximo a la raiz para cada función"
    read(5,*)a1,a2
    print*,"Dime la precisión de cero en y deseada para cada función" 
    read(5,*)erry1,erry2
    print*,"Dime el número máximo de iteraciones que se permiten para cada función"
    read(5,*)nmax
    print*,"Dime las iteraciones que va a usar el método para cada función"
    read(5,*)nite1,nite2
    
    call newton(fun1,a1,erry1,nmax,nite1,raiz1)
    print*,"El valor de la raiz obtenida es",raiz1,"y el número de iteraciones usadas es",nite1
    if (nite1.le.nmax) then
        print*,"Se ha conseguido la precisión pedida"
    else
        print*,"NO se ha conseguido la precisión pedida"
    end if
    call newton(fun2,a2,erry2,nmax,nite2,raiz2)
    print*,"El valor de la raiz obtenida es",raiz2,"y el número de iteraciones usadas es",nite2
    if (nite2.le.nmax) then
        print*,"Se ha conseguido la precisión pedida"
    else
        print*,"NO se ha conseguido la precisión pedida"
    end if
end program
