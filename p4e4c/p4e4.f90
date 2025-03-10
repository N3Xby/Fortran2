program p4e4
    implicit none
    real,allocatable::a(:)
    real::erry1,raiznewton1
    integer::dima,nmax,nite1,i   !Declaración variables método 3 y 4
    INTERFACE
    
    subroutine newton(func,a,facc,nmax,nite,raiz)
        implicit none
        real::a,facc,raiz,x,v(2),x1 !facc es la precisión
        integer::nite,nmax,i
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
    
    END INTERFACE
    Print*,"Dígame la dimensión del vector de valores de arranque"
    Read(5,*)dima
    Allocate(a(dima))
    Print*,"Teniendo en cuenta la dimensión dada, dígame los valores de arranque"
    Read(5,*)(a(i),i=1,dima)
    print*,"Dime la precisión de cero en y deseada" 
    read(5,*)erry1
    Print*,"Dime el número de iteraciones que va a usar el método"
    Read(5,*)nite1
    print*,"Dime el número máximo de iteraciones que se permiten"
    read(5,*)nmax
    
    do i=1,dima
        call newton(fun1,a(i),erry1,nmax,nite1,raiznewton1)
        print*,"El valor de la raiz obtenida es",raiznewton1,"y el número de iteraciones usadas es",nite1
        if (nite1.le.nmax) then
            print*,"Se ha conseguido la precisión pedida"
        else
            print*,"NO se ha conseguido la precisión pedida"
        end if
    end do
end program
