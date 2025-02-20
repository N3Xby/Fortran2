program p3e3
    implicit none
    real::a1,b1,a2,b2,errx1,errf1,errx2,errf2,result1(2),result2(2)
    integer::ite1,ite2
    INTERFACE
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
    END INTERFACE
    print*,"Dime cuantas iteraciones quieres usar para la primera y la segunda función, respectivamente"
    read(*,*)ite1,ite2
    print*,"Dime los valores extremos del intervalo incial"
    read(*,*)a1,b1
    print*,"Dime los errores en x e y de la funcion 1"
    read(*,*)errx1,errf1
    print*,"Dime los errores en x e y de la funcion 2"
    read(*,*)errx2,errf2
    
    do while (fun1(a1)*fun1(b1).gt.0)
        write(6,*) "Dame otros valores"
        read(*,*)a1,b1
    end do
    
    do while (fun2(a1)*fun2(b1).gt.0)
        write(6,*) "Dame otros valores"
        read(*,*)a1,b1
    end do
    
    result1=falsaposicion(fun1,a1,b1,errx1,errf1,ite1)
    if(result1(2).lt.ite1) then
    print*,"Se ha conseguido la precisión pedida (Han sobrado iteraciones)"
    Else
    print*,"No se ha conseguido la precisión deseada (Las iteraciones no han sido suficientes)"
    end if
    print*,"El valor aproximado de la raiz de la 1º función sería",result1(1)
    result2=falsaposicion(fun2,a2,b2,errx2,errf2,ite2)
    if(result2(2).lt.ite2) then
    print*,"Se ha conseguido la precisión pedida (Han sobrado iteraciones)"
    Else
    print*,"No se ha conseguido la precisión deseada (Las iteraciones no han sido suficientes)"
    end if
    print*,"El valor aproximado de la raiz de la 2º función sería",result2(1)
end program
