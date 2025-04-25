program p10e4
    implicit none
    real::a1,b1,a2,b2,pre1,ite1,pre2,ite2,resultado1(2),resultado2(2)
    integer::i,j,k
    INTERFACE
        real function fun1(x)
            implicit none
            real::x
        end function
        real function fun2(x)
            implicit none
            real::x
        end function
    END INTERFACE
    print*,"Dime el limite inferior y superior [a1,b1] para la funcion de trabajo 1"
    read(*,*)a1,b1
    print*,"Ahora dime la precisión deseada (epsilon) de la integración y el número máximo de iteraciones"
    read(*,*)pre1,ite1
    print*,"Dime el limite inferior y superior [a2,b2] para la funcion de trabajo 2"
    read(*,*)a2,b2
    print*,"Ahora dime la precisión deseada (epsilon) de la integración y el número máximo de iteraciones"
    read(*,*)pre2,ite2
    write(6,*)"====RESULTADOS FUNCION 1===="
    call trapecio(a1,b1,pre1,ite1,fun1,resultado1)
    !si es menor esta bien si no te has pasado de iteraciones resultado(2).le.ite1
    if (resultado1(2)<=ite1) then
        write(*,*)"El resultado de la integral es:",resultado1(1)
        write(*,*)"El número de iteraciones es:",resultado1(2)
    else
        write(*,*)"No se ha conseguido la precisión deseada"
    end if
    write(6,*)"====RESULTADOS FUNCION 2===="
    call trapecio(a2,b2,pre2,ite2,fun2,resultado2)
    if (resultado2(2)<=ite2) then
        write(*,*)"El resultado de la integral es:",resultado2(1)
        write(*,*)"El número de iteraciones es:",resultado2(2)
    else
        write(*,*)"No se ha conseguido la precisión deseada"
    end if
    end program