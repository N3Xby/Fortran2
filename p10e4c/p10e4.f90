program p10e4
    implicit none
    real::
    integer::
    INTERFACE
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
