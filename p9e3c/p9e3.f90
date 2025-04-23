program p9e3
    implicit none
    real:: x(2),h(2),aux(3),richardson
    integer:: n(2)
    external:: richardson
    INTERFACE
        function f(x)
            real:: f(3),x
        end function

        function g(x)
            real:: g(3),x
        end function
    END INTERFACE
    write(*,*) "Introduzca para f1 el punto donde se desea derivar, el paso a aplicar y el númeor de iteraciones."
    read(*,*) x(1),h(1),n(1)

    write(*,*) "Introduzca para f2 el punto donde se desea derivar, el paso a aplicar y el númeor de iteraciones."
    read(*,*) x(2),h(2),n(2)

    aux=f(x(1))
    write(*,*) "Función trabajo 1"
    write(*,*) "Resultado:", richardson(f,x(1),h(1),n(1))
    write(*,*) "Error relativo(%):", (100*abs(richardson(f,x(1),h(1),n(1))-aux(2)))/abs(aux(2))

    aux=g(x(2))
    write(*,*) "Función trabajo 2"
    write(*,*) "Resultado:", richardson(g,x(2),h(2),n(2))
    write(*,*) "Error relativo(%):", (100*abs(richardson(g,x(2),h(2),n(2))-aux(2)))/abs(aux(2))

end program p9e3