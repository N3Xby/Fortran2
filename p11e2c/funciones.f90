!Programamos las funciones f(x,y) de cada EDO en un módulo para usarlas en ambos ejercicios.
module Funciones
implicit none
contains

    real function f(x,y)
        implicit none
        real :: x,y

        f=(1-0.1*x)*y

    end function


    real function g(x,y)
        implicit none
        real :: x,y

        g=-(y/8000.)

    end function

end module