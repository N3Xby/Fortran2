function derivadas(func,x,h)
    implicit none
    real::x,h,derivadas(4),vi(3),vf(3),v(3)
    INTERFACE
    function func(x)
        implicit none
        real::x,func(3)
    end function
    END INTERFACE
    vi=func(x+h)
    vf=func(x-h)
    v=func(x)
    derivadas(1)=(vi(1)-v(1))/h  !paso adelante
    derivadas(2)=(v(1)-vf(1))/h !paso atras
    derivadas(3)=(vi(1)-vf(1))/(2*h)  !paso centrado
    derivadas(4)=(vi(1)+vf(1)-2*v(1))/h**2   !derivada segunda mediante el metodo
end function
