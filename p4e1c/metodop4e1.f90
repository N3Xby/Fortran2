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
    x=a
    v=func(x)
    do i=1,nmax
        x1=x-(v(1)/v(2))
        v=func(x1)
        if (abs(v(1)).lt.facc)then
            raiz=x1
            nite=i
            return
        else
            x=x1 !en la nueva iteracion la nueva x1 pasa a ser mi nueva x y asi sigue el metodo
        end if
    end do
    raiz=x1
    nite=nmax+1
end subroutine
        
