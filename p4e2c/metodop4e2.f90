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
    x0=a
    x1=b
    fx0=func(x0)
    fx1=func(x1)
    do i=1,nmax
        x2=x1-((fx1(1)*(x1-x0))/(fx1(1)-fx0(1)))
        fx2=func(x2)
        if (abs(fx2(1)).lt.facc) then
            raiz=x2
            nite=i
            return
        else
            x0=x1
            x1=x2
            fx0(1)=fx1(1)
            fx1(1)=fx2(1)
        end if
    end do
    raiz=x2
    nite=nmax+1
end subroutine
