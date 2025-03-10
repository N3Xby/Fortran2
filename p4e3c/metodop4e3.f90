function bisec(func,a,b,errx,errf,ite)
    implicit none
    real::func,a,b,errx,errf,c
    real::fa,fb,fc,bisec(2) !imagenes de mis funciones de trabajo
    integer::ite,j
    INTERFACE
        function func(x)
        implicit none
        real::x
        end function
    END interface
    fa=func(a)
    fb=func(b)
    do j=1,ite
        c=(a+b)*0.5
        fc=func(c)
        if (abs(fc).lt.errf.and.abs(b-a).lt.errx) then
            bisec(1)=c
            bisec(2)=j
            return
        end if
        if (fa*fc.lt.0) then
            b=c
            fb=fc
        Else
            a=c
            fa=fc
        end if
    end do
    bisec(1)=c
    bisec(2)=ite+1
    return
end function

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
    fa=func(a)
    fb=func(b)
    do j=1,ite
        c=a-((fa*(b-a))/(fb-fa))    !respecto al 2 cambiamos esta linea solamente
        fc=func(c)
        if (abs(fc).lt.errf.and.abs(b-a).lt.errx) then
            falsaposicion(1)=c
            falsaposicion(2)=j
            return
        end if
        if (fa*fc.lt.0) then
            b=c
            fb=fc
        Else
            a=c
            fa=fc
        end if
    end do
    falsaposicion(1)=c
    falsaposicion(2)=ite+1
    return
end function

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
