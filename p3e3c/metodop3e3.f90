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
        if (fc.le.errf.and.abs(b-a).le.errx) then
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
