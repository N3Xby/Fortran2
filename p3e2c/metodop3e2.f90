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
