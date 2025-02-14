subroutine metodo(xm,funm,sumam)
    implicit none
    real::xm,sumam
    INTERFACE
        function funm(xm)
            implicit none
            real::xm,funm(2)
        end function
    END INTERFACE
    sumam=sum(funm(xm))
    return
end subroutine
