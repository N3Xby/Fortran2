function fun1(xm)
    implicit none
    real::xm,fun1(2)
    fun1(1)=xm**2-exp(xm)
    fun1(2)=2*xm-exp(xm)
    return
end function

function fun2(xm)
    implicit none
    real::xm,fun2(2)
    fun2(1)=2*xm**3-exp(xm)
    fun2(2)=6*xm**2-exp(xm)
    return
end function
