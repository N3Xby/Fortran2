real function fun1(x)
    implicit none
    real::x
    fun1=x**2-exp(x)
    return
end function

real function fun2(x)
    implicit none
    real::x
    fun2=2*x**3-exp(x)
    return
end function

function fun3(xm)
    implicit none
    real::xm,fun3(2)
    fun3(1)=xm**2-exp(xm)
    fun3(2)=2*xm-exp(xm)
    return
end function

function fun4(xm)
    implicit none
    real::xm,fun4(2)
    fun4(1)=2*xm**3-exp(xm)
    fun4(2)=6*xm**2-exp(xm)
    return
end function
