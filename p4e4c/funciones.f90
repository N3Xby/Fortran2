function fun1(xm)
    implicit none
    real::xm,fun1(2)
    fun1(1)=xm**2-exp(xm)
    fun1(2)=2*xm-exp(xm)
    return
end function
