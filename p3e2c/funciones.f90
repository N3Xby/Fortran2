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
