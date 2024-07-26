real function fun1(x)
    implicit none
    real::x
    fun1=x**2-log(x)
    return
end function

real function fun2(x)
    implicit none
    real::x
    fun2=x**-1
    return
end function
