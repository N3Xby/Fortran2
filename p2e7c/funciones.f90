function fun1(x1)
    implicit none
    real::x1,fun1(2)
    fun1(1)=x1**2
    fun1(2)=2*x1
    return
end function

function fun2(x2)
    implicit none
    real::x2,fun2(2)
    fun2(1)=x2**3
    fun2(2)=3*x2**2
    return
end function
