function fun1(x)
    implicit none
    real::x,fun1(3)
    fun1(1)=x**2-exp(x) !resultado analitico
    fun1(2)=2*x-exp(x)  !resultado analitico primera derivada
    fun1(3)=2-exp(x)    !resultado analitico segunda derivada
    return
end function

function fun2(x)
    implicit none
    real::x,fun2(3)
    fun2(1)=x+log(x)    !resultado analitico
    fun2(2)=1+(1/x) !resultado analitico primera derivada
    fun2(3)=-1/x**2 !resultado analitico segunda derivada
    return
end function
