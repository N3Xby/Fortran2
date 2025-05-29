real function fun1(x,y)
    Implicit none
    Real::x,y
    integer::i
    fun1=(1-0.1*x)*y
    return
end function

real function fun2(x,y)
    Implicit none
    Real::x,y
    integer::i
    fun2=(-1./8000.)*y
    return
end function
