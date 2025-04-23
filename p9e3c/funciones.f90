function f(x)
    implicit none
    real:: f(3),x
    f(1)=(x**2)-exp(x)
    f(2)=(2*x)-exp(x)
    f(3)=2-exp(x)
end function

function g(x)
    implicit none
    real:: g(3),x
    g(1)=x+log(x)
    g(2)=1+(1/x)
    g(3)=-1/(x**2)
end function