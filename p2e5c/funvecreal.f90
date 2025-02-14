real function funvecreal(x)
    real::x
    funvecreal=x**3
    return
end function

real function funvecreal2(x)
    real::x(:),cubo(size(x))
    do i=1,size(x)
        cubo(i)=x(i)**3
    end do
    funvecreal2=sum((/(cubo(i),i=1,size(x),2)/))
    return
end function
