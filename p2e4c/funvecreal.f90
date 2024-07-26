real function funvecreal(y)
real::y
funvecreal=y**3
return
end function

real function funvecreal2(x)
real::x(5),cubo(5)
do i=1,5
    cubo(i)=x(i)**3
end do
funvecreal2=sum((/(cubo(i),i=1,5,2)/))
return
end function