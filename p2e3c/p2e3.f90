program p2e3
implicit none
!si necesitamos que sea alocatable real,allocatable::x(:),x3(:) quitariamos x(5) y x(3)
real::x(5),x3(5),resultado
integer::i
INTERFACE
function funvec(y)
real::y(5),funvec(5) !si lo hacemos dinamico cambiamos aqui tambien y(:) y funvec(size(y))
end function
END INTERFACE
!para hacerlo dinamico allocate(x(7),x3(7))
print*,"Digame 5 elementos del vector"
read(5,*)(x(i),i=1,5)
print*,(x(i),i=1,5)
x3=funvec(x)
resultado=sum((/(x3(i),i=1,5,2)/))
write(6,*)"El resultado de la suma es",resultado
end program
