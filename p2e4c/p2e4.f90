program p2e4
	implicit none
	real::x(5),x3(5),resultado
	integer::i
	INTERFACE
	    real function funvecreal(y)
		real::y
	    end function
	    real function funvecreal2(x)
		real::x(5),cubo(5)
	    end function
	END INTERFACE
	print*,"Digame 5 elementos del vector"
	read(5,*)(x(i),i=1,5)
	print*,(x(i),i=1,5)
	do i=1,5
	    x3(i)=funvecreal(x(i))
	end do
	resultado=sum((/(x3(i),i=1,5,2)/))
	write(6,*)"El resultado de la suma es",resultado
	write(6,*)"El resultado de la suma por el segundo metodo es",funvecreal2(x)
end program
