!La gran ventaja del module es que no es necesario declarar las funciones vectoriales con sus INTERFACE, con la sentencia "use" podemos acceder a estas directamente.
!Por lo demás es exactamente igual que el método 1 de este ejercicio.

program ej2
    use funciones

	implicit none
	real :: f,g,v(2)
	real :: a1,b1,a2,b2,a,b,p,e,x1,y1,x2,y2
	integer :: n,m
	external :: f,g

	write (*,*) "Introduzca los valores inferior y superior del intervalo inicial para f1"
	read (*,*) a1,b1
	
	do while (f(a1)*f(b1)>0)
		write (*,*) "En este intervalo no hay ninguna raiz, introduzca un intervalo valido"
		read (*,*) a1,b1
	end do
	
	write (*,*) "Introduzca los valores inferior y superior del intervalo inicial para f2"
	read (*,*) a2,b2
	
	do while (g(a2)*g(b2)>0)
		write (*,*) "En este intervalo no hay ninguna raiz, introduzca un intervalo valido"
		read (*,*) a2,b2
	end do

	write (*,*) "Introduzca la precision deseada"
	read (*,*) p
	
	write (*,*) "Introduzca el error de 0 deseado"
	read (*,*) e
	
	write (*,*) "Introduzca el numero maximo de iteraciones permitido"
	read (*,*) n


    write (*,*) " "
    write (*,*) "**METODO DE LA BISECCION**"
    write (*,*) " "

    x1=a1
    y1=b1
    x2=a2
    y2=b2
	m=n
	v=biseccion(a1,b1,p,e,n,f)
	if (v(2)>m) then
		write (*,*) "Se ha superado el nº maximo de iteraciones para f1. El resultado de la ultima iteracion es:",v(1)
		write (*,*) "Considere una o varias de estas opciones:"
		write (*,*) " a) Reducir el intervalo de acotacion."
		write (*,*) " b) Mejorar la precision."
		write (*,*) " c) Reducir el error de 0."
		write (*,*) " d) Aumentar el nº maximo de iteraciones."
	
	else
		write (*,*) "Raiz de f1:",v(1)
		write (*,*) "Nº de iteraciones:",v(2)
	end if
    write (*,*) " "
	
	v=biseccion(a2,b2,p,e,n,g)
	if (v(2)>m) then
		write (*,*) "Se ha superado el nº maximo de iteraciones para f2. El resultado de la ultima iteracion es:",v(1)
		write (*,*) "Considere una o varias de estas opciones:"
		write (*,*) " a) Reducir el intervalo de acotacion."
		write (*,*) " b) Mejorar la precision."
		write (*,*) " c) Reducir el error de 0."
		write (*,*) " d) Aumentar el nº maximo de iteraciones."
	
	else
		write (*,*) "Raiz de f1:",v(1)
		write (*,*) "Nº de iteraciones:",v(2)
	end if
    write (*,*) " "


    write (*,*) " "
    write (*,*) "**METODO DE LA FALSA POSICION**"
    write (*,*) " "

    a1=x1
    b1=y1
    a2=x2
    b2=y2
	n=m
	v=falsaposicion(a1,b1,p,e,m,f)
	if (v(2)>n) then
		write (*,*) "Se ha superado el nº maximo de iteraciones para f1. El resultado de la ultima iteracion es:",v(1)
		write (*,*) "Considere una o varias de estas opciones:"
		write (*,*) " a) Reducir el intervalo de acotacion."
		write (*,*) " b) Mejorar la precision."
		write (*,*) " c) Reducir el error de 0."
		write (*,*) " d) Aumentar el nº maximo de iteraciones."
	
	else
		write (*,*) "Raiz de f1:",v(1)
		write (*,*) "Nº de iteraciones:",v(2)
	end if
    write (*,*) " "
	
	v=falsaposicion(a2,b2,p,e,m,g)
	if (v(2)>n) then
		write (*,*) "Se ha superado el nº maximo de iteraciones para f2. El resultado de la ultima iteracion es:",v(1)
		write (*,*) "Considere una o varias de estas opciones:"
		write (*,*) " a) Reducir el intervalo de acotacion."
		write (*,*) " b) Mejorar la precision."
		write (*,*) " c) Reducir el error de 0."
		write (*,*) " d) Aumentar el nº maximo de iteraciones."
	
	else
		write (*,*) "Raiz de f1:",v(1)
		write (*,*) "Nº de iteraciones:",v(2)
	end if
    write (*,*) " "

end program