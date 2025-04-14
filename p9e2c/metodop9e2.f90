subroutine deriva(nodos,imagenes,puntos,h,prime,segun)
    implicit none
    real::nodos(:),imagenes(:),puntos(:),h,ymas(size(puntos)),ymenos(size(puntos)),prime(:),&
    &primemenos(size(puntos)),primemas(size(puntos)),segun(:),ymasmas(size(puntos)),ymasmenos(size(puntos)),&
    &ymenosmenos(size(puntos)),x(size(imagenes))
    integer::nn,np,i,j,k
    nn=size(imagenes)
    np=size(puntos)
    call metododirecto(nodos,imagenes,puntos,x)
    !calculamos las imagenes de los puntos Xo+h
    do i=1,size(puntos)
        do j=1,size(imagenes)
            ymas(i)=ymas(i)+x(j)*((puntos(i)+h)**(j-1))
        end do
    end do
    !calculamos las imagenes de los puntos Xo-h
    do i=1,size(puntos)
    	do j=1, size(imagenes)
    	    ymenos(i)=ymenos(i)+x(j)*((puntos(i)-h)**(j-1))
    	end do
    end do
    !Calculamos la aproximacion centrada de la derivada primera mediante la ecuacion
    do i=1,size(puntos)
        prime(i)=(ymas(i)-ymenos(i))/(2*h)
    end do
    !Repetimos el proceso para calcular la segunda derivada a partir de la primera
    !calculamos las imagenes de los puntos por arriba y por abajo de la primera derivada
    do i=1,size(puntos)
        do j=1,size(imagenes)
            ymasmas(i)=ymasmas(i)+x(j)*((puntos(i)+h+h)**(j-1))
        end do
    end do
    do i=1,size(puntos)
    	do j=1, size(imagenes)
    	    ymasmenos(i)=ymasmenos(i)+x(j)*((puntos(i)+h-h)**(j-1))
    	end do
    end do
    do i=1,size(puntos)
    	do j=1, size(imagenes)
    	    ymenosmenos(i)=ymenosmenos(i)+x(j)*((puntos(i)-h-h)**(j-1))
    	end do
    end do
    !calculamos la primera derivada por abajo y por arriba
    do i=1,size(puntos)
        primemenos(i)=(ymasmenos(i)-ymenosmenos(i))/(2*h)
    end do
    do i=1,size(puntos)
        primemas(i)=(ymasmas(i)-ymasmenos(i))/(2*h)
    end do
    !calculamos la segunda derivada
    do i=1,size(puntos)
        segun(i)=((primemas(i)-primemenos(i))*(2*h))/((2*h)**2)
    end do
end subroutine
