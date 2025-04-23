subroutine deriva(nodos,imagenes,puntos,h,prime,segun)
    implicit none
    real::nodos(:),imagenes(:),puntos(:),prime(size(puntos)),segun(size(puntos))
    integer::nn,np,i,j,k
    real::x(size(nodos)),aux1,aux2,aux3,h
    INTERFACE
        subroutine metododirecto(nodos,imagenes,puntos,x)
            implicit none
            real::nodos(:),imagenes(:),puntos(:),x(size(puntos)),matriz(size(imagenes),size(imagenes)),suma(size(imagenes))
            integer::n,m,i,j,k
        end subroutine

        function gauss(a,b)
            implicit none
            real::a(:,:),b(:),gauss(size(b))
            integer::k,n,i,j,imax,m
            real::suma,aux1(size(b)),aux2
        end function
    END INTERFACE
    nn=size(nodos)
    np=size(puntos)

    call metododirecto(nodos,imagenes,puntos,x)

    do i=1,np
        aux1=sum((/(x(j)*((puntos(i)+h)**j),j=0,nn)/))
        aux2=sum((/(x(j)*((puntos(i)-h)**j),j=0,nn)/))
        aux3=sum((/(x(j)*(puntos(i)**j),j=0,nn)/))

        prime(i)=(aux1-aux2)/(2*h)
        segun(i)=(aux1+aux2-2*aux3)/(h**2)
    end do
end subroutine
