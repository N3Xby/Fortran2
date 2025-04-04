subroutine metododirecto(nodos,imagenes,puntos,y)
    implicit none
    real::nodos(:),imagenes(:),puntos(:),y(size(puntos)),x(size(puntos)),matriz(size(imagenes),&
    &size(imagenes)),suma(size(imagenes))
    integer::n,m,i,j,k
    INTERFACE
        function gauss(a,b)
            implicit none
            real::a(:,:),b(:),gauss(size(b))
            integer::k,n,i,j,imax,m
            real::suma,aux1(size(b)),aux2
        end function
    END INTERFACE
    n=size(imagenes)
    m=size(puntos)
    do i=1,n !en vandermonde el grado es uno menos
        do j=1,n
        matriz(i,j)=nodos(i)**(j-1)
        end do
    end do
    print*,"La matriz de Vandermonde es:"
    do i=1,n
        write(6,*)(matriz(i,j),j=1,n)
    end do
    x=gauss(matriz,imagenes)
    print*, "Los valores de los coeficientes son:"
    do i=1,n
        print*,x(i)
    end do
    do i=1,size(puntos) !ponemos size pues n y m se redefinen en gauss, para no liarla
        do j=1,size(imagenes)
            y(i)=y(i)+x(j)*(puntos(i)**(j-1))
        end do
    end do
end subroutine
