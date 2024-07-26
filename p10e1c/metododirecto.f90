subroutine metododirecto(nodos,imagenes,x)
    implicit none	!he quitado los puntos pues no nos los dan, pero se deberia hacer suponiendo un punto y una sol
    real::nodos(:),imagenes(:),x(size(nodos)),matriz(size(nodos),size(nodos)),suma(size(nodos))
    integer::n,i,j,k
    INTERFACE
        function gauss(a,b)
            implicit none
            real::a(:,:),b(:),gauss(size(b))
            integer::k,n,i,j,imax,m
            real::suma,aux1(size(b)),aux2
        end function
    END INTERFACE
    n=size(nodos)
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
end subroutine
