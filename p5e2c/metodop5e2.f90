subroutine gauss(a,b,x)
    implicit none
    real::a(:,:),b(:),x(size(b))
    integer::k,n,i,j,imax,m
    real::suma,aux1(size(b)),aux2
    n=size(b)
    do k=1,n-1 !buscamos el pivote mayor de la fila 1 y lo permutamos
        imax=maxloc(abs(a(k:n,k)),1)
        m=imax+k-1 !te da la posicion exacta del pivote mayor a permutar dentro de la matriz general
        aux1(1:n)=a(k,1:n) 
        a(k,1:n)=a(m,1:n)
        a(m,1:n)=aux1(1:n)
        
        aux2=b(m)
        b(m)=b(k)
        b(k)=aux2
        do i=k+1,n
            b(i)=b(i)-(a(i,k)/a(k,k))*b(k)
            do j=n,k,-1
                a(i,j)=a(i,j)-(a(i,k)/a(k,k))*a(k,j)
            end do
        end do
    end do
    x(n)=(b(n)/a(n,n))
    do i=n-1,1,-1
        suma=0
        do j=i+1,n
            suma=suma+a(i,j)*x(j)
        end do
        x(i)=(1/a(i,i))*(b(i)-suma)
    end do
end subroutine
