subroutine LU(a,u,l,permuta)
    implicit none
    real::a(:,:),u(:,:),l(:,:),pro(size(a,1),size(a,1)),proord(size(a,1),size(a,1))
    integer::n,i,j,k,imax,m,permuta(size(a,1))
    real::aux1(size(a,1)),aux2
    n=size(a,1)
    permuta=(/(i,i=1,n)/)
    l=a
    u=a
    do k=1,n-1 !buscamos el pivote mayor de la fila 1 y lo permutamos
        imax=maxloc(abs(u(k:n,k)),1)
        m=imax+k-1 !te da la posicion exacta del pivote mayor a permutar dentro de la matriz general
        aux1(1:n)=u(k,1:n) !cambiamos las filas de la matriz U
        u(k,1:n)=u(m,1:n)
        u(m,1:n)=aux1(1:n)
        
        aux1(1:n)=l(k,1:n) !cambiamos las filas de la matriz L
        l(k,1:n)=l(m,1:n)
        l(m,1:n)=aux1(1:n)
        
        aux2=permuta(k) !guardamos el orden de permutación
        permuta(k)=permuta(m)
        permuta(m)=aux2
        !hacemos 1 la diagonal de L
        l(k,k)=1
        !hacemos 0 los elementos a la derecha de la diagonal de L
        do j=k+1,n
            l(k,j)=0
        end do
        !calculamos los nuevos elementos de las matrices L y U
        do i=k+1,n
            l(i,k)=u(i,k)/u(k,k)
            do j=n,k,-1
                u(i,j)=u(i,j)-l(i,k)*u(k,j)
            end do
        end do
    end do
    !incluimos el k-esimo 1 de la diagonal principal de L que faltaba
    l(n,n)=1
end subroutine
