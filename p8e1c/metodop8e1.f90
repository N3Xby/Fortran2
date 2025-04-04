function newton(nodos,imagenes,puntosbuenos)
    implicit none
    real::nodos(0:),imagenes(0:),puntosbuenos(:),newton(1:size(puntosbuenos))
    integer::n,np
    integer::i,j,p
    real::a(0:size(nodos)-1,0:size(nodos)-1),c(0:size(nodos)-1),prod,suma
    n=size(nodos)-1
    np=size(puntosbuenos)
    a(0:n,0)=1
    a(0:n,1:n)=0
    do i=1,n
        do j=1,i
            a(i,j)=a(i,j-1)*(nodos(i)-nodos(j-1))
        end do
    end do
    write(6,*) "Matriz Newton"
    do i=0,n
        write(6,*)(a(i,j),j=0,n)
    end do
    prod=1
    c(0)=imagenes(0)
    do i=1,n
        suma=0
        do j=0,i-1
            suma=suma+a(i,j)*c(j) 
        end do
        c(i)=(1/a(i,i))*(imagenes(i)-suma)
    end do
    print*,"Los valores de los coeficientes son:" 
    do i=0,n
        write(6,*)c(i)
    end do
    do i=1,np
        newton(i)=c(0)
        prod=1
        do j=1,n
            prod=prod*(puntosbuenos(i)-nodos(j-1))
            newton(i)=newton(i)+(c(j)*prod)
        end do
    end do
    print*,"Los valores de las interpolaciones son:"
    do i=1,n
        write(6,*)newton(i)
    end do
    return
end function
