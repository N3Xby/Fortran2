program p7e2
    implicit none
    real,allocatable::nodos(:),imagenes(:),puntos(:),y(:)
    integer::n,m,i,j,k
    INTERFACE
        function lagrange(nodos,imagenes,puntos)
            implicit none
            real::nodos(:),imagenes(:),puntos(:),lagrange(size(puntos)),nume,deno
            integer::n,m,i,j,k
        end function
        function gauss(a,b)
            implicit none
            real::a(:,:),b(:),gauss(size(b))
            integer::k,n,i,j,imax,m
            real::suma,aux1(size(b)),aux2
        end function
    END INTERFACE
    open(1,file="nodos_imagenes.txt")
    n=0
    do
        read (1,*,end=22)
        n=n+1
    end do
    22 allocate (nodos(n),imagenes(n))
    rewind(1)
    print*,"Los valores de los nodos y las imagenes son:"
    do i=1,n
        read(1,*)nodos(i),imagenes(i)
        print*,nodos(i),imagenes(i)
    end do
    open(2,file="puntos.txt")
    m=0
    do
        read(2,*,end=23)
        m=m+1
    end do
    23 allocate (puntos(m),y(m))
    rewind(2)
    print*,"Los valores de los puntos son:"
    do i=1,m
        read(2,*)puntos(i)
        print*,puntos(i)
    end do
    y=lagrange(nodos,imagenes,puntos)
    print*,"Las interpolaciones de cada uno de los puntos dados son:"
    do i=1,m
        write(*,*)y(i)
    end do
end program
