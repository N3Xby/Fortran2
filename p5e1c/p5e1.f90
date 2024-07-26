program p5e1
    implicit none
    real,allocatable::a(:,:),b(:),x(:)
    integer::k,n,i,j
    INTERFACE
    function gauss(a,b)
        implicit none
        real::a(:,:),b(:),gauss(size(b))
        integer::k,n,i,j,imax,m
        real::suma,aux1(size(b)),aux2
    end function
    END INTERFACE
    open(1,file="matriz_ampliada.txt")
    do i=1,1000 !lectura del archivo
        read(1,*,end=67)
    end do
    67 n=i-1
    allocate(a(n,n),b(n),x(n))
    rewind(1)
    do i=1,n
        read(1,*)a(i,1:n),b(i) !guarda 4 elementos de a con el formato (i,1:n) y guarda el primero de b
    end do
    print*,"La matriz ampliada es:"
    do i=1,n 
        write(6,*)(a(i,j),j=1,n),b(i)
    end do
    x=gauss(a,b)
    print*,"Los valores de los coeficientes son:"
    do i=1,n
        write(*,*)x(i)
    end do
end program
