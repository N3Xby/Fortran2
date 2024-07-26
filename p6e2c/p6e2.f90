program p6e2
    implicit none
    real,allocatable::a(:,:),b(:),bprima(:),bdesord(:),x(:),u(:,:),l(:,:),pro(:,:),proord(:,:)
    integer,allocatable::permuta(:)
    integer::n,i,j,k
    INTERFACE
        function doolitle(a,b)
            implicit none
            real::a(:,:),b(:),bprima(size(a,1)),bdesord(size(a,1)),u(size(a,1),size(a,1)),l(size(a,1),size(a&
            &,1)),doolitle(size(a,1))
            integer::n,i,j,permuta(size(a,1))
            real::suma1,suma2
        end function
        subroutine LU(a,u,l,permuta)
           implicit none
           real::a(:,:),u(:,:),l(:,:),pro(size(a,1),size(a,1)),proord(size(a,1),size(a,1))
           integer::n,i,j,k,imax,m,permuta(size(a,1))
           real::aux1(size(a,1)),aux2
       end subroutine
    END INTERFACE
    open(1,file="elementos_matriz_ampliada.txt")
    n=0
    do      !esta es otra forma para leer directamente la n, sin suponer un vector de 1000 elementos
        read(1,*,end=22)
        n=n+1
    end do
    22 allocate(a(n,n),b(n),bdesord(n),x(n),u(n,n),l(n,n),pro(n,n),proord(n,n),permuta(n))
    rewind(1)
    do i=1,n
        read(1,*)a(i,1:n),b(i) !guarda 4 elementos de a con el formato (i,1:n) y guarda el primero de b
    end do
    x=doolitle(a,b)
    print*,"Los valores de los coeficientes son:"
    do i=1,n
        write(*,*)x(i)
    end do
end program
