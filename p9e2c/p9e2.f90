program p9e2
    implicit none
    real,allocatable::nodos(:),imagenes(:),puntos(:),interpol(:),h
    integer::nn,np,i,j,k
    INTERFACE
        
        subroutine
        
        subroutine metododirecto(nodos,imagenes,puntos,x)
            implicit none
            real::nodos(:),imagenes(:),puntos(:),metododirecto(size(puntos)),x(size(puntos)),matriz(size(imagenes),&
            &size(imagenes)),suma(size(imagenes)),valpoli(size(puntos),size(imagenes))
            integer::n,m,i,j,k
        end subroutine
        
        function gauss(a,b)
            implicit none
            real::a(:,:),b(:),gauss(size(b))
            integer::k,n,i,j,imax,m
            real::suma,aux1(size(b)),aux2
        end function
    END INTERFACE
    open(1,file="nodos_imagenes.txt")
    nn=0
    do
        read (1,*,end=22)
        nn=nn+1
    end do
    22 allocate (nodos(n),imagenes(n))
    rewind(1)
    print*,"Los valores de los nodos y las imagenes son:"
    do i=1,nn
        read(1,*)nodos(i),imagenes(i)
        print*,nodos(i),imagenes(i)
    end do
    open(2,file="puntos.txt")
    np=0
    do
        read(2,*,end=23)
        np=np+1
    end do
    23 allocate (puntos(np),interpol(np))
    rewind(2)
    print*,"Los valores de los puntos son:"
    do i=1,np
        read(2,*)puntos(i)
        print*,puntos(i)
    end do
    print*, "Dime el paso h a aplicar en la derivacion"
    read(*,*)h
    call deriva(nodos,imagenes,puntos,h,prime,segun)
