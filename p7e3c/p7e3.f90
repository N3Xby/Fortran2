program p7e3
    implicit none
    real,allocatable::nodos(:),imagenes(:),puntos(:),y(:)
    integer::n,m,i,j,k
    INTERFACE
        subroutine metododirecto(nodos,imagenes,puntos,y)
            implicit none
            real::nodos(:),imagenes(:),puntos(:),y(size(puntos)),x(size(puntos)),matriz(size(imagenes),&
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
    
    do i=1,m
    	if (puntos(i).lt.nodos(1)) then
    	    puntos(1:m-1) = (/ puntos(1:i-1), puntos(i+1:m) /)
    	    m = m+1
    	else if (puntos(i).gt.nodos(n)) then
    	    puntos(1:m-1) = (/ puntos(1:i-1), puntos(i+1:m) /)
    	    m = m-1
    	else
    	end if
    end do
    
    print*,"Los valores de los puntos que se pueden interpolar son:"
    do i=1,m
        print*,puntos(i)
    end do
    
    call metododirecto(nodos,imagenes,puntos,y)
    print*,"Las interpolaciones de cada uno de los puntos dados son:"
    do i=1,m
        write(*,*)y(i)
    end do
end program
