program p8e1
    implicit none
    real,allocatable::nodos(:),imagenes(:),puntos(:),interpol(:)
    integer::nn,np,i,j,k
    INTERFACE
	    function newton(nodos,imagenes,puntos)
		    implicit none
		    real::nodos(0:),imagenes(0:),puntos(:),newton(1:size(puntos))
		    integer::n,np
		    integer::i,j,p
		    real::a(0:size(nodos)-1,0:size(nodos)-1),c(0:size(nodos)-1),prod,suma
		end function
    END INTERFACE
    open(1,file="nodos_imagenes.txt")
    nn=0
    do
        read (1,*,end=22)
        nn=nn+1
    end do
    22 allocate (nodos(0:nn-1),imagenes(0:nn-1))
    rewind(1)
    print*,"Los valores de los nodos y las imagenes son:"
    do i=0,nn-1
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
    interpol=newton(nodos,imagenes,puntos)

    
    
    !open(3,file="salida.txt")
    !do i=1,n
    !	write(1,*)puntos(i),interpol(i)
    !end do
    !close(3)
    
