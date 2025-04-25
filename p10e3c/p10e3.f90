program p10e3
	implicit none
	real,allocatable::nodos(:),imagenes(:)
	real::sol
	integer::i,j,k,nn
	INTERFACE
        real function simpson(nodos,imagenes)
            implicit none
            real::nodos(:),imagenes(:),h(size(nodos)-1),piv
            integer::i,j,k,nn
        end function
	END INTERFACE
	open(1,file="nodos_imagenes.txt")
	nn=0
	do
	    read(1,*,end=22)
	    nn=nn+1
    end do
    22 allocate(nodos(nn),imagenes(nn))
    rewind(1)
    print*,"Los valores de los nodos e imagenes son:"
    do i=1,nn
        read(1,*) nodos(i),imagenes(i)
        write(*,*) nodos(i),imagenes(i)
    end do
    sol=simpson(nodos,imagenes)
    write(*,*) "El resultado de la integral es:",sol
end program
