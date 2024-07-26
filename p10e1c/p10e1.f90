program p10e1
    implicit none
    real,allocatable::nodos(:),imagenes(:)
    real::a,b,sol
    integer::nn,i,j,k
    INTERFACE
    real function integranume(nodos,imagenes,a,b)
        implicit none
        real::nodos(:),imagenes(:),a,b,x(size(nodos)),sumaa,sumab,coef(size(nodos))
        integer::i,j,k,nn
    end function
    subroutine metododirecto(nodos,imagenes,x)
        implicit none
        real::nodos(:),imagenes(:),x(size(nodos)),matriz(size(nodos),size(nodos)),suma(size(nodos))
        integer::n,i,j,k
    end subroutine
    END INTERFACE
    open(1,file="nodos_imagenes.txt")
    nn=0
    do
        read (1,*,end=22)
        nn=nn+1
    end do
    22 allocate (nodos(nn),imagenes(nn))
    rewind(1)
    print*,"Los valores de los nodos y las imagenes son:"
    do i=1,nn
        read(1,*)nodos(i),imagenes(i)
        print*,nodos(i),imagenes(i)
    end do
    print*," Dime el límite inferior (a) y el límite superior (b) para la integración numérica"
    read(*,*)a,b
    sol=integranume(nodos,imagenes,a,b)
    print*,"El valor de la integral para el intervalo[",a,",",b,"] es",sol
end program
