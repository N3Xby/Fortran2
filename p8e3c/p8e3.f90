program p8e2
    implicit none
    real,allocatable::nodos(:),imagenes(:),puntos(:),puntosbuenos(:),interpol(:),df(:,:)
    integer::nn,np,i,j,k
    INTERFACE
        subroutine newtongreg(nodos,imagenes,puntosbuenos,df)
            implicit none
            real::nodos(0:),imagenes(0:),puntosbuenos(:)
            integer::n,np
            integer::i,j,p,k
            real::df(0:size(nodos)-1,0:size(nodos)-1),piv,s,h
        end subroutine
    END INTERFACE
    open(1,file="nodos_imagenes.txt")
    nn=-1
    do
        read (1,*,end=22)
        nn=nn+1
    end do
    22 allocate (nodos(0:nn),imagenes(0:nn),df(0:nn,0:nn))
    rewind(1)
    print*,"Los valores de los nodos y las imagenes son:"
    do i=0,nn
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

    k=0
    do i=1,np
        if (puntos(i)>=minval(nodos) .and. puntos(i)<=maxval(nodos) ) then
            k = k+1
        end if
    end do
    allocate (puntosbuenos(k))
    k=0
    do i=1,np
        if (puntos(i)>=minval(nodos) .and. puntos(i)<=maxval(nodos) ) then
            k=k+1
            puntosbuenos(k)=puntos(i)
        end if
    end do

    print*,"Los valores de los puntos que se pueden interpolar son:"
    do i=1,k
        print*,puntosbuenos(i)
    end do

    call newtongreg(nodos,imagenes,puntosbuenos,df)
    np=size(puntosbuenos)
    write(*,*) "Resultados de las interpolaciones:"
    do k=1,np
        interpol(np)=imagenes(0)+sum((/((product((/(((puntosbuenos(k)-nodos(0))/(nodos(1)-nodos(0))-j),j=0,i-1)/))&
        &/product((/(j,j=1,i)/)))*df(0,i),i=1,nn)/))
        write(*,*) interpol(np)
    end do

    print*,"Los valores de las diferencias finitas en forma de matriz son:"
    do i=0,nn
        write(*,*) (df(i,j),j=0,nn-i)
    end do

    end program p8e2