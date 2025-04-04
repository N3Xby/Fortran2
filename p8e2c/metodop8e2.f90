function newtongreg(nodos,imagenes,puntosbuenos)
    implicit none
    real::nodos(0:),imagenes(0:),puntosbuenos(:),newtongreg(1:size(puntosbuenos))
    integer::n,np
    integer::i,j,p,k
    real::ng(size(puntosbuenos)),df(0:size(nodos)-1),piv,s,h

    n=size(nodos)-1
    np=size(puntosbuenos)
!ordenamos los nodos de menor a mayor.
    do i=0,n-1
        do j=i+1,n
            if (nodos(j)<nodos(i)) then
                piv=nodos(i)
                nodos(i)=nodos(j)
                nodos(j)=piv

                piv=imagenes(i)
                imagenes(i)=imagenes(j)
                imagenes(j)=piv
            end if
        end do
    end do

!calculamos el paso con los primeros nodos, y comprobamos que todos estén equiespaciados.
    h=nodos(1)-nodos(0)
    do i=0,n-1
        if ((nodos(i+1)-nodos(i))/=h) then
            return
        end if
    end do
!iniciamos el vector de diferencias finitas con el vector imágenes.
    df=imagenes
!los sucesivos elementos a partir del primero se irán sobreescribiendo hasta llegar a todas las f0.
!para ello, en cada iteración i, se sobreescriben los términos a partir del i-ésimo.
!de la forma en la que se sobreescriben es restando al propio vector los elementos de una posición atrasada.
!si no se ve claro, comprobar en papel.
    do i=1,n
        df(i:)=df(i:)-df(i-1:n-i)
    end do
!calculamos las soluciones con la fórmula general.
    do k=1,np
        s=(puntosbuenos(k)-nodos(0))/h
        newtongreg(k)=imagenes(0)+sum((/((product((/((s-j),j=0,i-1)/))/product((/(j,j=1,i)/)))*df(i),i=1,n)/))
    end do
    end function