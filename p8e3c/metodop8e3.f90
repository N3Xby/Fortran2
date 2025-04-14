subroutine newtongreg(nodos,imagenes,puntosbuenos,df)
    implicit none
    real::nodos(0:),imagenes(0:),puntosbuenos(:)
    integer::n,np
    integer::i,j,p,k
    real::df(0:size(nodos)-1,0:size(nodos)-1),piv,s,h

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
    df(0:,0)=imagenes
    !En esta ocasión, vamos a calcular las diferencias finitas en una matriz, en lugar de un vector.
    !La primera columna será la del vector imágenes.
    !Para las siguientes columnas, tomamos la columna anterior desde una posición adelantada, y le restamos ella misma pero desde el principio.
    !La tabla de diferencias finitas es una especie de triangular, por lo que los elementos que no nos interesan los hacemos 0.
    do j=1,n
        df(0:n-j,j)=df(1:n-(j-1),j-1)-df(0:n-j,j-1)
        df(n-j+1:n,j)=0
    end do
end subroutine