real function trapecio(nodos,imagenes)
    implicit none
    real::nodos(:),imagenes(:),h(size(nodos)-1),piv
    integer::i,j,k,nn
    nn=size(nodos)
    
!ordenamos los nodos de menor a mayor.
    do i=1,nn
        do j=i+1,nn
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
!calculamos los pasos
    do i=1,nn
        h(i)=nodos(i+1)-nodos(i)
    end do
!calculamos la integral
trapecio=0
    do i=1,nn-1
        trapecio=trapecio+(h(i)/2)*(imagenes(i+1)+imagenes(i))
    end do
end function
