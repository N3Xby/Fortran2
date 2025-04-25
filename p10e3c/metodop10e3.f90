real function simpson(nodos,imagenes)
    implicit none
    real::nodos(:),imagenes(:),h(size(nodos)-1),piv
    integer::i,j,k,nn
    nn=size(nodos)

!ordenamos de menor a mayor los nodos e imagenes
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
!calculamos el valor de la integral
    simpson=0
    do i=1,nn-2,2
        simpson=simpson+(h(i)/3)*(imagenes(i)+4*imagenes(i+1)+imagenes(i+2))
    end do
end function
