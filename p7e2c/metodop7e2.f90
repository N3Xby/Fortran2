function lagrange(nodos,imagenes,puntos)
    implicit none
    real::nodos(:),imagenes(:),puntos(:),lagrange(size(puntos)),nume,deno
    integer::n,m,i,j,k
    INTERFACE
        function gauss(a,b)
            implicit none
            real::a(:,:),b(:),gauss(size(b))
            integer::k,n,i,j,imax,m
            real::suma,aux1(size(b)),aux2
        end function
    END INTERFACE
    n=size(imagenes)
    m=size(puntos)
    do i=1,m
        lagrange(i)=0
        do k=1,n
            nume=1
            deno=1
            do j=1,n
                if (k.ne.j) then
                    nume=nume*(puntos(i)-nodos(j))
                    deno=deno*(nodos(k)-nodos(j))
                end if
            end do
            lagrange(i)=lagrange(i)+((nume/deno)*imagenes(k))
        end do
    end do  
end function
