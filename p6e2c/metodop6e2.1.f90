function doolitle(a,b)
    implicit none
    real::a(:,:),b(:),bprima(size(a,1)),bdesord(size(a,1)),u(size(a,1),size(a,1)),l(size(a,1),size(a,1)),doolitle(size(a,1))
    integer::n,i,j,permuta(size(a,1))
    real::suma1,suma2
    INTERFACE
       subroutine LU(a,u,l,permuta)
           implicit none
           real::a(:,:),u(:,:),l(:,:),pro(size(a,1),size(a,1)),proord(size(a,1),size(a,1))
           integer::n,i,j,k,imax,m,permuta(size(a,1))
           real::aux1(size(a,1)),aux2
       end subroutine
    END INTERFACE
    call LU(a,u,l,permuta)
    n=size(a,1)
    print*, "Vector pivotamiento", permuta
    !generamos el vector b desordenado con permuta
    do i=1,n
        bdesord(i)=b(permuta(i))
    end do
    print*, "el vector b desordenado es:"
    do i=1,n
        print*,bdesord(i)
    end do
    !obtenemos el vector con los terminos independientes equivalente b'
    bprima(1)=bdesord(1)
    do i=2,n
        suma1=0
        do j=1,i-1
            suma1=suma1+l(i,j)*bprima(j)
        end do
        bprima(i)=bdesord(i)-suma1
    end do
    doolitle(n)=(bprima(n)/u(n,n))
    do i=n-1,1,-1
        suma2=0
        do j=i+1,n
            suma2=suma2+u(i,j)*doolitle(j)
        end do
        doolitle(i)=(1/u(i,i))*(bprima(i)-suma2)
    end do
end function
