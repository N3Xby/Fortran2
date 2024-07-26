program p6e1
    implicit none
    real,allocatable::a(:,:),u(:,:),l(:,:),pro(:,:),proord(:,:)
    integer,allocatable::permuta(:)
    integer::n,i,j,k
    INTERFACE
        subroutine LU(a,u,l,permuta)
            implicit none
            real::a(:,:),u(:,:),l(:,:),pro(size(a,1),size(a,1)),proord(size(a,1),size(a,1))
            integer::n,i,j,k,imax,m,permuta(size(a,1))
            real::aux1(size(a,1)),aux2
        end subroutine
    END INTERFACE
    open(1,file="elementos_matriz.txt")
    n=0
    do      !esta es otra forma para leer directamente la n, sin suponer un vector de 1000 elementos
        read(1,*,end=22)
        n=n+1
    end do
    22 allocate(a(n,n),u(n,n),l(n,n),pro(n,n),proord(n,n),permuta(n))   !alocatamos cada vector
    rewind(1)
    do i=1,n    !leemos los elementos de la matriz
        read(1,*)a(i,1:n)
    end do
    
    call LU(a,u,l,permuta)
    
    print*,"La matriz triangular superior U es:"
    do i=1,n
        print*, (u(i,j),j=1,n)
    end do
    
    print*,"La matriz triangular inferior L es:"
    do i=1,n
        print*, (l(i,j),j=1,n)
    end do
    
    print*,"El producto matricial de L*U es:"
    pro=matmul(l,u)
    
    do i=1,n
        print*, (pro(i,j),j=1,n)
    end do
    
    print*, "Vector pivotamiento", permuta
    
    do i=1,n
        proord(permuta(i),1:n)=pro(i,1:n)
    end do
    
    print*, "La matriz resultante del producto L*U ordenada es:"
    do i=1,n
        print*, (proord(i,j),j=1,n)
    end do
    
    stop
   
end program
