program p2e5
    implicit none
    real,allocatable::x(:),x3(:)
    real::resultado
    integer::i,dimx
    INTERFACE
        real function funvecreal(x)
            real::x
        end function

        real function funvecreal2(x)
            real::x(:),cubo(size(x))
        end function
    END INTERFACE
    Print*,"Dígame la dimensión del vector x"
    read(5,*) dimx
    allocate (x(dimx),x3(dimx))
    Print*,"Dígame ahora los valores de este vector x, respetando la dimesión dada"
    read(5,*) (x(i),i=1,dimx)
    do i=1,dimx
        x3(i)=funvecreal(x(i))
    end do
    resultado=sum((/(x3(i),i=1,dimx,2)/))
    write(6,*)"El resultado de la suma con el primer método es:",resultado
    write(6,*)"El resultado de la suma con el segundo método es:",funvecreal2(x)
end program
    
