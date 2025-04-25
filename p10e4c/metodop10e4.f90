subroutine trapecio(a,b,pre,ite,fun,resultado)
    implicit none
    real::a,b,pre,ite,fun,resultado(2),trape0
    integer::i,j,k,n
    INTERFACE
        real function fun1(x)
            implicit none
            real::x
        end function
        real function fun2(x)
            implicit none
            real::x
        end function
    END INTERFACE
    print*,"La aproximacion cero será:"
    trape0=((b-a)/2)*(fun1(a)+fun1(b))
    print*,trape0
    n=ite
    do k=1,n
    	h(k)=(b-a)/(2**k)
    end do
    do k=1,n
        x
    	
