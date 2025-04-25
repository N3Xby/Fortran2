subroutine trapecio(a,b,pre,ite,fun,resultado)
    implicit none
    real::a,b,pre,ite,fun,resultado(2),h,aux
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
    resultado(1)=((b-a)/2)*(fun(a)+fun(b))
    print*,resultado(1)
    k=0
    do while (abs(resultado(1)-aux)>pre)
        k=k+1
        if (k>ite) then
            goto 99
        end if

        aux=resultado(1)
        h=(b-a)/(2**k)
        resultado(1)=(aux/2.)+h*sum((/(fun(a+(2*j-1)*h),j=1,2**(k-1))/))
    end do
    99 continue
    resultado(2)=k
end subroutine trapecio
