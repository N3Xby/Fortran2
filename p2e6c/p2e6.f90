program p2e5
    implicit none
    real::x1,x2,vv1(2),vv2(2),resu1,resu2
     INTERFACE
        real function metodo(xm,vvm,subm)
            implicit none
            real::xm,vvm(2)
            INTERFACE
                subroutine subm(xm,vvm)
                implicit none
                real:: xm,vvm(2)
                end subroutine
            END INTERFACE
        end function

        subroutine sub1(x1,vv1)
            implicit none
            real::x1,vv1(2)
        end subroutine

        subroutine sub2(x2,vv2)
            implicit none
            real::x2,vv2(2)
        end subroutine
     END INTERFACE
    Print*,"Digame los valores de X para calcular las sumas de las funcinones con sus derivadas"
    Read(*,*)x1,x2
    resu1=metodo(x1,vv1,sub1)
    resu2=metodo(x2,vv2,sub2)
    Print*,"El valor para la suma de la primera función es",resu1,"y para la segunda es",resu2
end program
