program p2e7
    implicit none
    real::x1,x2,suma1,suma2
    external:: metodo
    INTERFACE       
        function fun1(x1)
            implicit none
            real::x1,fun1(2)
        end function

        function fun2(x2)
            implicit none
            real::x2,fun2(2)
        end function
    END INTERFACE
    Print*,"Digame los valores de X para calcular las sumas de las funcinones con sus derivadas"
    Read(*,*)x1,x2
    call metodo(x1,fun1,suma1)
    print*,"El valor para la suma de la primera función es:",suma1
    call metodo(x2,fun2,suma2)
    print*,"El valor para la suma de la segunda función es:",suma2
end program


        !subroutine metodo(xm,funm,sumam)
         !   implicit none
          !  real::xm,sumam
           ! INTERFACE
            !    function funm(xm)
             !       implicit none
              !      real::xm,funm(2)
               ! end function
         !   END INTERFACE
       ! end subroutine
