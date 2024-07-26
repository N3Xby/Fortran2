!Utilizamos la estructura "module" para agrupar ambas funciones vectoriales.
!Normalmente podríamos declarar todas las variables comúnes en el module, pero como en las funciones hay que declarar los argumentos de estas, no serviría de nada.
!Por ejemplo, la única variable que podríamos declarar en module sería "i", ya que no es argumento.

module funciones
implicit none

contains

    function biseccion(a,b,p,e,n,fun)
        implicit none
        real :: fun,a,b,c,p,e,biseccion(2)
        integer :: i,n
        external :: fun

        c=0
        do i=1,n+1
            c=(a+b)/2.
            
            if ((abs(fun(c))<e).AND.((b-a)<p)) then
                goto 10
            end if
            
            if (fun(a)*fun(c)<0) then
                b=c
            else if (fun(c)*fun(b)<0) then
                a=c
            else if (fun(c)==0) then
                goto 10
            end if
        end do
        
    10 continue
        n=i

        biseccion(1)=c
        biseccion(2)=n

    end function


    function falsaposicion(a,b,p,e,n,fun)
        implicit none
        real :: fun,a,b,c,p,e,falsaposicion(2)
        integer :: i,n
        external :: fun

        c=0
        do i=1,n+1
            c=a-((fun(a)*(b-a))/(fun(b)-fun(a)))
            
            if ((abs(fun(c))<e).AND.((b-a)<p)) then
                goto 10
            end if
            
            if (fun(a)*fun(c)<0) then
                b=c
            else if (fun(c)*fun(b)<0) then
                a=c
            else if (fun(c)==0) then
                goto 10
            end if
        end do
        
    10 continue
        n=i

        falsaposicion(1)=c
        falsaposicion(2)=n

    end function

end module