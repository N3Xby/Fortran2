subroutine EDO(x,y0,fun,explicito,centrado,heun)
    implicit none
!Para que de forma supuesta se siga guardando que un vector empieza en 0, se tiene que poner "0:".
    real :: x(0:)
    real, dimension(0:size(x)-1) :: explicito,centrado,heun
    real :: y0,fun,h
    integer :: m,i
    external :: fun

!Importante tener en cuenta que los vectores empiezan en 0.
    m=size(x)-1
    h=(x(m)-x(0))/(size(x)-1)

!En este caso no debemos sobreescribir los valores en un proceso iterativo, se irán guardando todos en un vector.
!En este caso de aprecia mucho más claro el parecido con las fórmulas teóricas.
    explicito(0)=y0
    do i=0,m-1
        explicito(i+1)=explicito(i)+h*fun(x(i),explicito(i))
    end do

    heun(0)=y0
    do i=0,m-1
        heun(i+1)=heun(i)+(h/2.)*(fun(x(i),heun(i))+fun(x(i+1),heun(i)+h*fun(x(i),heun(i))))
    end do

    centrado(0)=y0
    centrado(1)=y0+h*fun(x(0),y0)
    do i=0,m-2
        centrado(i+2)=centrado(i)+2*h*fun(x(i+1),centrado(i+1))
    end do

end subroutine