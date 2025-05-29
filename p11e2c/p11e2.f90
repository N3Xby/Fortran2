program p11e2
    use Funciones
    implicit none
    real, allocatable, dimension(:) :: x,explicito,centrado,heun
    real :: a,b,y0,h
    integer :: m,i
    INTERFACE
        subroutine EDO(x,y0,fun,explicito,centrado,heun)
            implicit none
            real :: x(:)
            real, dimension(0:size(x)-1) :: explicito,centrado,heun
            real :: y0,fun,h
            integer :: m,i
            external :: fun
        end subroutine
    END INTERFACE

!Repetimos de forma parecida al ejercicio 1.
    write(*,*) "Para la EDO 1, introduzca:"
    write(*,*) "    -El punto final."
    write(*,*) "    -El punto inicial x0."
    write(*,*) "    -El valor inicial y0."
    write(*,*) "    -El paso a aplicar."
    read(*,*) b,a,y0,h

    m=int(abs(b-a)/h)
    allocate(x(0:m))
    allocate(explicito(0:m))
    allocate(centrado(0:m))
    allocate(heun(0:m))

!Construimos el vector a mano con el constructor de vectores.
    x=(/(i/2.,i=0,40,1)/)
    call EDO(x(0:m),y0,f,explicito,centrado,heun)
    open(1,file="salida1.txt")
    do i=0,m
        write(1,*) x(i),explicito(i),centrado(i),heun(i)
    end do
    close(1)
    
!Escribimos las órdenes de gnuplot para representar los conjuntos y la función.
!Cada vez que hagamos un salto de línea en el mismo write, ponemos ",&".
!Para representar conjuntos de puntos ponemos "plot 'data.txt'".
!Para usar columnas concretas del documento ponemos "using 1:2, using 1:3...".
!Para unir los puntos con líneas ponemos "w lp" (with linepoints).
!Para ejecutar las acciones con Windows, en el call system hay que poner "start gnuplot -p".
    open(1,file="pinta1.gnpl")
    write(1,*) "set terminal png"
    write(1,*) "set output 'figura1.png'"
    write(1,*) "set title 'Práctica 11, ejercicio 2 - Función 1'"
    write(1,*) "set xlabel 't'"
    write(1,*) "set ylabel 'y(t)'"
    write(1,*) "plot 'salida1.txt' using 1:2 w lp title 'Euler explicito','salida1.txt' using 1:3 w lp title 'Euler centrado',&
    'salida1.txt' using 1:4 w lp title 'Heun',(25*exp(x-0.05*(x**2))) title 'Función analítica'"
    close(1)
    call system("start gnuplot -p pinta1.gnpl") 
    
!Reasignamos valores y repetimos para la segunda EDO.
    deallocate(x)
    deallocate(explicito)
    deallocate(centrado)
    deallocate(heun)


    write(*,*) " "
    write(*,*) "Para la EDO 2, introduzca:"
    write(*,*) "    -El punto final."
    write(*,*) "    -El punto inicial x0."
    write(*,*) "    -El valor inicial y0."
    write(*,*) "    -El paso a aplicar."
    read(*,*) b,a,y0,h

    m=int(abs(b-a)/h)
    allocate(x(0:m))
    allocate(explicito(0:m))
    allocate(centrado(0:m))
    allocate(heun(0:m))

    x=(/(i,i=0,10000,200)/)
    call EDO(x(0:m),y0,g,explicito,centrado,heun)
    open(2,file="salida2.txt")
    do i=0,m
        write(2,*) x(i),explicito(i),centrado(i),heun(i)
    end do
    close(2)
    
    open(2,file="pinta2.gnpl")
    write(2,*) "set terminal png"
    write(2,*) "set output 'figura2.png'"
    write(2,*) "set title 'Práctica 11, ejercicio 2 - Función 2'"
    write(2,*) "set xlabel 'z'"
    write(2,*) "set ylabel 'p(z)'"
    write(2,*) "plot 'salida2.txt' using 1:2 w lp title 'Euler explicito','salida2.txt' using 1:3 w lp title 'Euler centrado',&
    'salida2.txt' using 1:4 w lp title 'Heun',(101325*exp(-x/8000.)) title 'Función analítica'"
    close(2)
    call system("start gnuplot -p pinta2.gnpl") 

end program
