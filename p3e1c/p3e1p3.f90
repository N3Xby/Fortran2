program p3e1p3
    implicit none
    real::x(101),y1(101),y2(101)
    integer::i
    x=(/(i/10.,i=-50,50)/)
    y1=x**2-exp(x)
    y2=2*x**3-exp(x)
    open(1,file='salida.txt')
    do i=1,size(x)
        write(1,'(f4.1,2x,f6.1,2x,f6.1)')x(i),y1(i),y2(i)
    end do
    close(1)
    open(2,file='pinta2.gnpl')
    
    write(2,*)'plot [-5:5] [-20:20] "salida.txt" using 1:2 w l title "Función trabajo 1"&
    &,"salida.txt" using 1:3 w l title "Función trabajo 2"'
    !lt lo pinta en linea, pt lo pinta en puntos, show palette colornames (en consola te dice los colores)
    
    write(2,*) 'set xzeroaxis           #se puede poner set xrange [-5:5]'
    write(2,*) 'set yzeroaxis           #y set yrange [-20:20] en vez de ponerlo en el plot'
    write(2,*) 'set border 0            #quitar marco alrededor'
    write(2,*) 'set xtics axis          #incluir tics en eje x'
    write(2,*) 'set xtics -5,1,5        #limites e incrementos entre tics del eje x'
    write(2,*) 'set ytics axis          #incluir tics eje y'
    write(2,*) 'set grid xtics ytics    #malla'
    write(2,*) 'set grid                #habilitar la malla'
    write(2,*) 'set ticscale 0'
    write(2,*) 'set xtics add ("" 0)'
    write(2,*) 'set ytics add ("" 0)'
    
    write(2,*) '# incluye flechas direccion positiva ejes'
    write(2,*) 'set arrow 1 from 0,0 to graph 1, first 0 filled head'
    write(2,*) 'set arrow 2 from 0,0 to first 0, graph 1 filled head'
    
    !write(2,*) '#incluye flechas en todas las direcciones'
    !write(2,*) 'set arrow 3 from 0,0 to graph 0, first 0 filled head'
    !write(2,*) 'set arrow 4 from 0,0 to first 0, graph 0 filled head'
    
    write(2,*) 'set title "Funciones trabajo de la Practica 3 parte 3"'
    write(2,*) 'set ylabel "f(x)"'
    write(2,*) 'set xlabel "x"'
    write(2,*) 'replot'
    
    write(2,*) 'set terminal png'
    write(2,*) 'set output "figura2.png"'
    
    close(2)
    
    !llamo a GNUPLOT de dos posibles maneras en Windows
    
    !call system('start gnuplot -p pinta.gnpl')
    !call system('"C:\Program File\gnuplot\bin\gnuplot" -p pinta.gnpl')
    
    !llamo en linux y mac
    call system('gnuplot -p pinta2.gnpl')
    
    !Desde fuera de Fortran, hay qye abrir GNUPLOT y dirigirse al directorio de trabajo y escribir:

!   set title "Funciones trabajo Practica 3"
!   set ylabel "f(x)"
!   set xlabel "x"
!   plot [-5:5] [-20:20] 0 title "",x**2-exp(x) title "Función trabajo1", 2*x**3-exp(x) title "Función trabajo 2"
!   set terminal png
!   set output "figura.png"
!   replot
  
end program
