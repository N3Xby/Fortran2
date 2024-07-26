program p3e1
    implicit none
    
    open(2,file='pinta.gnpl')
    
    write(2,*)'plot [-5:5] [-20:20] x**2-exp(x) lt rgb "red" title "Función trabajo 1", 2*x**3-exp(x) lt rgb "blue" &
    & title "Función trabajo 2"' 
    !lt lo pinta en linea, pt lo pinta en puntos, show palette colornames (en consola te dice los colores)
    
    write(2,*) 'set xzeroaxis'
    write(2,*) 'set yzeroaxis'
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
    
    write(2,*) 'set title "Funciones trabajo de la Practica 3"'
    write(2,*) 'set ylabel "f(x)"'
    write(2,*) 'set xlabel "x"'
    write(2,*) 'replot'
    
    write(2,*) 'set terminal png'
    write(2,*) 'set output "figura.png"'
    
    close(2)
    
    !llamo a GNUPLOT de dos posibles maneras en Windows
    
    !call system('start gnuplot -p pinta.gnpl')
    !call system('"C:\Program File\gnuplot\bin\gnuplot" -p pinta.gnpl')
    
    !llamo en linux y mac
    call system('gnuplot -p pinta.gnpl')
    
    !Desde fuera de Fortran, hay qye abrir GNUPLOT y dirigirse al directorio de trabajo y escribir:

!   set title "Funciones trabajo Practica 3"
!   set ylabel "f(x)"
!   set xlabel "x"
!   plot [-5:5] [-20:20] 0 title "",x**2-exp(x) title "Función trabajo1", 2*x**3-exp(x) title "Función trabajo 2"
!   set terminal png
!   set output "figura.png"
!   replot
  
end program
    
