program p8e2
    implicit none
    real,allocatable::nodos(:),imagenes(:),puntos(:),puntosbuenos(:),interpol(:)
    integer::nn,np,i,j,k
    INTERFACE
        function newtongreg(nodos,imagenes,puntosbuenos)
            implicit none
            real::nodos(0:),imagenes(0:),puntosbuenos(:),newtongreg(1:size(puntosbuenos))
            integer::n,np
            integer::i,j,p,k
            real::ng(size(puntosbuenos)),df(0:size(nodos)-1),piv,s,h
		end function
    END INTERFACE
    open(1,file="nodos_imagenes.txt")
    nn=0
    do
        read (1,*,end=22)
        nn=nn+1
    end do
    22 allocate (nodos(0:nn-1),imagenes(0:nn-1))
    rewind(1)
    print*,"Los valores de los nodos y las imagenes son:"
    do i=0,nn-1
        read(1,*)nodos(i),imagenes(i)
        print*,nodos(i),imagenes(i)
    end do
    open(2,file="puntos.txt")
    np=0
    do
        read(2,*,end=23)
        np=np+1
    end do
    23 allocate (puntos(np),interpol(np))
    rewind(2)
    print*,"Los valores de los puntos son:"
    do i=1,np
        read(2,*)puntos(i)
        print*,puntos(i)
    end do
    
    k=0
    do i=1,np
    	if (puntos(i)>=minval(nodos) .and. puntos(i)<=maxval(nodos) ) then
    	    k = k+1
    	end if
    end do
    allocate (puntosbuenos(k))
    k=0
    do i=1,np
        if (puntos(i)>=minval(nodos) .and. puntos(i)<=maxval(nodos) ) then
            k=k+1
            puntosbuenos(k)=puntos(i)
        end if
    end do
                   
    print*,"Los valores de los puntos que se pueden interpolar son:"
    do i=1,k
        print*,puntosbuenos(i)
    end do
    
    interpol=newtongreg(nodos,imagenes,puntosbuenos)

    print*,"Los valores de las interpolaciones son:"
    do i=1,np
        write(6,*)interpol(i)
    end do

    open(3,file="salida.txt")
    do i=0,nn-1
        write(3,*)nodos(i),imagenes(i)
    end do
    do i=1,k
    	write(3,*)puntosbuenos(i),interpol(i)
    end do
    close(3)
    
    open (8,file="pinta.gnpl")
    
    write(8,*)'plot [-5:5] [-20:20] "salida.txt"'
    !lt lo pinta en linea, pt lo pinta en puntos, show palette colornames (en consola te dice los colores)
    
    write(8,*) 'set xzeroaxis'
    write(8,*) 'set yzeroaxis'
    write(8,*) 'set border 0            #quitar marco alrededor'
    write(8,*) 'set xtics axis          #incluir tics en eje x'
    write(8,*) 'set xtics -5,1,5        #limites e incrementos entre tics del eje x'
    write(8,*) 'set ytics axis          #incluir tics eje y'
    write(8,*) 'set grid xtics ytics    #malla'
    write(8,*) 'set grid                #habilitar la malla'
    write(8,*) 'set ticscale 0'
    write(8,*) 'set xtics add ("" 0)'
    write(8,*) 'set ytics add ("" 0)'
    
    write(8,*) '# incluye flechas direccion positiva ejes'
    write(8,*) 'set arrow 1 from 0,0 to graph 1, first 0 filled head'
    write(8,*) 'set arrow 2 from 0,0 to first 0, graph 1 filled head'
    
    !write(8,*) '#incluye flechas en todas las direcciones'
    !write(8,*) 'set arrow 3 from 0,0 to graph 0, first 0 filled head'
    !write(8,*) 'set arrow 4 from 0,0 to first 0, graph 0 filled head'
    
    write(8,*) 'set title "Funciones trabajo de la Practica 3"'
    write(8,*) 'set ylabel "f(x)"'
    write(8,*) 'set xlabel "x"'
    write(8,*) 'replot'
    
    write(8,*) 'set terminal png'
    write(8,*) 'set output "figura.png"'
    
    close(8)
    
    call system('gnuplot -p pinta.gnpl')
    
end program
