program p1e1
implicit none
!1 declaración variables dinámicas
Real,allocatable::a(:),b(:),c(:),d(:,:),e(:,:)
real::aux(9999),maximo,minimo,maxpos(1),minpos(1),sumapc,sumaimc,pivote,sumacol2,productofila4
integer,parameter::nmax=9999
Integer::dima,dimb,i,j,dimc
!2-3 saco dimensión de a y sus valores
write(6,*)"Introduce el número de elementos de a"
read(5,*)dima
allocate(a(dima))
write(6,*)"Dame los elementos que forman el vector a, atendiendo a su dimensión"
read(5,*)(a(i),i=1,dima)
print*, (a(i), i=1,dima)
!4 saco dimensión de b y sus valores a partir del archivo elementosB
open(1,file="elementosB.txt")
do i=1,nmax
    read(1,*,end=99)aux(i)
end do
99 dimb=i-1
allocate(b(dimb))
do i=1,dimb
	b(i)=aux(i)
end do
print*, "El vector b es el siguiente:"
write(6,*)(b(i),i=1,dimb)
write(6,*)"La dimensión del vector b es",dimb
!5 generamos un vector c con la union de los vectores a y b
dimc=size(a)+size(b)
allocate(c(dimc))
print*, "La dimensión del vector c es:",dimc
 c=(/ a,b /)   !concatenamos
write(6,*) "El vector c es el siguiente:"
print*,(c(i),i=1,dimc)
!6 a)
print*, "Ahora veamos el valor máximo y el valor mínimo del vector c, y su posición, con func intrínsecas"
maximo=maxval(c)
minimo=minval(c)    !para que sea un real sin tener que poner (1)
maxpos=maxloc(c)    !en la declaracion ponemos maxloc(c,1)
minpos=minloc(c)    !si hay más de un minimo o maximo mejor declaramos el vector
print*,"máximo=",maximo,"mínimo=",minimo,"posición del max=",maxpos,"posición del min=",minpos
!b) ahora por el método alternativo sin funciones intrínsecas
print*,"Y ahora sin funciones intrínsecas (método de la burbuja)"
do i=1,dimc-1
    do j=1,dimc
        if (c(i)>c(j)) then
        pivote=c(j)
        c(j)=c(i)
        c(i)=pivote
        end if
    end do
end do
print*,"máximo=",c(1),"mínimo=",c(dimc),"posición del max=",maxpos,"posición del min=",minpos

!7 Sumamos los elementos pares del vector c
sumapc=sum((/(c(i),i=2,dimc,2)/))
sumapc=0
do i=2,dimc,2
    sumapc=sumapc+c(i)
end do
print*,"La suma de los elementos pares del vector c es:",sumapc
!8 Sumamos los elementos impares del vector c
sumaimc=sum((/(c(i),i=1,dimc,2)/))
sumaimc=0
do i=1,dimc,2
    sumaimc=sumaimc+c(i)
end do
print*,"La suma de los elementos impares del vector c es:",sumaimc
!9 a) construimos un nuevo vector a de forma implícita (do)
deallocate(a)
allocate (a(15))
a=(/(i,i=1,15)/)
print*,"El nuevo vector a sería:",(a(i),i=1,15)
!b) cosntruimos un nuevo vector b de forma explícita (constructor de vectores)
deallocate(b)
allocate (b(15))
b=(/16,17,18,19,20,21,22,23,24,25,26,27,28,29,30/)
print*,"El nuevo vector b sería:",(b(i),i=1,15)
!10 generamos un nuevo vector c en el que cada elemento es la raiz cuadrada del producto de los valores A y B
deallocate(c)
allocate (c(15))
do i=1,15
    c(i)=sqrt(a(i)*b(i))
end do
print*,"El nuevo vector c sería:",(c(i),i=1,15)
!11-12 Asignamos la dimension 5x3 a D y la generamos a partir del nuevo vector A.
allocate (d(5,3))
d=reshape(a,(/5,3/),order=(/1,2/))
print*,"generamos la matriz D a partir del nuevo vector A:"
do i=1,5
    write(6,*)(d(i,j),j=1,3)
end do
!Ahora lo hacemos por el método alternativo sin función intrínseca
do i=1,5
    do j=1,3
        d(i,j)=a(i)
    end do
end do
!13 calculamos la suma de los elementos de la columna 2 de la matriz D y el producto de los elementos de la fila 4
sumacol2=0
do i=1,5
    sumacol2=sumacol2+d(i,2)
end do
print*,"La suma de los elementos de la columna 2 es:",sumacol2
productofila4=1
do j=1,3
    productofila4=productofila4*d(4,j)
end do
print*,"El producto de los elementos de la fila 4 es:",productofila4
!14 generamos una matriz E en el que cada valor sea el doble que el valor de la matriz D
allocate (e(5,3))
e=reshape(2*d,(/5,3/),order=(/1,2/))
print*,"La matriz E generada es:"
do i=1,5
    write(6,*)(e(i,j),j=1,3)
end do
!15 generamos una nueva matriz E de dimensiones 3x2 a partir de elementos de la matriz D
deallocate(e)
allocate(e(3,2))
e=reshape((/8,9,10,13,14,15/),(/3,2/),order=(/1,2/))
print*,"La nueva matriz E sería:"
do i=1,3
    write(6,*)(e(i,j),j=1,2)
end do
!16 generamos un nuevo vector A con los elementos de la nueva matriz E mediante funciones intrínsecas y sin
deallocate(a)
allocate(a(6))
a=reshape(e,(/6/))
print*,"El nuevo vector A sería:",(a(i),i=1,size(a))
end program
