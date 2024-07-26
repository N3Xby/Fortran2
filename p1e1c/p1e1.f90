program p1e1
implicit none
!declaración variables dinámicas
Real,allocatable::a(:),b(:),c(:),d(:,:),e(:,:)
real::aux(9999),maximo,minimo,maxpos(1),minpos(1),sumapc,sumaimc,pivote,sumacol2,productofila4
integer,parameter::nmax=9999
Integer::dima,dimb,i,j,dimc
!saco dimensión de a y sus valores
write(6,*)"Introduce el número de elementos de a"
read(5,*)dima
allocate(a(dima))
write(6,*)"Dame los elementos que forman el vector a, atendiendo a su dimensión"
read(5,*)(a(i),i=1,dima)
!saco dimensión de b y sus valores a partir del archivo elementosB
allocate(b(dimb))
open(1,file="elementosB.txt")
do i=1,nmax
    read(1,*,end=99)aux(i)
end do
99 dimb=i-1
do i=1,dimb
	b(i)=aux(i)
end do
write(6,*)(b(i),i=1,dimb)
write(6,*)"La dimensión del vector b es",dimb
!generamos un vector c con la union de los vectores a y b
allocate(c(dima+dimb))
c=(/a,b/)   !concatenamos
dimc=dima+dimb
print*,(c(i),i=1,dimc)
maximo=maxval(c)
minimo=minval(c)
maxpos=maxloc(c)    !para que sea un real sin tener que poner (1) en la declaracion ponemos maxloc(c,1)
minpos=minloc(c)    !si hay más de un minimo o maximo mejor declaramos el vector
print*,"máximo=",maximo,"mínimo=",minimo,"posición del max=",maxpos,"posición del min=",minpos
!ahora por el método alternativo sin funciones intrínsecas
do i=1,dimc-1
    do j=1,dimc
        if (c(i)>c(j)) then
        pivote=c(j)
        c(j)=c(i)
        c(i)=pivote
        end if
    end do
end do
if (c(1)
print*,"máximo=",c(1),"mínimo=",c(dimc),"posición del max=",maxpos,"posición del min=",minpos



!Sumamos los elementos pares del vector c
sumapc=sum((/(c(i),i=2,dimc,2)/))
sumapc=0
do i=2,dimc,2
    sumapc=sumapc+c(i)
end do
print*,sumapc
!Sumamos los elementos impares del vector c
sumaimc=sum((/(c(i),i=1,dimc,2)/))
sumaimc=0
do i=1,dimc,2
    sumaimc=sumaimc+c(i)
end do
print*,sumaimc
!construimos un nuevo vector a de forma implicita (do)
deallocate(a)
allocate (a(15))
a=(/(i,i=1,15)/)
print*,(a(i),i=1,15)
!cosntruimos un nuevo vector b de forma explicita (constructor de vectores)
deallocate(b)
allocate (b(15))
b=(/16,17,18,19,20,21,22,23,24,25,26,27,28,29,30/)
print*,(b(i),i=1,15)
!generamos un nuevo vector c en el que cada elemento es la raiz cuadrada del producto de los valores A y B
deallocate(c)
allocate (c(15))
do i=1,15
    c(i)=sqrt(a(i)*b(i))
end do
print*,(c(i),i=1,15)
!Asignamos la dimension 5x3 a D y la generamos a partir del nuevo vector A.
allocate (d(5,3))
d=reshape(a,(/5,3/),order=(/1,2/))
do i=1,5
    write(6,*)(d(i,j),j=1,3)
end do
!Ahora lo hacemos por el método alternativo sin función intrínseca
do i=1,5
    do j=1,3
        d(i,j)=a(i)
    end do
end do
!calculamos la suma de los elementos de la columna 2 de la matriz D y el producto de los elementos de la fila 4
sumacol2=0
do i=1,5
    sumacol2=sumacol2+d(i,2)
end do
print*,sumacol2
productofila4=1
do j=1,3
    productofila4=productofila4*d(4,j)
end do
print*,productofila4
!generamos una matriz E en el que cada valor sea el doble que el valor de la matriz D
allocate (e(5,3))
e=reshape(2*d,(/5,3/),order=(/1,2/))
do i=1,5
    write(6,*)(e(i,j),j=1,3)
end do
!generamos una nueva matriz E de dimensiones 3x2 a partir de elementos de la matriz D
deallocate(e)
allocate(e(3,2))
e=reshape((/8,9,10,13,14,15/),(/3,2/),order=(/1,2/))
do i=1,3
    write(6,*)(e(i,j),j=1,2)
end do
!generamos un nuevo vector A con los elementos de la nueva matriz E mediante funciones intrínsecas y sin
deallocate(a)
allocate(a(6))
a=reshape(e,(/6/))
