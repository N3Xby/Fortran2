subroutine deriva(nodos,imagenes,puntos,h,prime,segun)
    implicit none
    real::nodos(:),imagenes(:),puntos(:),h,prime(size(puntos)),segun(size(puntos))
    integer::nn,np,i,j,k
    nn=size(imagenes)
    np=size(puntos)
    call metododirecto(nodos,imagenes,puntos,x)
