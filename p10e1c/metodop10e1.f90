real function integranume(nodos,imagenes,a,b)
    implicit none
    real::nodos(:),imagenes(:),a,b,x(size(nodos)),sumaa,sumab,coef(size(nodos))
    integer::i,j,k,nn
    INTERFACE
        subroutine metododirecto(nodos,imagenes,x)
            implicit none
            real::nodos(:),imagenes(:),x(size(nodos)),matriz(size(nodos),size(nodos)),suma(size(nodos))
            integer::n,i,j,k
        end subroutine
    END INTERFACE
    nn=size(nodos)
    call metododirecto(nodos,imagenes,x)
    coef=x
    sumab=0
    !Tenemos los coeficientes del polinomio del metodo directo, ahora tenemos que integrar el polinomio de este metodo
    !Una vez tenemos la integración, nos damos cuenta que la integración es integrar desde b a a este polinomio.
    !Así, calculamos ambos extremos y los restamos para obtener el valor de la integral.
    do i=1,nn
        sumab=sumab+(coef(i)*((b**i)/i))
    end do
    sumaa=0
    do i=1,nn
        sumaa=sumaa+(coef(i)*((a**i)/i))
    end do
    integranume=sumab-sumaa
end function
