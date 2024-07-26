program p2e1
implicit none
real::a,funcionx3,calculo
print*,"Dime el valor para calcular la funcion x³"
read(5,*)a
call subfuncionx3(a,calculo)
write(6,*)calculo
write(6,*)funcionx3(a)
end program
