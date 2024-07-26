function funvec(y)
real::y(5),funvec(5) !si estamos haciendolo con dimension dinamica ponemos y(:) y funvec(size(y))
funvec=y**3 !funvec e y tienen misma dimension y tipo
return
end function
