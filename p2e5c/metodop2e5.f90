real function metodo(xm,vvm,subm)
    implicit none
    real::xm,vvm(2)
    INTERFACE
        subroutine subm(xm,vvm)
        implicit none
        real:: xm,vvm(2)
        end subroutine
    END INTERFACE
    call subm(xm,vvm)
    metodo=sum(vvm)
    return
end function
