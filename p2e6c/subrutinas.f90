subroutine sub1(x1,vv1)
    implicit none
    real::x1,vv1(2)
    vv1(1)=x1**2
    vv1(2)=2*x1
    return
end subroutine

subroutine sub2(x2,vv2)
    implicit none
    real::x2,vv2(2)
    vv2(1)=x2**3
    vv2(2)=3*x2**2
    return
end subroutine
