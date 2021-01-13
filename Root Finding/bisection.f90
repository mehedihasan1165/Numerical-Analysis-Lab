program bisection
    !equation::x^3+x^2+x+7=0
    implicit none
    real a, b, p
    real, external :: f
    real, parameter :: error = 0.00001
    integer i, n
    
    print *, "Enter a(start interval) and b(end interval) and n(iterations)"
    read *, a, b, n

    do while (f(a)*f(b) > 0)
        print *, "No root contains in the given interval. Try again"
        print *, "Enter a(start interval) and b(end interval)"
        read *, a, b
    end do

    write (*, "(2x,'N',11x,'P',10x,'f(P)')")
    do i = 1, n
        p = (a + b)/2.0
        write (*, "(1x,i3,3x,1x,f9.5,3x,1x,f9.5)") i, p, f(p)
        if (f(p) == 0.0 .or. (abs((b - a)/2.0)) < error) then
            write (*, "('The root is P=',1x,f8.4)") p
            stop
        end if
        if (f(p) > 0) then
            a = p
        else
            b = p
        end if
    end do
    
    write (*, "('NO ROOT FOUND AFTER',1x,i3,1x,'ITERATIONS')") n

end program bisection

real function f(x)
    implicit none
    real, intent(in) :: x

    f = x**3 + x**2 + x + 7

end function f
