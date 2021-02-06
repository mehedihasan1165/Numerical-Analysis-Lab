program falseposition
    !equation is 4*exp(-x)*sin(x) - 1
    implicit none
    real a, b, p0, p
    real, external :: f
    real, parameter :: error = 0.0001
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
        if (i == 1) then
            p0 = (a*f(b) - b*f(a))/(f(b) - f(a))
            write (*, "(1x,i3,3x,1x,f9.5,3x,1x,f9.5)") i, p0, f(p0)
            if (f(p0) < 0) then
                a = p0
            else
                b = p0
            end if
            continue
        end if

        p = (a*f(b) - b*f(a))/(f(b) - f(a))
        write (*, "(1x,i3,3x,1x,f9.5,3x,1x,f9.5)") i, p, f(p)

        if (f(p) == 0.0 .or. (abs(p - p0) < error)) then
            write (*, "('The root is P=',1x,f8.4)") p
            stop
        end if

        if (f(p) < 0) then
            a = p
            p0 = p
        else
            b = p
            p0 = p
        end if
    end do
    write (*, "('NO ROOT FOUND AFTER',1x,i3,1x,'ITERATIONS')") n
end program falseposition

real function f(x)
    implicit none
    real, intent(in) :: x

    f = 4*exp(-x)*sin(x) - 1
end function f
