!-----------------------------------------------------------------------------------------------------------------
! Program name :: Bisection Method
! Program details :: Solving algebraic equation using bisection method
!                    Here we solve x^3+x^2+x+7=0 with bisection method
!
! Variables :: a = left interval, b = right interval, p = assumed root of equation, f = function,
!              error = error tolerance, i = loop counter, n = number of iterations
!-----------------------------------------------------------------------------------------------------------------
program bisectionMethod
    implicit none
    real a, b, p
    real, external :: f
    real, parameter :: error = 0.0001
    integer i, n

    ! taking left and right interval and number of iterations
    print *, "Enter a (start interval) and b (end interval) and n (iterations)"
    read *, a, b, n

    ! checking if any root exists in given interval
    do while (f(a)*f(b) > 0)
        print *, "No root exists in the given interval. Try again"
        print *, "Enter a (start interval) and b (end interval)"
        read *, a, b
    end do

    ! printing header for columns
    write (*, "(2x,'N',11x,'P',10x,'f(P)')")

    ! main loop
    do i = 1, n

        ! setting p as middle of the interval
        p = (a + b)/2.0

        write (*, "(1x,i3,3x,1x,f9.5,3x,1x,f9.5)") i, p, f(p)

        ! checking end condition either f(x) is zero or error between two successive values are sufficiently small
        if (f(p) == 0.0 .or. (abs((b - a)/2.0)) < error) then
            write (*, "(1x,'Root is',1x,f11.5,1x,'(correct to 4 decimal places)')") p
            stop
        end if

        ! changing interval size
        if (f(p) > 0) then
            a = p
        else
            b = p
        end if
    end do

    ! no root found in given iteration
    write (*, "('NO ROOT FOUND AFTER',1x,i3,1x,'ITERATIONS')") n

end program bisectionMethod

! function for returning values of f(x)=x^3+x^2+x+7
real function f(x)
    implicit none
    real, intent(in) :: x

    f = x**3 + x**2 + x + 7

end function f
