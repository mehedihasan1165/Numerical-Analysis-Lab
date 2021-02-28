!---------------------------------------------------------------------------------------------------------------------
! Program name :: Secant Method
! Program details :: Solving algebraic equation using secant method
!                    Here we solve x^3-2x-5=0 with Newton Raphson method
!
! Variables :: x0 = left interval/(i-1)th assumed root of equation, x1 = right interval/ ith assumed root of equation,
!              f = function, root = i+1 th assumed root of equation, tol = error tolerance, i = loop counter,
!              n = number of iterations
!----------------------------------------------------------------------------------------------------------------------

program secantMethod
    implicit none
    real :: x0, x1, root
    real, external :: f
    real, parameter :: tol = 0.0001
    integer :: i, n

    ! taking left and right interval and number of iterations
    print *, "Enter initial values (x_0 and x_1) and number of iterations:"
    read *, x0, x1, n

    ! checking if any root exists in given interval
    do while (f(x0)*f(x1) > 0)
        print *, "No root exists between previous initial values. Try again"
        print *, "Enter initial values (x_0 and x_1):"
        read *, x0, x1
    end do

    ! printing header for columns
    write (*, "(2x,'N',10x,'x')")

    ! main loop
    do i = 2, n
        ! evaluating assumed root
        root = ((x0*f(x1)) - (x1*f(x0)))/(f(x1) - f(x0))
        write (*, "(1x,i3,2x,f11.6)") i, root
        
		! checking end condition either f(x) is zero or error between two successive values are sufficiently small
        if (f(root) == 0 .or. abs(root - x1) < tol) then
            write (*, "(1x,'Root is',1x,f11.5,1x,'(correct to 4 decimal places)')") root
            stop
        end if

        ! changing interval size
        x0 = x1
        x1 = root
    end do

    ! no root found in given iteration
    write (*, "('NO ROOT FOUND AFTER',1x,i3,1x,'ITERATIONS')") n

end program secantMethod

! function for returning values of f(x)=x^3-2x-5
real function f(x)
    implicit none
    real, intent(in) :: x

    f = x**3 - (2*x) - 5
end function f
