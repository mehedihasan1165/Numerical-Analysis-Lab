!-----------------------------------------------------------------------------------------------------------------
! Program name :: Newton Raphson Method
! Program details :: Solving algebraic equation using Newton Raphson method
!                    Here we solve x^3-3x+1=0 with Newton Raphson method
!
! Variables :: x = initial value, x = assumed root of equation, f = function, fprime = derivative of function
!              error = error tolerance, i = loop counter, n = number of iterations
!-----------------------------------------------------------------------------------------------------------------
program newtonRaphson
    implicit none
    real :: x, xold, error = 0.0001
    integer :: i, n
    real, external:: f, fprime
    
	! reading initial value and number of iterations
    print *, "Enter initial value(x nought) and number of iteration(n)"
    read *, xold,n

    ! printing header for columns
    write (*, "(2x,'N',8x,'x')")

	! main loop
    do i = 0, n
      
      	! evaluating assumed root
        x = xold - (f(xold)/fprime(xold))
        write (*, "(1x,i3,2x,f10.6)") i, x

        ! checking end condition either f(x) is zero or error between two successive values are sufficiently small
        if (f(x) == 0.0 .or. abs(x - xold) < error) then
            write(*,"(1x,'Root is',1x,f10.5,1x,'(correct to 4 decimal places)')") x
            stop
        end if

        ! setting previous root as the current root
        xold = x
    end do
    
    ! no root found in given iteration   
    write (*, "('NO ROOT FOUND AFTER',1x,i3,1x,'ITERATIONS')") n

end program newtonRaphson

! function for returning values of f(x)=x^3-3x+1
real function f(x)
    implicit none
    real, intent(in) ::x

    f = x**3 - 3*x + 1

end function f

! function for returning values of f'(x)=3x^2-3
real function fprime(x)
    implicit none
    real, intent(in):: x

    fprime = 3*x**2 - 3

end function fprime
