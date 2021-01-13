program newtonraphson
    !equation::x^3-3x+1
    implicit none
    real :: x, xold, error = 0.00001
    integer :: i, n
    real, external:: f, fprime
    
    print *, "Enter initial value(x nought) and number of iteration(n)"
    read *, xold,n
    
    do i = 0, n
        x = xold - (f(xold)/fprime(xold))
        write (*, "(1x,i3,2x,f10.6)") i, x
        if (f(x) == 0.0 .or. abs(x - xold) < error) then
            print *, "Root is ", x
            stop
        end if
        xold = x
    end do
    
    write (*, "('NO ROOT FOUND AFTER',1x,i3,1x,'ITERATIONS')") n

end program newtonraphson

real function f(x)
    implicit none
    real, intent(in) ::x

    f = x**3 - 3*x + 1

end function f

real function fprime(x)
    implicit none
    real, intent(in):: x

    fprime = 3*x**2 - 3

end function fprime
