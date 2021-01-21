program secantmethod
    !x^3-2x-5=0
    implicit none
    real :: x0, x1, root
    real, external :: f
    real, parameter :: tol = 0.0001
    integer :: i, n

    print *, "Enter initial values (x_0 and x_1) and number of iterations:"
    read *, x0, x1, n
    
    do while (f(x0)*f(x1) > 0)
        print *, "No root containts between privious inital values"
        print *, "Enter initial values (x_0 and x_1):"
        read *, x0, x1
    end do
    
    write (*, "(2x,'N',10x,'x')")
    do i = 2, n
        root = ((x0*f(x1)) - (x1*f(x0)))/(f(x1) - f(x0))
        write (*, "(1x,i3,2x,f11.6)") i, root
        if (f(root) == 0 .or. abs(root - x1) < tol) then
            write (*, "(1x,'Root is',1x,f11.5,1x,'(correct to 4 decimal places)')") root
            stop
        end if
        x0 = x1
        x1 = root
    end do
    write (*, "('NO ROOT FOUND AFTER',1x,i3,1x,'ITERATIONS')") n
end program secantmethod
    
real function f(x)
    implicit none
    real, intent(in) :: x
    
    f = x**3 - (2*x) - 5
end function f
    