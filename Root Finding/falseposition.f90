!------------------------------------------------------------------------------------------------------------------------
! Program name :: Method of false position/regula falsi method
! Program details :: Solving algebraic equation using method of false position which is also known as regula falsi method
!                    Here we solve 4e^(-x)*sin(x) - 1 = 0 with of false position
!
! Variables :: a = left interval, b = right interval, error = error tolerance, i = loop counter, n = number of iterations
!              p0 = (i-1)th assumed root of equation, p = i th assumed root of equation, f = function
!------------------------------------------------------------------------------------------------------------------------
program falsePosition
    implicit none
    real a, b, p0, p
    real, external :: f
    real, parameter :: error = 0.0001
    integer i, n

    ! taking left and right interval and number of iterations
    print *, "Enter a (start interval) and b (end interval) and n (iterations)"
    read *, a, b, n

    ! checking if any root exists in given interval
    do while (f(a)*f(b) > 0)
        print *, "No root exists  in the given interval. Try again"
        print *, "Enter a (start interval) and b (end interval)"
        read *, a, b
    end do

    ! printing header for columns
    write (*, "(2x,'N',11x,'P',10x,'f(P)')")

    ! main loop
    do i = 1, n
      
		! evaluating assumed root for first time  
        if (i == 1) then
            p0 = (a*f(b) - b*f(a))/(f(b) - f(a))
            write (*, "(1x,i3,3x,1x,f9.5,3x,1x,f9.5)") i, p0, f(p0)
            
			! changing interval size
            if (f(p0) < 0) then
                a = p0
            else
                b = p0
            end if
            continue
        end if
		
		! evaluating assumed root
        p = (a*f(b) - b*f(a))/(f(b) - f(a))
        write (*, "(1x,i3,3x,1x,f9.5,3x,1x,f9.5)") i, p, f(p)
		
		! checking end condition either f(x) is zero or error between two successive values are sufficiently small
        if (f(p) == 0.0 .or. (abs(p - p0) < error)) then
            write (*, "('The root is P =',1x,f8.4)") p
            stop
        end if

		! changing interval size
        if (f(p) < 0) then
            a = p
            p0 = p
        else
            b = p
            p0 = p
        end if
    end do
    
	! no root found in given iteration
	write (*, "('NO ROOT FOUND AFTER',1x,i3,1x,'ITERATIONS')") n
    
end program falsePosition

! function for returning values of f(x)=4e^(-x)*sin(x) - 1 = 0
real function f(x)
    implicit none
    real, intent(in) :: x

    f = 4*exp(-x)*sin(x) - 1
end function f
