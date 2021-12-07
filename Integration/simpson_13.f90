! Simpson 1/3 rule for integration
! Number of partition must be an even number
! Formula=h/3*[f(a)+f(b)+4[(f(x_1)+(f(x_3)+(f(x_5)+...]+2[(f(x_2)+(f(x_4)+...]]]
program simpson_one_third
    implicit none
    real :: a, b, h, tempsum, f, ans
    integer:: i, n
    write (*, '(A)') " Enter lower limit, upper limit and number of partition"
    read (*, *) a, b, n
    do while (mod(n, 2) .ne. 0)
        write (*, '(A)') " Number of partition must be an even number"
        write (*, '(A)') " Enter number of partition"
        read (*, *) n
    end do
    h = (b - a)/n
    tempsum = (f(a) + f(b))
    do i = 1, n - 1
        if (mod(i, 2) .eq. 0) then
            tempsum = tempsum + 2*f(a + i*h)
        else
            tempsum = tempsum + 4*f(a + i*h)
        end if
    end do
    ans = h/3*(tempsum)
    write (*, '(A,f9.4)') ' Value of the integration is: ', ans
end program simpson_one_third

real function f(x)
    implicit none
    real :: x
    f = log(x)
end function f
