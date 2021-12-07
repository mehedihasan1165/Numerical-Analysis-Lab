! Simpson 3/8 Rule
! Number of interval must be of multiple of 3
! Formula=3h/8*[f(a)+f(b)+3[(f(x_1)+(f(x_2)+(f(x_4)+(f(x_5)+...]+2[(f(x_3)+(f(x_6)+...]]]
program simpson_three_eighth
    implicit none
    real :: a, b, h, tempsum, f, ans
    integer:: i, n
    write (*, '(A)') " Enter lower limit, upper limit and number of partition"
    read (*, *) a, b, n
    do while (mod(n, 3) .ne. 0)
        write (*, '(A)') " Number of partition must be a multiple of 3"
        write (*, '(A)') " Enter number of partition"
        read (*, *) n
    end do
    h = (b - a)/n
    tempsum = (f(a) + f(b))
    do i = 1, n - 1
        if (mod(i, 3) .eq. 0) then
            tempsum = tempsum + 2*f(a + i*h)
        else
            tempsum = tempsum + 2*f(a + i*h)
        end if
    end do
    ans = (3*h/8)*(tempsum)
    write (*, '(A,f9.4)') ' Value of the integration is: ', ans
end program simpson_three_eighth

real function f(x)
    implicit none
    real :: x
    f = log(x)
end function f
