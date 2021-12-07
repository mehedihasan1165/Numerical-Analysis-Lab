! Trapezodial Rule
! Formula=h/2*[f(a)+f(b)]+h*[sum_{i=1}^{n-1} f(x_i)]
program trapezoidal
    implicit none
    real :: a, b, h, tempsum, f, ans
    integer:: i, n
    write (*, '(A)') " Enter lower limit, upper limit and number of partition"
    read (*, *) a, b, n
    h = (b - a)/n
    tempsum = 0.5*(f(a) + f(b))
    do i = 1, n - 1
        tempsum = tempsum + f(a + i*h)
    end do
    ans = h*(tempsum)
    write (*, '(A,f9.4)') ' Value of the integration is: ', ans
end program trapezoidal

real function f(x)
    implicit none
    real::x
    f = 1/exp(x**2)
end function f
