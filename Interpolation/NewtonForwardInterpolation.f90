!-----------------------------------------------------------------------------------------------------------------
! Program name :: Newton's Forward Difference Interpolate
!
! Program details :: Estimating unknown value of a function from known values of the
!                    function using Newton's forward difference table.
!
! Variables :: i = loop counter, j = loop counter, n = number of known values, x = interpolating point,
!              xi(:) = array of known x_i values, table(:,:) = array for computing difference table,
!              ans = value of function in the interpolating point
!----------------------------------------------------------------------------------------------------------------------
! Functions :: factorial = function to calulate the factorial of an integer,
!              uCalculation = calculate 'u(u-1)(u-2)...' terms to apply in the final formula
!
!----------------------------------------------------------------------------------------------------------------------
! Sample data points (interpolate.txt)
!
! -----------------
! | n   x         |
! | x_1  f(x_1)   |
! | x_2  f(x_2)   |
! | ...  ...      |
! | x_n  f(x_n)   |
! -----------------
!
!----------------------------------------------------------------------------------------------------------------------

program newtonForward
    implicit none
    integer :: n, i, j
    real :: x, u, ans = 0
    real, allocatable :: xi(:), table(:, :)
    integer, external :: factorial
    real, external :: uCalculation

    ! open and reading data from txt file
    open (10, file='points.txt')
    read (10, *) n, x
    allocate (xi(n), table(n, n))
    read (10, *) (xi(i), table(i, 1), i=1, n)
    close (10)

    ! calculate the forward difference table
    do i = 2, n
        do j = 1, n - i + 1
            table(j, i) = table(j + 1, i - 1) - table(j, i - 1)
        end do
    end do

    ! printing the forward difference table
    do i = 1, n
        do j = 1, n - i + 1
            write (*, '(3x,f14.8)', advance='no') table(i, j)
        end do
        write (*, '(/)')
    end do

    ! calculating u for calculation given that the data
    ! are sorted and are in equal intervals
    u = (x - xi(1))/(xi(2) - xi(1))

    ! loop for final calculation
    do i = 1, n
        ans = ans + ((uCalculation(u, i)/factorial(i - 1))*table(1, i))
    end do

    ! print the value of x
    write (*, '(1x, "The function value at x=",f9.4," is:",f14.8)') x, ans
end program newtonForward

! function for calulating factorial
integer function factorial(n)
    implicit none
    integer, intent(in) :: n
    integer :: i, ans
    ans = 1
    if (n < 1) then
        factorial = ans
    else
        do i = 1, n
            ans = ans*i
        end do
        factorial = ans
    end if
end function factorial

! function for calulating 'u(u-1)...' terms in final formula
real function uCalculation(u, n)
    implicit none
    real  :: u, ans
    integer :: i, n
    if (n == 1) then
        uCalculation = 1
    else
        ans = u
        do i = 1, n - 2
            ans = ans*(u - i)
        end do
        uCalculation = ans
    end if
end function
