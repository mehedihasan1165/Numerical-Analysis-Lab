!-----------------------------------------------------------------------------------------------------------------
! Program name :: Newton's Divided Difference Interpolate
!
! Program details :: Estimating unknown value of a function from known values of the
!                    function using Newton's divided difference table.
!
! Variables :: i = loop counter, j = loop counter, n = number of known values, x = interpolating point,
!              xi(:) = array of known x_i values, table(:) = array for computing difference table,
!              prodTerm = values of Newton basis/(product(i=0 to j-1) x-x_i),
!              ans = value of function in the interpolating point
!----------------------------------------------------------------------------------------------------------------------
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

program NewtonInterpolation
    implicit none
    integer :: i, j, n
    real :: ans = 0, x
    real, allocatable :: xi(:), table(:, :)
    real, external :: prodTerm
    
    ! open and reading data from file
    open (10, file='interpolate.txt')
    read (10, *) n, x
    allocate (xi(n), table(n, n))
    read (10, *) (xi(i), table(i, 1), i=1, n)
    close (10)

    ! loop for computing difference table 
    do i = 1, n
        do j = 1, n - i
            table(j, i + 1) = (table(j, i) - table(j + 1, i))/(xi(j) - xi(i + j))
        end do
    end do

    ! printing difference table
    write (*,'(1x,"Divided Difference Table",/)')
    do i = 1, n
        do j = 1, n - i + 1
            write (*, '(3x,f11.6)', advance='no') table(i, j)
        end do
        write (*, '(/)')
    end do

    ! loop for computing value at interpolating point
    do i = 1, n
        ans = ans + (table(1, i)*prodTerm(i, n, x, xi))
    end do

    write (*, '(1x, "The function value at x=",f9.4," is:",f11.6)') x, ans
end program NewtonInterpolation


! function for computing values of Newton basis/ (product(i=0 to j-1) x-x_i)
real function prodTerm(i, n, x, xi)
    implicit none
    integer i, j, n
    real :: x, xi(n)
    
    prodTerm = 1.0
    
    do j = 1, i - 1
        prodTerm = prodTerm*(x - (xi(j)))
    end do

end function prodTerm
