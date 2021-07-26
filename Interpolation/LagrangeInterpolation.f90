!-----------------------------------------------------------------------------------------------------------------
! Program name :: Lagrange Interpolate
!
! Program details :: Estimating unknown value of a function from known values of the
!                    function using Lagrange interpolation.
!
! Variables :: i = loop counter, j = loop counter, n = number of known values, x = interpolating point,
!              xi(:) = array of known x_i values, fxi(:) = array of known f(x_i) values,
!              lagrange_basis = values of Lagrange basis/ l_k(x), ans = value of function in the interpolating point
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

program lagrange_interpolation
    implicit none
    integer :: i, j, n
    real :: ans = 0, x, lagrange_basis = 1
    real, allocatable :: xi(:), fxi(:)

    ! open and reading data from file
    open (10, file="interpolate.txt")
    read (10, *) n, x
    allocate (xi(n), fxi(n))
    read (10, *) (xi(i), fxi(i), i=1, n)

    ! main loop
    do i = 1, n
        lagrange_basis = 1
		
	! computing lagrange basis or l_k(x)
        do j = 1, n
            if (j /= i) then
                lagrange_basis = lagrange_basis*(x - xi(j))/(xi(i) - xi(j))
            end if
        end do
        
	! building value term by term
        ans = ans + (lagrange_basis*fxi(i))
    end do
    
	write (*, '(1x, "The function value at x=",f7.4," is:",f11.6)') x, ans

end program lagrange_interpolation
