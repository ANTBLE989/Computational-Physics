program RK4
    implicit none 

    integer :: i, N
    real :: xmin, xmax, dx, k1, k2, k3, k4 
    real, allocatable :: x(:), y(:)

    ! set the x-grid size and spacing 
    dx = 0.01; xmin=0; xmax = 1.
    N = (xmax-xmin)/dx + 1 

    ! allocate the x and y arrays 
    allocate(x(1:N),y(1:N))
    x = [(xmin + dx*i, i=0,N-1)]
    y = 0. 
    y(1) = 2.

    ! the RK4 4-step method 
    do i=1, N-1
        k1 = f(y(i),x(i))
        k2 = f(y(i)+0.5*k1*dx,x(i)+0.5*dx)
        k3 = f(y(i)+k3*dx,x(i)+dx)

        y(i+1) = y(i) + dx*(k1 + 2*k2 + 3*k3 + k4)/6. 

    end do 

    contains 
        function f(y,x)
            real, intent(in) :: x, y
            real :: f 
            f = x*y 
        end function 
end program RK4
