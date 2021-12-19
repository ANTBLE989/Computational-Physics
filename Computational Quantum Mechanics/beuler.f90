program beuler
    implicit none
    integer :: i, j, N 
    real :: x, y, y0, dx 

    ! set x-grid size and spacing 
    dx = 0.01 
    N = (1. - 0.)/dx + 1

    ! set initial conditions 
    x =0. ; y = 0.

    ! write initial conditions 
    open(10, file='results.txt')
    write(10, '(f8.2, f8.4)') x,y 

    do i=1,N-1
        y0 = y ! estimate the root 

        ! iterate the Newton-Raphson method 
        do j=1,5
            y0 = y0 - (y-y0+dx*exp(-y0))/(-1.-dx*exp(-y0))
        end do 
        
        y=y0    !increment y 
        x=x+dx  !increment x
        
        write(10,'(f8.2,f8.4)')x,y 
    end do

    close(10)
end program beuler

        




