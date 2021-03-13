PROGRAM diffusion1d
  IMPLICIT NONE

  CALL solve_and_plot(dt = 0.01, output="output/dt_0.010.txt")
  CALL solve_and_plot(dt = 0.002, output="output/dt_0.002.txt")

CONTAINS

  SUBROUTINE solve_and_plot(dt, output)
    INTEGER, PARAMETER :: n = 10
    REAL, PARAMETER :: xmin = 0.0, xmax = 1.0, tmax = 0.4

    REAL, INTENT(in) :: dt
    CHARACTER(*), INTENT(in) :: output
    
    REAL :: u(0:n)
    REAL :: dx, dt_over_dx2
    INTEGER :: niter, i, status
    CHARACTER(200) :: gnuplot_cmd

    niter = INT(tmax / dt)
    dx = (xmax - xmin) / n
    dt_over_dx2 = dt/dx**2

    u = 0
    ! boundary conditions
    u(0) = 100.0
    u(n) = 100.0

    OPEN (99, FILE=output, ACTION='write')

    WRITE(99, '(*(ES14.5))') u
    DO i = 1, niter
       CALL evolve(u, dt_over_dx2)
       WRITE(99, '(*(ES14.5))') u
    END DO

    CLOSE(99)

    WRITE(gnuplot_cmd, '(A,A,A,F5.3,A)') 'gnuplot -e "data=''',&
         & output, ''';dt=', dt,'" ./gnuplot/plot.plt'
    PRINT '(2A)', '> ', TRIM(gnuplot_cmd)
    CALL EXECUTE_COMMAND_LINE(gnuplot_cmd, CMDSTAT=status, WAIT=.true.)
    IF (status == 0) THEN
       PRINT '(A)', "  [OK]"
    ELSE
       PRINT '(A)', "  [FAIL]"
    END IF
  END SUBROUTINE solve_and_plot

  SUBROUTINE evolve(u, dt_over_dx2)
    REAL :: u(0:), dt_over_dx2, uprev, unew
    INTEGER :: i

    uprev = u(0)
    DO i = 1, SIZE(u) - 2
       unew = u(i) + dt_over_dx2 * (u(i + 1) - 2*u(i) + uprev)
       uprev = u(i)
       u(i) = unew
    END DO  
  END SUBROUTINE evolve

END PROGRAM diffusion1d
