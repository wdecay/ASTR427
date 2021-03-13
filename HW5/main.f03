PROGRAM diffusion1d
  IMPLICIT NONE

  CALL tabulate_and_plot(dt = 0.01)
  PRINT *, NEW_LINE('')
  CALL tabulate_and_plot(dt = 0.002)

CONTAINS

  SUBROUTINE tabulate_and_plot(dt)
    INTEGER, PARAMETER :: n = 10
    REAL, PARAMETER :: xmin = 0.0, xmax = 1.0, tmax = 0.4

    REAL, INTENT(in) :: dt
    
    REAL :: u(0:n), dx, dt_over_dx2
    INTEGER :: niter, i, status
    CHARACTER(100) :: output
    CHARACTER(200) :: gnuplot_cmd

    niter = NINT(tmax / dt)
    dx = (xmax - xmin) / n
    dt_over_dx2 = dt/dx**2

    u = 0
    ! boundary conditions
    u(0) = 100.0
    u(n) = 100.0

    PRINT '(A, F6.4, A, F4.2)', 'dt=', dt, ', dt/dx**2=', dt_over_dx2
    IF (dt_over_dx2 >= 0.5) PRINT '(A)', &
         &'  [WARNING] stability criterion violated (dt/dx**2>=0.5)'

    WRITE(output, '(A,F5.3,A)') 'output/dt_', dt, '.txt'

    OPEN(99, FILE=TRIM(output), ACTION='write')

    WRITE(99, '(*(ES14.5))') u
    DO i = 1, niter
       CALL evolve(u, dt_over_dx2)
       WRITE(99, '(*(ES14.5))') u
    END DO

    CLOSE(99)

    WRITE(gnuplot_cmd, '(A,A,A,F5.3,A)') 'gnuplot -e "data=''',&
         & TRIM(output), ''';dt=', dt,'" ./gnuplot/plot.plt'
    PRINT '(2A)', '> ', TRIM(gnuplot_cmd)
    CALL EXECUTE_COMMAND_LINE(gnuplot_cmd, CMDSTAT=status, WAIT=.true.)
    PRINT '(A)', MERGE("  [OK]  ", "  [FAIL]", status==0)
  END SUBROUTINE tabulate_and_plot

  SUBROUTINE evolve(u, dt_over_dx2)
    REAL :: u(0:), dt_over_dx2, uprev, unew
    INTEGER :: i

    uprev = u(0)
    DO i = 1, SIZE(u) - 2
       unew = u(i) + dt_over_dx2 * (u(i + 1) - 2 * u(i) + uprev)
       uprev = u(i)
       u(i) = unew
    END DO
  END SUBROUTINE evolve

END PROGRAM diffusion1d
