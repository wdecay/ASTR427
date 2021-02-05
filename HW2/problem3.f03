PROGRAM orbit_calculation
  USE ode
  IMPLICIT NONE

  REAL, DIMENSION(4) :: x = [1.0, 0.0, 0.0, 0.3]
  REAL, PARAMETER :: t_max = 100.0
  REAL :: t = 0.0, h, e
  CHARACTER(100) :: h_str

  IF (COMMAND_ARGUMENT_COUNT() == 1) THEN
     CALL GET_COMMAND_ARGUMENT(1, h_str) 
     READ(h_str, *) h
  ELSE
     ERROR STOP "No step provided."
  END IF

  DO
     e = 0.5 * (x(3)**2 + x(4)**2) - 1 / denom(x(1), x(2), 0.5)
     PRINT *, t, x, e
     IF (t == t_max) EXIT
     t = t + h
     IF (t > t_max) t = t_max

     ! CALL leapfrog_step(f_lf, g_lf, x_lf, v_lf, h)
     x = rk4_step(f, t, x, h)
  END DO

  STOP

CONTAINS

  FUNCTION denom(x, y, p) RESULT(v)
    REAL, INTENT(in) :: x, y, p
    REAL :: v

    v = (1 + x**2 + y**2)**p
  END FUNCTION denom

  FUNCTION f(t, y) RESULT(yprime)
    REAL, INTENT(in) :: t
    REAL, DIMENSION(:), INTENT(in) :: y
    REAL, DIMENSION(SIZE(y, 1)) :: yprime
    REAL :: d

    d = denom(y(1), y(2), 1.5)
    yprime = [y(3), y(4), -y(1)/d, -y(2)/d]
  END FUNCTION f

END PROGRAM orbit_calculation
