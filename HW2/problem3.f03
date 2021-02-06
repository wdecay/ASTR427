PROGRAM orbit_calculation
  USE ode
  IMPLICIT NONE

  REAL, DIMENSION(4) :: x = [1.0, 0.0, 0.0, 0.3]
  REAL, PARAMETER :: t_max = 100.0
  REAL :: t = 0.0, h, e
  CHARACTER(100) :: arg_str
  INTEGER :: method = -1

  IF (COMMAND_ARGUMENT_COUNT() == 2) THEN
     CALL GET_COMMAND_ARGUMENT(1, arg_str)
     IF (arg_str .EQ. '--rk') method = 0
     IF (arg_str .EQ. '--lf') method = 1

     CALL GET_COMMAND_ARGUMENT(2, arg_str)
     READ(arg_str, *) h
     IF (method == -1) ERROR STOP "Unknown integrator. Try --rk or --lf"
  ELSE
     ERROR STOP "Invalid parameters. Usage: --rk <step>, or --lf <step>"
  END IF

  DO
     e = 0.5 * (x(3)**2 + x(4)**2) - 1 / denom(x(1), x(2), 0.5)
     PRINT *, t, x, e
     IF (t == t_max) EXIT
     t = t + h
     IF (t > t_max) t = t_max

     SELECT CASE (method)
     CASE (0)
        x = rk4_step(f, t, x, h)
     CASE (1)
        CALL leapfrog_step(f_lf, g_lf, x(1:2), x(3:4), h)
     END SELECT     
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

  FUNCTION f_lf(v, n) RESULT(w)
    REAL, DIMENSION(:), INTENT(in) :: v
    INTEGER, INTENT(in) :: n
    REAL, DIMENSION(n) :: w

    w = v
  END FUNCTION f_lf

  FUNCTION g_lf(x, n) RESULT(w)
    REAL, DIMENSION(:), INTENT(in) :: x
    INTEGER, INTENT(in) :: n
    REAL, DIMENSION(n) :: w

    w = -x / denom(x(1), x(2), 1.5)
  END FUNCTION g_lf

END PROGRAM orbit_calculation
