! Problem 3. Orbit calculation.
!
PROGRAM orbit_calculation
  USE ode
  IMPLICIT NONE

  REAL, DIMENSION(4) :: x = [1.0, 0.0, 0.0, 0.3] ! initial values
  REAL, PARAMETER :: t_max = 100.0
  REAL :: t = 0.0, h, e
  CHARACTER(100) :: arg_str
  INTEGER :: method = -1

  IF (COMMAND_ARGUMENT_COUNT() == 2) THEN
     ! Two command-line arguments are expected. The first one should
     ! be either `--rk` or `--lf` for, respectively, leapfrog and
     ! Runge-Kutta. The second argument is the step size.
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
     ! Prints after every step. Generally should be throttled, but
     ! wasn't a problem for this assignment.
     PRINT *, t, x, e
     IF (t == t_max) EXIT
     t = t + h
     IF (t > t_max) t = t_max ! make sure t_max is hit

     SELECT CASE (method)
     CASE (0) ! Runge-Kutta
        x = rk4_step(rk_f, t, x, h)
     CASE (1) ! Leapfrog
        CALL leapfrog_step(lf_f, lf_g, x(1:2), x(3:4), h)
     END SELECT     
  END DO

  STOP

CONTAINS

  ! Computes a frequently-occurring denominator expression.
  !
  FUNCTION denom(x, y, p) RESULT(v)
    REAL, INTENT(in) :: x, y, p
    REAL :: v

    v = (1 + x**2 + y**2)**p
  END FUNCTION denom

  ! Euler/Runge-Kutta derivatives generating function.
  ! 
  FUNCTION rk_f(t, x) RESULT(dd)
    REAL, INTENT(in) :: t
    REAL, DIMENSION(:), INTENT(in) :: x
    REAL, DIMENSION(SIZE(x, 1)) :: dd
    REAL :: d

    d = denom(x(1), x(2), 1.5)
    dd = [x(3), x(4), -x(1)/d, -x(2)/d]
  END FUNCTION rk_f

  ! Leapfrog `f(v)` function.
  !
  FUNCTION lf_f(v, n) RESULT(dd)
    REAL, DIMENSION(:), INTENT(in) :: v
    INTEGER, INTENT(in) :: n
    REAL, DIMENSION(n) :: dd

    dd = v
  END FUNCTION lf_f

  ! Leapfrog `g(x)` function.
  !
  FUNCTION lf_g(x, n) RESULT(dd)
    REAL, DIMENSION(:), INTENT(in) :: x
    INTEGER, INTENT(in) :: n
    REAL, DIMENSION(n) :: dd

    dd = -x / denom(x(1), x(2), 1.5)
  END FUNCTION lf_g

END PROGRAM orbit_calculation
