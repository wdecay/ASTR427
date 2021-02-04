PROGRAM test
  USE ode
  IMPLICIT NONE
  CHARACTER(100) :: h_str
  REAL :: h

  CALL GET_COMMAND_ARGUMENT(1, h_str) 
  READ(h_str, *) h

  CALL integrate_benchmark(h)
  STOP

CONTAINS
  SUBROUTINE integrate_benchmark(h)
    REAL, INTENT(in) :: h

    REAL, DIMENSION(2) :: x_euler, x_rk
    REAL, DIMENSION(1) :: x_lf, v_lf
    REAL :: t


    x_euler = [1, 0]

    x_lf = [1]
    v_lf = [0]
    
    x_rk = x_euler
    
    t = 0
    DO
       PRINT *, t, x_euler, x_lf, v_lf,  x_rk
       t = t + h
       IF (t > 30) EXIT
       x_euler = euler_step(f, t, x_euler, h)
       CALL leapfrog_step(f_lf, g_lf, x_lf, v_lf, h)
       x_rk = rk4_step(f, t, x_rk, h)
    END DO

  END SUBROUTINE integrate_benchmark
  
  FUNCTION f(t, y) result(yprime)
    REAL, INTENT(in) :: t
    REAL, DIMENSION(:), INTENT(in) :: y
    REAL, DIMENSION(SIZE(y, 1)) :: yprime

    yprime = [y(2), -y(1)]
  END FUNCTION f

  FUNCTION f_lf(v, n) RESULT(w)
    REAL, DIMENSION(:), INTENT(in) :: v
    REAL, DIMENSION(n) :: w
    INTEGER :: n

    w(1) = v(1)
  END FUNCTION f_lf

  FUNCTION g_lf(x, n) RESULT(w)
    REAL, DIMENSION(:), INTENT(in) :: x
    REAL, DIMENSION(n) :: w
    INTEGER :: n

    w(1) = -x(1)
  END FUNCTION g_lf
  
END PROGRAM test
