MODULE ode
  IMPLICIT NONE

  INTERFACE
     FUNCTION fn(t, y) RESULT(yprime)
       REAL, INTENT(in) :: t
       REAL, DIMENSION(:), INTENT(in) :: y
       REAL, DIMENSION(SIZE(y, 1)) :: yprime
     END FUNCTION fn

     FUNCTION fnl(z, n) RESULT(w)
       IMPLICIT NONE
       REAL, DIMENSION(:), INTENT(in) :: z
       INTEGER, INTENT(in) :: n
       REAL, DIMENSION(n) :: w
     END FUNCTION fnl

  END INTERFACE

CONTAINS

  FUNCTION euler_step(f, t, y, h) RESULT(ynext)
    PROCEDURE(fn) :: f
    REAL, INTENT(in) :: t, h
    REAL, DIMENSION(:), INTENT(in) :: y
    REAL, DIMENSION(SIZE(y, 1)) :: ynext

    ynext = y + h * f(t, y)
  END FUNCTION euler_step
  
  SUBROUTINE leapfrog_step(f, g, x, v, h)
    PROCEDURE(fnl) :: f
    PROCEDURE(fnl) :: g
    REAL, DIMENSION(:) :: x, v
    REAL :: h

    v = v + h / 2 * g(x, SIZE(v, 1))
    x = x + h * f(v, SIZE(x, 1))
    v = v + h / 2 * g(x, SIZE(v, 1))
  END SUBROUTINE leapfrog_step
  
  FUNCTION rk4_step(f, t, y, h) RESULT(ynext)
    PROCEDURE(fn) :: f
    REAL, INTENT(in) :: t, h
    REAL, DIMENSION(:), INTENT(in) :: y
    REAL, DIMENSION(SIZE(y, 1)) :: ynext

    REAL, DIMENSION(SIZE(y, 1)) :: k1, k2, k3, k4

    k1 = h * f(t, y)
    k2 = h * f(t + h/2, y + k1/2)
    k3 = h * f(t + h/2, y + k2/2)
    k4 = h * f(t + h, y + k3)
    ynext = y + (k1 + 2 * k2 + 2 * k3 + k4) / 6
  END FUNCTION rk4_step
END MODULE ode
