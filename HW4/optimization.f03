MODULE optimization
  USE ieee_arithmetic
  IMPLICIT NONE

  REAL, PARAMETER :: r = (3.0 - SQRT(5.0))/2.0
  REAL, PARAMETER :: tol = SQRT(EPSILON(0.0))

  INTERFACE
     PURE FUNCTION f1d(x) RESULT(y)
       VALUE :: x
     END FUNCTION f1d
  END INTERFACE

CONTAINS
  FUNCTION gss(fn, a, c) RESULT(x)
    PROCEDURE(f1d) :: fn
    REAL, VALUE :: a, c
    REAL :: x, b, fa, fb, fx, fc

    x = IEEE_VALUE(x, IEEE_QUIET_NAN) ! Default to NaN if unable to
                                      ! solve.

    b = a + r * (c - a)

    fa = fn(a)
    fb = fn(b)
    fc = fn(c)
    x = b + r * (c - b)
    fx = fn(x)

    IF (.NOT. (fx > fb .AND. fa > fb .OR. fb > fx .AND. fc > fx)) RETURN

    DO
       IF (ABS(c - a) <= tol * (ABS(x) + ABS(b))) THEN
          IF (fx > fb) x = b ! otherwise x is already set
          EXIT
       END IF
       
       IF (fx > fb) THEN ! new bracket (a, b, x), ab is the larger
                         ! interval
          c = x
          fc = fx

          x = b
          fx = fb
          
          b = a + r * (c - a)
          fb = fn(b)
       ELSE ! new bracket (b, x, c), xc is the larger interval
          a = b
          fa = fb

          b = x
          fb = fx

          x = b + r * (c - b)
          fx = fn(x)
       END IF

       ! PRINT *, (b - a)/(c - a), (x - b)/(x - a)
    END DO
  END FUNCTION gss
END MODULE optimization
