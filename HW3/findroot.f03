! Library of numerical root-finding methods. Contains Bisection
! and Newton-Raphson algorithms.
!
MODULE findroot
  USE ieee_arithmetic
  IMPLICIT NONE

  ! Interface of a pure (no side effects) single-variable function
  ! y=f(x).
  !
  INTERFACE
     PURE FUNCTION f1d(x) RESULT(y)
       VALUE :: x
     END FUNCTION f1d
  END INTERFACE

CONTAINS

  ! Finds zero of a function within an interval using bisection.
  !
  ! Arguments:
  !   fn: pure single-variable callback function
  !   a: lower bound of the bracketing interval
  !   b: upper bound of the bracketing interval
  !   debug: Optional. Set to .TRUE. to print debug information.
  !
  ! Returns: A value of x within [a, b] for which fn(x) = 0 (up to
  !          round-off error).
  !
  FUNCTION findroot_bs(fn, a, b, debug) RESULT(x)
    PROCEDURE(f1d) :: fn
    LOGICAL, OPTIONAL :: debug
    LOGICAL :: verbose = .FALSE.
    REAL, VALUE :: a, b
    REAL x, d
    
    x = IEEE_VALUE(x, IEEE_QUIET_NAN) ! Default to NaN if unable to
                                      ! solve.

    IF (PRESENT(debug)) THEN
       verbose = debug
    END IF

    IF (fn(a) == 0) THEN
       x = a
       RETURN
    END IF

    IF (fn(b) == 0) THEN
       x = b
       RETURN
    END IF

    IF (a > b .or. fn(a) * fn(b) > 0) RETURN

    d = HUGE(d)

    DO
       x = (a + b) / 2

       IF (verbose) PRINT *, x, a, b
       
       IF (d == b - a) THEN
          RETURN
       END IF
       d = b - a
       IF (fn(x) == 0) RETURN

       IF (fn(x) * fn(b) < 0) THEN
          a = x
       ELSE
          b = x
       END IF
    END DO
  END FUNCTION findroot_bs

  ! Finds zero of a function within an interval using Newton-Raphson
  ! algorithm.
  !
  ! Arguments:
  !   fn: pure single-variable callback function; f(x)
  !   dfn: pure single-variable callback function; f'(x)
  !   a: lower bound of the bracketing interval
  !   b: upper bound of the bracketing interval
  !   debug: Optional. Set to .TRUE. to print debug information.
  !
  ! Returns: A value of x within [a, b] for which fn(x) = 0 (up to
  !          round-off error).
  !
  FUNCTION findroot_nr(fn, dfn, a, b, debug) RESULT(x)
    PROCEDURE(f1d) :: fn, dfn
    REAL, VALUE :: a, b
    LOGICAL, OPTIONAL :: debug
    LOGICAL :: verbose = .FALSE.
    REAL x, dx, w

    IF (fn(a) == 0) THEN
       x = a
       RETURN
    END IF

    IF (fn(b) == 0) THEN
       x = b
       RETURN
    END IF

    IF (a > b .OR. fn(a) * fn(b) > 0) THEN
       x = IEEE_VALUE(x, IEEE_QUIET_NAN)
       RETURN
    END IF

    IF (PRESENT(debug)) THEN
       verbose = debug
    END IF
    
    x = (a + b) / 2

    w = IEEE_VALUE(x, IEEE_QUIET_NAN)
    DO
       dx = fn(x) / dfn(x)
       x = x - dx
       
       IF (x < a .OR. x > b) THEN
          x = (a + b) / 2
          IF (verbose) WRITE(0, *) "# out of bracket: performing bisection"
       END IF
          
       IF (fn(x) * fn(b) < 0) THEN
          a = x
       ELSE   
          b = x
       END IF

       IF (verbose) PRINT *, x, a, b, dx

       IF (w == b - a) RETURN
       w = b - a
    END DO
  END FUNCTION findroot_nr
END MODULE findroot
