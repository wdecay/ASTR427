! Library of numerical optimization methods. Contains Golden Section
! Search algorithm implementation.
!
MODULE optimization
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

  ! Minimizes the given function on the interval [a, c] using the
  ! Golden Section Search algorithm.
  !
  ! Arguments:
  !  fn: pure single-variable callback function
  !  a: lower bound of the search interval
  !  c: upper bound of the search interval
  !
  ! Returns: The abscissa within [a, c] which minimizes fn (accurate
  ! up to round-off/tolerance). The caller is expected to choose the
  ! initial bracket such that the minimum is contained within it.  The
  ! algorithm will converge close to either a or c if the function
  ! grows monotonically or is a constant.
  !
  FUNCTION gss(fn, a, c) RESULT(x)
    ! constants
    REAL, PARAMETER :: r = (3.0 - SQRT(5.0))/2.0
    REAL, PARAMETER :: tol = SQRT(EPSILON(0.0)) ! the square root of
                                                ! the machine epsilon

    PROCEDURE(f1d) :: fn
    REAL, VALUE :: a, c
    REAL :: x, b, fb, fx

    b = a + r * (c - a)
    x = b + r * (c - b)

    ! Note: fn(a) and fn(b) are never evaluated.
    fb = fn(b)
    fx = fn(x)

    DO
       ! "it is hopeless to ask for a bracketing interval of width
       ! less than `tol` times its central value" Press, et al.
       ! The stopping criterion is the same as in Numberical Recipes,
       ! however their tolerance was greater by a factor of 2.
       IF (ABS(c - a) <= tol * (ABS(x) + ABS(b))) THEN
          IF (fx > fb) x = b ! otherwise x is already set
          EXIT
       END IF
       
       IF (fx > fb) THEN ! new bracket (a, b, x), ab is the larger
                         ! interval
          c = x
          x = b
          fx = fb
          
          b = a + r * (c - a)
          fb = fn(b)
       ELSE ! new bracket (b, x, c), xc is the larger interval
          a = b
          b = x
          fb = fx

          x = b + r * (c - b)
          fx = fn(x)
       END IF
       ! at this point the golden ratio invariant should always hold
       ! which can be confirmed by uncommenting the statement below
       !
       ! PRINT *, (b - a)/(c - a), (x - b)/(x - a)
    END DO
  END FUNCTION gss
END MODULE optimization
