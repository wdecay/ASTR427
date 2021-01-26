! Problem 2: Roundoff error
!
PROGRAM roundoff_error
  IMPLICIT NONE

  INTEGER :: i
  REAL(kind=8) :: x, seed = 2d-7

  WRITE(*, "(A15,A15)") "X", "Y"
  DO i=1,20
     x = seed / i
     WRITE(*, "(E15.10,F15.10)") x, expr(x)
  END DO

  STOP
  
CONTAINS 

  ! Returns the value of (1-cos(x))/x**2 in double precision
  !
  FUNCTION expr(x)
    REAL(kind=8) :: expr, x
    expr = (1 - COS(x)) / x**2
  END FUNCTION expr
  
END PROGRAM roundoff_error
