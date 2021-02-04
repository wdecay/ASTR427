MODULE rk4
  IMPLICIT NONE
CONTAINS
  FUNCTION step(f, t, y) result(ynext)
    REAL, DIMENSION(:), INTENT(in) :: y
    REAL, DIMENSION(SIZE(y, 1)) :: ynext
    REAL :: t
    
    INTERFACE
       FUNCTION f(t, y) result(yprime)
         REAL, INTENT(in) :: t
         REAL, DIMENSION(:), INTENT(in) :: y
         REAL, DIMENSION(SIZE(y, 1)) :: yprime
       END FUNCTION f
    END INTERFACE

    ynext = f(t, y)
  END FUNCTION step
END MODULE rk4
