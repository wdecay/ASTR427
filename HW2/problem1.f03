PROGRAM test
  USE rk4
  REAL, DIMENSION(5) :: y
  PRINT *, step(f, 1.0, y)
  STOP

CONTAINS
  FUNCTION f(t, y) result(yprime)
    REAL, INTENT(in) :: t
    REAL, DIMENSION(:), INTENT(in) :: y
    REAL, DIMENSION(SIZE(y, 1)) :: yprime

  END FUNCTION f
END PROGRAM test
