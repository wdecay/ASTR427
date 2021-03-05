PROGRAM rot_curve
  USE optimization
  USE dataio

  IMPLICIT NONE

  REAL, dimension(:, :), allocatable, TARGET :: tbl
  REAL :: x(2), rhobeg=0.2, rhoend=1.0e-8
  INTEGER :: n = 2, w(200), npt=5, iprint=0, maxfun=5000
  
  CALL load_data('rot.csv', tbl)

  x = [1, 1]
  CALL newuoa(fn, n, npt, x, rhobeg, rhoend, iprint, maxfun, w)

  PRINT *, x
CONTAINS

  SUBROUTINE fn(n, x, f)
    INTEGER n
    REAL x(n), f
    f = SUM((tbl(:, 2) - x(1)*(1 - EXP(-tbl(:, 1)/x(2))))**2)
  END SUBROUTINE fn

END PROGRAM rot_curve
