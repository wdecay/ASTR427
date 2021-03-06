PROGRAM rot_curve
  USE optimization
  USE dataio

  IMPLICIT NONE

  REAL, PARAMETER :: vinf = 100
  REAL, dimension(:, :), allocatable, TARGET :: tbl
  REAL :: x(2), rhobeg=0.2, rhoend=1.0e-8, xmin
  INTEGER :: n = 2, w(200), npt=5, iprint=0, maxfun=5000, n_task = 0, i, j, ios
  CHARACTER(100) :: arg_str

  IF (COMMAND_ARGUMENT_COUNT() /= 1) CALL print_usage_and_stop
  
  CALL GET_COMMAND_ARGUMENT(1, arg_str)
  READ(arg_str, *, iostat=ios) n_task

  IF (n_task < 1 .OR. n_task > 3) CALL print_usage_and_stop

  CALL load_data('rot.csv', tbl)
  
  SELECT CASE (n_task)
  CASE (1)
     xmin = gss(fn1d, 1.0, 5.0)
     PRINT *, vinf, xmin, fn1d(xmin)
  CASE (2)
     x = [1, 1]
     CALL newuoa(fn2d, n, npt, x, rhobeg, rhoend, iprint, maxfun, w)
     PRINT *, x, fn2d(SIZE(x), x)
  CASE (3)
     DO i = 0,35
        PRINT *, (fn2d(2, [95.0 + i, j * 0.1]), j = 30,60)
     END DO
  END SELECT

  DEALLOCATE(tbl)
  STOP
CONTAINS

  PURE FUNCTION fn2d(n, x) RESULT(f)
    INTEGER, VALUE :: n
    REAL, INTENT(in) :: x(n)
    REAL :: f
    f = SUM((tbl(:, 2) - x(1)*(1 - EXP(-tbl(:, 1)/x(2))))**2)
  END FUNCTION fn2d

  PURE FUNCTION fn1d(x) RESULT(f)
    REAL, VALUE :: x
    REAL :: f
    f = fn2d(2, [vinf, x])
  END FUNCTION fn1d

  SUBROUTINE print_usage_and_stop()
    PRINT '(A)', "One parameters is expected: "//NEW_LINE('')// &
         "1 - run Golden Section Search test"//NEW_LINE('')// &
         "2 - run Powell's NEWUOA test"//NEW_LINE('')// &
         "3 - compute LSQ function over a 2D grid"
    STOP
  END SUBROUTINE print_usage_and_stop

END PROGRAM rot_curve
