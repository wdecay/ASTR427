PROGRAM rot_curve
  USE optimization
  USE dataio

  IMPLICIT NONE

  REAL, dimension(:, :), allocatable, TARGET :: tbl
  REAL :: x(2), rhobeg=0.2, rhoend=1.0e-8, xmin, v0 = 100
  INTEGER :: n = 2, w(200), npt=5, iprint=0, maxfun=5000, n_task, i, j
  CHARACTER(100) :: arg_str

  IF (COMMAND_ARGUMENT_COUNT() /= 1) CALL print_usage_and_stop
  
  CALL GET_COMMAND_ARGUMENT(1, arg_str)
  READ(arg_str, *) n_task

  CALL load_data('rot.csv', tbl)
  
  SELECT CASE (n_task)
  CASE (1)
     xmin = gss(fn1, 1.0, 5.0)
     PRINT *, xmin, fn1(xmin)
  CASE (2)
     x = [1, 1]
     CALL newuoa(fn, n, npt, x, rhobeg, rhoend, iprint, maxfun, w)
     PRINT *, x
  CASE (3)
     DO j = 0, 35, 1
        v0 = 95.0 + j
        PRINT *, (fn1(i*0.1), i=30,60,1)
     END DO
  CASE DEFAULT
     CALL print_usage_and_stop
  END SELECT

CONTAINS

  SUBROUTINE fn(n, x, f)
    INTEGER :: n
    REAL :: x(n), f
    f = SUM((tbl(:, 2) - x(1)*(1 - EXP(-tbl(:, 1)/x(2))))**2)
  END SUBROUTINE fn

  PURE FUNCTION fn1(x) RESULT(f)
    REAL, VALUE :: x
    REAL :: f
    f = SUM((tbl(:, 2) - v0 * (1 - EXP(-tbl(:, 1)/x)))**2)
  END FUNCTION fn1

  SUBROUTINE print_usage_and_stop()
    PRINT '(A)', "One parameters is expected: "//NEW_LINE('')// &
         "1 - run Golden Section test"//NEW_LINE('')// &
         "2 - run Powell's NEWUOA test"//NEW_LINE('')// &
         "3 - compute LSQ function over a 2D grid"
    STOP
  END SUBROUTINE print_usage_and_stop

END PROGRAM rot_curve
