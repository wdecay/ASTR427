! Problems 2 and 3:
! - 1D least squares optimization
! - 2D least squares optimization
! - tabulation of the LSQ error function
!
! Run without parameters to see usage information.
!
PROGRAM rot_curve
  USE optimization
  USE dataio

  IMPLICIT NONE

  REAL, PARAMETER :: vinf = 100 ! fixed vinf for 1D
                                ! optimization. Note: the optimal
                                ! value is ~110.780
  REAL, dimension(:, :), allocatable, TARGET :: tbl
  REAL :: x(2), rhobeg=0.2, rhoend=1.0e-8, xmin
  INTEGER :: n = 2, w(200), npt=5, iprint=0, maxfun=5000, n_task = 0
  INTEGER :: i, j, ios
  CHARACTER(100) :: arg_str

  IF (COMMAND_ARGUMENT_COUNT() /= 1) CALL print_usage_and_stop
  
  CALL GET_COMMAND_ARGUMENT(1, arg_str)
  READ(arg_str, *, iostat=ios) n_task

  IF (n_task < 0 .OR. n_task > 2) CALL print_usage_and_stop

  ! n_task is valid, can load the data
  CALL load_data('rot.csv', tbl)
  
  SELECT CASE (n_task)
  CASE (0) ! "undocumented" and debugging only. Use to test GSS.
     xmin = gss(test_fn, -1.0, 5.0)
     PRINT *, xmin
  CASE (1) ! 1D optimization with GSS and 2D optimization with NEWUOA
     xmin = gss(fn1d, 1.0, 5.0)
     PRINT '(4A16)', "Method", "Vinf", "R0", "LSQ error"
     PRINT '(a)', REPEAT('-', 16*4)
     PRINT '(A16,3F16.7)', "GSS", vinf, xmin, fn1d(xmin)
     x = [1, 1]
     CALL newuoa(fn2d, n, npt, x, rhobeg, rhoend, iprint, maxfun, w)
     PRINT '(A16,3F16.7)', "NEWUOA", x, fn2d(SIZE(x), x)
  CASE (2) ! LSQ error function tabulation for vinf \in [95,130],
           ! r0 \in [3,6]
     DO i = 0,35
        PRINT *, (fn2d(2, [95.0 + i, j * 0.1]), j = 30,60)
     END DO
  END SELECT

  DEALLOCATE(tbl)
  STOP
CONTAINS

  ! 2D LSQ error function. Used directly by NEWUOA and also
  ! indirectly by GSS with fixed vinf
  !
  ! Arguments:
  !  n: number of independent variables
  !  x: array of independent variables vinf = x(1), r0 = x(2)
  !
  ! Returns:
  !  LSQ error for given vinf and r0.
  !
  PURE FUNCTION fn2d(n, x) RESULT(f)
    INTEGER, VALUE :: n
    REAL, INTENT(in) :: x(n)
    REAL :: f
    f = SUM((tbl(:, 2) - x(1)*(1 - EXP(-tbl(:, 1)/x(2))))**2)
  END FUNCTION fn2d

  ! 1D LSQ error function. Used directly by GSS with fixed vinf.
  !
  ! Arguments:
  !  x: independent variable (r0)
  !
  ! Returns:
  !  LSQ error for the given r0 and fixed vinf.
  !
  PURE FUNCTION fn1d(x) RESULT(f)
    REAL, VALUE :: x
    REAL :: f
    f = fn2d(2, [vinf, x])
  END FUNCTION fn1d
  
  ! Prints usage and stops the program.
  !
  SUBROUTINE print_usage_and_stop()
    PRINT '(A)', "One parameters is expected: "//NEW_LINE('')// &
         "1 - run optimization tests"//NEW_LINE('')// &
         "2 - compute LSQ function over a 2D grid"
    STOP
  END SUBROUTINE print_usage_and_stop

  ! This is a test function for debugging purposes.
  PURE FUNCTION test_fn(x) RESULT(f)
    REAL, VALUE :: x
    REAL :: f
    f = x**2 + 1
  END FUNCTION test_fn

END PROGRAM rot_curve
