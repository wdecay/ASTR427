MODULE interpol
  IMPLICIT NONE

  TYPE, ABSTRACT :: InterpolationBase
     REAL(kind=8), DIMENSION(:, :), POINTER :: data
   CONTAINS
     PRIVATE
     PROCEDURE :: closest_idx
     PROCEDURE(interpolate), DEFERRED :: interp_at
  end type InterpolationBase

  INTERFACE
     FUNCTION interpolate(this, x) RESULT(val)
       IMPORT InterpolationBase
       CLASS(InterpolationBase), INTENT(in) :: this
       REAL(kind=8), INTENT(in) :: x
       REAL(kind=8) :: val
     END FUNCTION INTERPOLATE
  END INTERFACE

  TYPE, EXTENDS(InterpolationBase) :: LinearInterpolation
   CONTAINS
     PROCEDURE :: interp_at => interp_at_linear
  END TYPE LinearInterpolation

  TYPE, EXTENDS(InterpolationBase) :: NevilleInterpolation
     INTEGER :: n
   CONTAINS
     PROCEDURE :: interp_at => interp_at_neville
  END TYPE NevilleInterpolation

CONTAINS 
  FUNCTION closest_idx(this, x) result(idx)
    CLASS(InterpolationBase), INTENT(in) :: this
    REAL(kind=8), INTENT(in) :: x
    INTEGER :: idx, l, r, m
    l = 1
    r = SIZE(this%data, 1)

    ! Edge cases
    IF (this%data(l, 1) > x .OR. this%data(r, 1) < x) THEN
       idx = -1
       RETURN
    END IF

    IF (this%data(r, 1) == x) THEN
       idx = r
       RETURN
    END IF
    
    ! Binary search
    DO
       m = (l + r) / 2
       IF (this%data(m, 1) <= x) THEN
          IF (this%data(m + 1, 1) > x) THEN
             idx = m
             RETURN
          END IF
          l = m
          CYCLE
       ELSE
          r = m
       END IF
    END DO
  END FUNCTION closest_idx

  FUNCTION interp_at_linear(this, x) result(y)
    CLASS(LinearInterpolation), INTENT(in) :: this
    REAL(kind=8), INTENT(in) :: x
    REAL(kind=8) :: y, a
    INTEGER :: idx

    idx = this%closest_idx(x)

    ! edge case: attempted extrapolation
    IF (idx == -1) THEN
       ERROR STOP "Unable to extrapolate."
    END IF

    ! edge case: x is exactly the last element
    IF (idx == SIZE(this%data, 1)) THEN
       y = this%data(idx, 2)
       RETURN
    END IF
    
    a = (this%data(idx + 1, 1) - x) / (this%data(idx + 1, 1) - this%data(idx, 1))
    y = a * this%data(idx, 2) + (1 - a) * this%data(idx + 1, 2)
  END FUNCTION interp_at_linear

  FUNCTION interp_at_neville(this, x) result(y)
    CLASS(NevilleInterpolation), INTENT(in) :: this
    REAL(kind=8), INTENT(in) :: x
    REAL(kind=8) :: y, t, dy
    INTEGER :: idx, shift, i, m
    REAL(kind=8) :: c(this%n + 1), d(this%n + 1), p(this%n + 1)

    idx = this%closest_idx(x)

    ! WARNING: it is assumed here that the input is evenly
    ! spaced in the independent variable
    shift = (this%n + 1)/ 2
    
    ! edge cases
    IF (this%n + 1 > SIZE(this%data, 1)) THEN
       ERROR STOP "Not enough points for interpolation."
    END IF

    IF (idx == -1 .AND. x > this%data(1, 1)) idx = SIZE(this%data, 1)
    
    IF (idx - shift <= 0) shift = idx - 1
    IF (idx - shift + this%n > SIZE(this%data, 1)) THEN
       shift = (idx + this%n - SIZE(this%data, 1))
    END IF

    idx = idx - shift


    ! initialization of P, C and D
    DO i=1, this%n + 1
       p(i) = this%data(idx + i - 1, 2)
       c(i) = p(i)
       d(i) = p(i)
    END DO

    y = c(1)

    DO m=1, this%n
       DO i=1, this%n + 1 - m
          t = (c(i+1) - d(i)) / (this%data(idx + i - 1, 1) - this%data(idx + i + m - 1, 1))
          d(i) = (this%data(idx + i + m - 1, 1) - x) * t
          c(i) = (this%data(idx + i - 1, 1) - x) * t
       END DO
       y = y + c(1)
    END DO

  END FUNCTION interp_at_neville

  SUBROUTINE load_data(fname, out)
    REAL(kind=8), DIMENSION(:, :), ALLOCATABLE, INTENT(out) :: out
    INTEGER :: n = 0, error, i, j
    CHARACTER(*) :: fname
    CHARACTER(1) :: buf
    LOGICAL :: has_header = .false.
    OPEN (99, FILE=fname, ACTION='read')
    DO
       READ(99, *, iostat=error) buf
       IF (error /= 0) EXIT
       
       IF (buf == '#') THEN
          IF (n == 0) THEN
             has_header = .true.
          ELSE
             ERROR STOP "Invalid input file."
          END IF
       ELSE
          n = n + 1          
       END IF
    END DO
    ALLOCATE(out(n, 2))
    REWIND 99
    IF (has_header) READ(99, *)
    READ(99, *) ((out(i, j), j=1,2), i=1,n)
    CLOSE(99)
  END SUBROUTINE load_data

END MODULE interpol

PROGRAM interpolation_demo
  USE interpol  
  IMPLICIT NONE
  
  REAL(kind=8), dimension(:, :), allocatable, TARGET :: tbl
  REAL(kind=8) :: x
  type(LinearInterpolation) :: lin_interp
  type(NevilleInterpolation) :: nev_interp

  CALL load_data('hw1_data.txt', tbl)
  write (*,*) 'Enter x'
  READ *, x

  PRINT *, "Actual value:         ", 100 / x**2
  lin_interp = LinearInterpolation(tbl)
  PRINT *, "Linear interpolation: ", lin_interp%interp_at(x)
  nev_interp = NevilleInterpolation(tbl, 4)
  PRINT *, "Neville's algorithm:  ", nev_interp%interp_at(x)

  OPEN(10, file="tabulation.txt")
  x = 1
  DO
     WRITE(10, *) x, nev_interp%interp_at(x)
     x = x + 0.1
     IF (x > 6) EXIT
  END DO

  CALL EXECUTE_COMMAND_LINE('gnuplot plot.plt')
  
  DEALLOCATE(tbl)
  STOP
END PROGRAM interpolation_demo
