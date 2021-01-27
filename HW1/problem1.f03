! Problem 1: Floating point representation.
!
PROGRAM empirical_real_numbers
  IMPLICIT NONE
  REAL :: r ! REAL is by default 32 bit in Fortran
  CHARACTER(*), PARAMETER :: fmt = '(A, ES15.8, A, B0.32)'

  r = machine_epsilon_minus()
  WRITE(*, fmt=fmt) '(a)', r, ', ', r
  r = machine_epsilon_plus()
  WRITE(*, fmt=fmt) '(b)', r, ', ', r
  r = max_real()
  WRITE(*, fmt=fmt) '(c)', r, ', ', r
  r = min_positive_real()
  WRITE(*, fmt=fmt) '(d)', r, ', ', r
  PRINT *, 'Fortran''s HUGE and TINY REAL numbers: ', HUGE(0.0), TINY(0.0)

  STOP
  
CONTAINS 

  ! Returns the smallest number e such that 1.0 - e /= 1.0
  !
  FUNCTION machine_epsilon_minus()
    REAL :: machine_epsilon_minus, seed = 1.0
    DO
       IF (1.0 - seed == 1.0) EXIT
       machine_epsilon_minus = seed
       seed = seed / 2
    END DO
  END FUNCTION machine_epsilon_minus

  ! Returns the smallest number e such that 1.0 + e /= 1.0
  !
  FUNCTION machine_epsilon_plus()
    REAL :: machine_epsilon_plus, seed = 1.0
    DO
       IF (1.0 + seed == 1.0) EXIT
       machine_epsilon_plus = seed
       seed = seed / 2
    END DO
  END FUNCTION machine_epsilon_plus

  ! Return an empirically calculated largest float32 number
  !
  FUNCTION max_real()
    REAL, TARGET :: max_real
    ! To get better than a factor of 2 precision, need a good initial
    ! seed. An ideal seed will have all the ones in the binary
    ! representation of the mantissa, which can be achieved by either
    ! initializing the seed explicitly with the binary literal
    ! B'111111111111111111111111' or by summing up numbers 1.0/2**k
    ! for k: [0..23]. The partial sum below is sufficient to obtain
    ! the greatest float32 number to better than a factor of 2.
    REAL :: seed = 1.0 + 1.0/2 + 1.0/4 + 1.0/8 ! etc.

    DO
       IF (seed == seed * 2) EXIT
       max_real = seed
       seed = seed * 2
    END DO
  END FUNCTION max_real

  ! Return an empirically calculated smallest positive float32 number
  !
  ! Discussion:
  !   This will produce a subnormal number
  !   (https://en.wikipedia.org/wiki/Denormal_number) and trigger
  !   floating-point exceptions IEEE_OVERFLOW_FLAG, IEEE_UNDERFLOW_FLAG and
  !   IEEE_DENORMAL, which Fortran will report upon exit, unless suppressed.
  !   
  FUNCTION min_positive_real()
    REAL :: min_positive_real, seed = 1.0
    DO
       IF (seed / 2 == 0.0) EXIT
       seed = seed / 2
    END DO
    min_positive_real = seed
  END FUNCTION min_positive_real
  
END PROGRAM empirical_real_numbers
