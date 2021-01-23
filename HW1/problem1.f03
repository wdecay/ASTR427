PROGRAM empirical_real_numbers
  IMPLICIT NONE

  PRINT *, "(a) ", machine_epsilon_minus()
  PRINT *, "(b) ", machine_epsilon_plus()
  PRINT *, "(c) ", max_real() 
  PRINT *, "(d) ", min_positive_real()
  PRINT *, TINY(0.0)
  STOP
  
CONTAINS 
  
  FUNCTION machine_epsilon_minus()
    REAL :: machine_epsilon_minus, seed = 1.0
    DO
       IF (1.0 - seed == 1.0) EXIT
       machine_epsilon_minus = seed
       seed = seed / 2
    END DO
    write(*, fmt='(B0.32)') machine_epsilon_minus
  END FUNCTION machine_epsilon_minus

  FUNCTION machine_epsilon_plus()
    REAL :: machine_epsilon_plus, seed = 1.0
    DO
       IF (1.0 + seed == 1.0) EXIT
       machine_epsilon_plus = seed
       seed = seed / 2
    END DO
    write(*, fmt='(B0.32)') machine_epsilon_plus
  END FUNCTION machine_epsilon_plus
  
  FUNCTION max_real()
    REAL, TARGET :: max_real
    REAL :: seed = B'111111111111111111111111'
    DO
       IF (seed == seed * 2) EXIT
       max_real = seed
       seed = seed * 2
    END DO
    write(*, fmt='(B0.32)') max_real
  END FUNCTION max_real

  FUNCTION min_positive_real()
    ! https://en.wikipedia.org/wiki/Denormal_number
    REAL :: min_positive_real, seed = 1.0
    DO
       IF (seed / 2 == 0.0) EXIT
       seed = seed / 2
    END DO
    min_positive_real = seed ! TINY(0.0)
    write(*, fmt='(B0.32)') min_positive_real
  END FUNCTION min_positive_real
  
END PROGRAM empirical_real_numbers
