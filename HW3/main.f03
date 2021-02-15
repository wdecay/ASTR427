MODULE test_problems
  USE findroot

  PARAMETER (pi=4.D0*DATAN(1.D0))

CONTAINS
  FUNCTION square_root_finder(a, x_lo, x_hi, method, debug) RESULT(x)
    VALUE :: a, x_lo, x_hi
    LOGICAL :: debug

    SELECT CASE (method)
    CASE (1)
       x = findroot_bs(f, x_lo, x_hi, debug)
    CASE (2)
       x = findroot_nr(f, df, x_lo, x_hi, debug)
    CASE DEFAULT
       ERROR STOP "Unknown method"
    END SELECT

  CONTAINS ! internal subprograms
    ! http://fortranwiki.org/fortran/show/Fortran+2008+status
    PURE FUNCTION f(x) RESULT(y)
      VALUE :: x
      y = x**2 - a
    END FUNCTION f
    
    PURE FUNCTION df(x) RESULT(y)
      VALUE :: x
      y = 2*x
    END FUNCTION df
  END FUNCTION square_root_finder

  
  FUNCTION kepler(w, e, method, debug) RESULT(x)
    VALUE :: w, e
    LOGICAL :: debug
    ! PARAMETER (pi=4.D0*DATAN(1.D0))
    
    SELECT CASE (method)
       CASE (1)
          x = findroot_bs(f, 0.0, 2*pi, debug)
       CASE (2)
          x = findroot_nr(f, df, 0.0, 2*pi, debug)
       CASE DEFAULT
          ERROR STOP "Unknown method"
       END SELECT

  CONTAINS
    PURE FUNCTION f(x) RESULT(y)
      VALUE :: x
      y = w - x + e*SIN(x)
    END FUNCTION f
    
    PURE FUNCTION df(x) RESULT(y)
      VALUE :: x
      y = -1.0 + e*COS(x)
    END FUNCTION df

  END FUNCTION kepler
END MODULE test_problems


PROGRAM root_finding
  USE test_problems
  CHARACTER(100) :: arg_str
  
  CALL GET_COMMAND_ARGUMENT(1, arg_str) 
  READ(arg_str, *) n_task

  SELECT CASE (n_task)
  CASE (1, 2)
     x = square_root_finder(3.0, 1.0, 2.0, n_task, .TRUE.)
  CASE (3, 4)
     e = kepler(1.5, 0.5, n_task - 2, .TRUE.)
  CASE (5, 6)
     e = kepler(1.5, 0.9, n_task - 4, .TRUE.)
  CASE (7, 8)
     dm = 2 * pi / 20
     CALL GET_COMMAND_ARGUMENT(2, arg_str) 
     READ(arg_str, *) ecc
     
     DO i = 0, 20
        a = i * dm
        e = kepler(a, ecc, n_task - 6, .FALSE.)
        PRINT *, a, e, a - e + ecc * sin(e)
     END DO
  CASE DEFAULT
     ERROR STOP "Unknown task."
  END SELECT
END PROGRAM root_finding
