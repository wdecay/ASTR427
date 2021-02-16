MODULE test_problems
  USE findroot

  PARAMETER (pi=4.0*ATAN(1.0))

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

  SUBROUTINE calculate_orbit(n, ecc, method, quiet)
    LOGICAL :: quiet

    dm = 2 * pi / n
    DO i = 0, n - 1
       a = i * dm
       e = kepler(a, ecc, method, .FALSE.)
       IF (.NOT. quiet) PRINT *, a, e, a - e + ecc * SIN(e)
    END DO
  END SUBROUTINE calculate_orbit
END MODULE test_problems


PROGRAM root_finding
  USE test_problems
  CHARACTER(100) :: arg_str
  
  CALL GET_COMMAND_ARGUMENT(1, arg_str) 
  READ(arg_str, *) n_task

  CALL GET_COMMAND_ARGUMENT(2, arg_str) 
  READ(arg_str, *) param

  
  SELECT CASE (n_task)
  CASE (1, 2)
     x = square_root_finder(param, 0.0, MAX(param, 1.0), n_task, .TRUE.)
  CASE (3, 4)
     e = kepler(1.5, param, n_task - 2, .TRUE.)
  CASE (5, 6)
     CALL calculate_orbit(20, param, n_task - 4, .FALSE.)
  CASE (7)
     WRITE(*, fmt='(A)') "Calculating 100000 orbital positions..."
     CALL cpu_time(tic)
     CALL calculate_orbit(100000, param, 1, .TRUE.)
     CALL cpu_time(toc)
     t1 = toc - tic

     CALL cpu_time(tic)
     CALL calculate_orbit(50000, param, 2, .TRUE.)
     CALL cpu_time(toc)
     t2 = toc - tic

     WRITE(*, fmt="(A, F5.3, A, F5.3, A)") "Bisection: ", t1, "s; Newton-Raphson: ", t2, "s"

  CASE DEFAULT
     ERROR STOP "Unknown task."
  END SELECT
END PROGRAM root_finding
