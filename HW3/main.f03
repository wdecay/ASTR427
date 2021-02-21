! Problems 1 and 2:
! - Calculation of quare roots
! - Numerical solution of Kepler's equation
!
! Discussion:
!
!   Some of the code was placed into the module called test_problems.
!   This was done to take advantage of the internal procedures which
!   are only allowed if the containing function or subroutine belongs
!   to a module. In modern Fortran, internal procedures allow for
!   closure-like constructs where the parameters of the host function
!   are accessible in the contained internal procedures. If the
!   compiler supports Fortran 2008 standard, internal procedures can
!   be passed as actual arguments (callbacks) to other functions,
!   which enables elegant callback parametrization.  Compiling this
!   code requires that gfortran be no older than version 4.6
!   (https://gcc.gnu.org/wiki/GFortran/News#gfortran_4.6). For other
!   compilers refer to
!   http://fortranwiki.org/fortran/show/Fortran+2008+status (see
!   `Internal procedure as an actual argument`)
!
MODULE test_problems
  USE findroot

  PARAMETER (pi=4.0*ATAN(1.0)) ! Fortran doesn't provide Pi; instead
                               ! it's commonly defined this way.

CONTAINS

  ! Computes the square root of a number using either bisection or
  ! Newton-Raphson method.
  !
  ! Arguments:
  !  a: parameter `a` in equation x**2 - a = 0
  !  x_lo: lower bound for square root search
  !  x_hi: higher bound for square root search
  !  method: 1 for Bisection, 2 for Newton-Raphson
  !  debug: Set to .TRUE. to print debug information.
  !
  ! Returns: The square root of `a` obtained numerically via one of
  !          the two methods.
  !
  FUNCTION square_root_finder(a, x_lo, x_hi, method, debug) RESULT(x)
    INTENT(in) :: a, x_lo, x_hi, method, debug
    LOGICAL :: debug

    SELECT CASE (method)
    CASE (1)
       x = findroot_bs(f, x_lo, x_hi, debug)
    CASE (2)
       x = findroot_nr(f, df, x_lo, x_hi, debug)
    CASE DEFAULT
       ERROR STOP "Unknown method"
    END SELECT
    
  CONTAINS ! internal precedures

    ! Function definition; y = x**2 - a
    !
    PURE FUNCTION f(x) RESULT(y)
      VALUE :: x
      y = x**2 - a
    END FUNCTION f

    ! Derivative definition; y = 2x (only used by Newton-Raphson)
    !
    PURE FUNCTION df(x) RESULT(y)
      VALUE :: x
      y = 2*x
    END FUNCTION df
  END FUNCTION square_root_finder

  ! Solves the Kepler's equation using either bisection or
  ! Newton-Raphson method.
  !
  ! Arguments:
  !
  !  w: mean anomaly (M). Note: in this module implicit types were
  !  used to reduce the amount of boilerplate code. In Fortran, m is
  !  assigned the INTEGER type, whereas w is REAL. There may be a few
  !  other places where similar name substitutions are made for the
  !  same reason.
  !  e: eccentricity of the orbit
  !  method: 1 for Bisection, 2 for Newton-Raphson
  !  debug: Set to .TRUE. to print debug information.
  !
  ! Returns: Eccentric anomaly for the given values of w (or M) and e.
  !  
  FUNCTION kepler(w, e, method, debug) RESULT(x)
    INTENT(in) :: w, e, method, debug
    LOGICAL :: debug
    
    SELECT CASE (method)
       CASE (1)
          x = findroot_bs(f, 0.0, 2*pi, debug)
       CASE (2)
          x = findroot_nr(f, df, 0.0, 2*pi, debug)
       CASE DEFAULT
          ERROR STOP "Unknown method"
       END SELECT

  CONTAINS ! internal procedures

    ! Function definition
    !
    PURE FUNCTION f(x) RESULT(y)
      VALUE :: x
      y = w - x + e*SIN(x)
    END FUNCTION f

    ! Derivative definition (only used by Newton-Raphson)
    !    
    PURE FUNCTION df(x) RESULT(y)
      VALUE :: x
      y = -1.0 + e*COS(x)
    END FUNCTION df

  END FUNCTION kepler

  ! Calculates the eccentric anomaly for `n` equally spaced values of
  ! the mean anomaly using either bisection of Newton-Raphson method.
  !
  ! Arguments:
  !  n: number mean anomaly values in the interval [0, 2pi] to use
  !  e: eccentricity of the orbit
  !  method: 1 for Bisection, 2 for Newton-Raphson
  !  quiet: Set to .TRUE. to suppress output
  !  
  SUBROUTINE calculate_orbit(n, e, method, quiet)
    LOGICAL :: quiet
    INTENT(in) :: n, e, method, quiet

    dm = 2 * pi / n ! mean anomaly increment
    DO i = 0, n - 1
       a = i * dm ! current value of the mean anomaly
       x = kepler(a, e, method, .FALSE.)
       IF (.NOT. quiet) PRINT *, a, x, a - x + e * SIN(x)
    END DO
  END SUBROUTINE calculate_orbit
END MODULE test_problems

! Main program
! Takes two parameters:
! $ ./main <n_task:INTEGER> <param:REAL>
! According to the value of `n_task` does the following:
! 1: computes the square root of `param` using bisection
! 2: computes the square root of `param` using Newton-Raphson
! 3: solves Kepler's equation for M=1.5 and e=`param` via bisection
! 4: solves Kepler's equation for M=1.5 and e=`param` via
!    Newton-Raphson
! 5: solves Kepler's equation for 20 equally spaced M and e=`param`
!    via bisection
! 6: solves Kepler's equation for 20 equally spaced M and e=`param`
!    via Newton-Raphson
! 7: Runs a simple performance benchmark for bisection and
!    Newton-Raphson by computing 100000 mean anomalies with each
!    method; e=`param`
!
PROGRAM root_finding
  USE test_problems
  CHARACTER(100) :: arg_str
  
  CALL GET_COMMAND_ARGUMENT(1, arg_str) 
  READ(arg_str, *) n_task

  CALL GET_COMMAND_ARGUMENT(2, arg_str) 
  READ(arg_str, *) param

  
  SELECT CASE (n_task)
  CASE (1, 2)
     x = square_root_finder(param, MIN(param, 1.0), MAX(param, 1.0), n_task, .TRUE.)
     PRINT *, "# Result: ", x
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
     CALL calculate_orbit(100000, param, 2, .TRUE.)
     CALL cpu_time(toc)
     t2 = toc - tic

     WRITE(*, fmt="(A, F5.3, A, F5.3, A)") "Bisection: ", t1, "s; Newton-Raphson: ", t2, "s"

  CASE DEFAULT
     ERROR STOP "Unknown task."
  END SELECT
END PROGRAM root_finding
