MODULE findroot
  USE ieee_arithmetic
  
  INTERFACE
     PURE FUNCTION f1d(x) RESULT(y)
       VALUE :: x
     END FUNCTION f1d
  END INTERFACE

CONTAINS
  FUNCTION findroot_bs(fn, a, b, debug) RESULT(x)
    PROCEDURE(f1d) :: fn
    LOGICAL, OPTIONAL :: debug
    LOGICAL :: verbose = .FALSE.
    VALUE :: a, b
    x = IEEE_VALUE(x, IEEE_QUIET_NAN)

    IF (PRESENT(debug)) THEN
       verbose = debug
    END IF
    
    IF (a > b .or. fn(a) * fn(b) > 0) RETURN

    x_old = x

    DO
       x = (a + b) / 2

       IF (verbose) PRINT *, x, a, b
       
       IF (x == x_old) RETURN
       x_old = x
       
       IF (fn(x) * fn(b) < 0) THEN
          a = x
       ELSE
          b = x
       END IF
    END DO
  END FUNCTION findroot_bs

  FUNCTION findroot_nr(fn, dfn, a, b, debug) RESULT(x)
    PROCEDURE(f1d) :: fn, dfn
    VALUE :: a, b
    LOGICAL, OPTIONAL :: debug
    LOGICAL :: verbose

    IF (a > b .or. fn(a) * fn(b) > 0) THEN
       x = IEEE_VALUE(x, IEEE_QUIET_NAN)    
       RETURN
    END IF

    IF (PRESENT(debug)) THEN
       verbose = debug
    END IF
    
    x = (a + b) / 2

    w = IEEE_VALUE(x, IEEE_QUIET_NAN)
    DO
       dx = fn(x) / dfn(x)
       x = x - dx
       
       IF (x < a .OR. x > b) THEN
          x = (a + b) / 2
          IF (verbose) PRINT *, "# out of bracket: performing bisection"
       END IF
          
       IF (fn(x) * fn(b) < 0) THEN
          a = x
       ELSE   
          b = x
       END IF

       IF (verbose) PRINT *, x, a, b, dx

       IF (w == b - a) RETURN
       w = b - a
    END DO
  END FUNCTION findroot_nr
END MODULE findroot
