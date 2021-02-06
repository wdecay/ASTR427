PROGRAM integrator_comparison
  USE ode
  IMPLICIT NONE

  CHARACTER(100) :: h_str
  REAL, PARAMETER :: t_max = 30.0
  REAL :: h
  REAL, DIMENSION(:), POINTER :: out_ptr => NULL()
  REAL, DIMENSION(7), TARGET :: out

  IF (COMMAND_ARGUMENT_COUNT() == 1) THEN
     CALL GET_COMMAND_ARGUMENT(1, h_str) 
     READ(h_str, *) h
     CALL integrate(0.0, t_max, h)
  ELSE
     ! as a function of step size. comparison
     out_ptr => out
     h = 1.0/2**15
     DO
        CALL integrate(0.0, t_max, h, out_ptr, .FALSE.)
        PRINT *, [LOG(h), LOG(ABS(out([2, 4, 6]) - COS(t_max)))] / LOG(2.0)
        h = h * 2
        IF (h > 2.0) EXIT
     END DO
  END IF
  
  STOP

CONTAINS
  SUBROUTINE integrate(t_from, t_to, h, final_out_ptr, print_output)
    REAL, INTENT(in) :: t_from, t_to, h
    REAL, DIMENSION(:), POINTER, INTENT(in), OPTIONAL :: final_out_ptr
    LOGICAL, INTENT(in), OPTIONAL :: print_output
    
    REAL, DIMENSION(2) :: x_euler, x_lf, x_rk
    REAL :: t
    REAL, DIMENSION(7) :: last
    LOGICAL :: print_output_val = .TRUE.

    t = t_from
    IF (PRESENT(print_output)) print_output_val = print_output
    x_euler = [1, 0]
    x_lf = x_euler
    x_rk = x_euler

    DO
       last = [t, x_euler, x_lf,  x_rk]
       IF (print_output_val) PRINT *, last
       IF (t == t_to) EXIT
       t = t + h
       IF (t > t_to) t = t_to

       x_euler = euler_step(f, t, x_euler, h)
       CALL leapfrog_step(f_lf, g_lf, x_lf(1:1), x_lf(2:2), h)
       x_rk = rk4_step(f, t, x_rk, h)
    END DO

    IF (PRESENT(final_out_ptr)) THEN
       final_out_ptr = last
    END IF
    
  END SUBROUTINE integrate
  
  FUNCTION f(t, y) RESULT(yprime)
    REAL, INTENT(in) :: t
    REAL, DIMENSION(:), INTENT(in) :: y
    REAL, DIMENSION(SIZE(y, 1)) :: yprime

    yprime = [y(2), -y(1)]
  END FUNCTION f

  FUNCTION f_lf(v, n) RESULT(w)
    REAL, DIMENSION(:), INTENT(in) :: v
    INTEGER, INTENT(in) :: n
    REAL, DIMENSION(n) :: w

    w(1) = v(1)
  END FUNCTION f_lf

  FUNCTION g_lf(x, n) RESULT(w)
    REAL, DIMENSION(:), INTENT(in) :: x
    INTEGER, INTENT(in) :: n
    REAL, DIMENSION(n) :: w
    
    w(1) = -x(1)
  END FUNCTION g_lf
  
END PROGRAM integrator_comparison
