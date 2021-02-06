! Problem 2. Second-order differential equation.
!
PROGRAM integrator_comparison
  USE ode
  IMPLICIT NONE

  CHARACTER(100) :: h_str
  REAL, PARAMETER :: t_max = 30.0
  REAL :: h
  REAL, DIMENSION(:), POINTER :: out_ptr => NULL()
  REAL, DIMENSION(7), TARGET :: out

  IF (COMMAND_ARGUMENT_COUNT() == 1) THEN
     ! If one command-line argument received, it is assumed to be the
     ! step size. In the case, the function is integrated with 3
     ! different methods and the results from each are tabulated.
     CALL GET_COMMAND_ARGUMENT(1, h_str) 
     READ(h_str, *) h
     CALL integrate(0.0, t_max, h)
  ELSE
     ! Without arguments (or if more than 1) convergence test is
     ! executed for a geometric sequence of step sizes.
     out_ptr => out
     h = 1.0/2**15 ! better avoid noise in low binary digits, or else
                   ! poor convergence is likely
     DO
        CALL integrate(0.0, t_max, h, out_ptr, .FALSE.)
        ! nice array subset syntax below 
        PRINT *, [LOG(h), LOG(ABS(out([2, 4, 6]) - COS(t_max)))] / LOG(2.0)
        h = h * 2
        IF (h > 2.0) EXIT
     END DO
  END IF
  
  STOP

CONTAINS
  ! Integrates a system of first order ODEs.
  !
  ! Arguments:
  !  t_from: Initial value of the independent variable
  !  t_to: Final value of the independent variable
  !  h: step size
  !  final_out_ptr: Optional. If provided the values at t_to are
  !  written to this array
  !  print_output: Optional. Set to .FALSE. to suppress terminal
  !  output.
  !
  ! Discussion:
  !  This will print the values after each step, which may be
  !  excessive if the interval is large and/or h is small. Having some
  !  output throttling mechanism would be nice, but for this
  !  assignment it wasn't necessary.
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
       last = [t, x_euler, x_lf,  x_rk] ! the total of 7 values
       IF (print_output_val) PRINT *, last
       IF (t == t_to) EXIT
       t = t + h
       IF (t > t_to) t = t_to ! to make sure t_to is hit

       x_euler = euler_step(rk_f, t, x_euler, h)
       CALL leapfrog_step(lf_f, lf_g, x_lf(1:1), x_lf(2:2), h)
       x_rk = rk4_step(rk_f, t, x_rk, h)
    END DO

    IF (PRESENT(final_out_ptr)) THEN
       final_out_ptr = last
    END IF    
  END SUBROUTINE integrate

  ! Euler/Runge-Kutta derivatives generating function.
  ! 
  FUNCTION rk_f(t, x) RESULT(dd)
    REAL, INTENT(in) :: t
    REAL, DIMENSION(:), INTENT(in) :: x
    REAL, DIMENSION(SIZE(x, 1)) :: dd

    dd = [x(2), -x(1)]
  END FUNCTION rk_f

  ! Leapfrog `f(v)` function.
  !
  FUNCTION lf_f(v, n) RESULT(dd)
    REAL, DIMENSION(:), INTENT(in) :: v
    INTEGER, INTENT(in) :: n
    REAL, DIMENSION(n) :: dd

    dd = v
  END FUNCTION lf_f

  ! Leapfrog `g(x)` function.
  !
  FUNCTION lf_g(x, n) RESULT(dd)
    REAL, DIMENSION(:), INTENT(in) :: x
    INTEGER, INTENT(in) :: n
    REAL, DIMENSION(n) :: dd
    
    dd = -x
  END FUNCTION lf_g
  
END PROGRAM integrator_comparison
