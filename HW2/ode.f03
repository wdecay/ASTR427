! Library of numerical integration methods. Contains Euler's, leapfrog
! and 4th order Runge-Kutta step implementations. Each method supports
! an arbitrary number of coupled first-order ODEs.
!
! Discussion:
!  In most cases, these integration steps will be called in a
!  loop. Fortran compiler may decide to inline them in order to reduce
!  the overhead from calling a subroutine. There exist compiler flags
!  which promote more aggressive inlining.
!
MODULE ode
  IMPLICIT NONE

  INTERFACE
     ! Interface of the derivative-generating function.
     !
     ! Arguments:
     !  t: independent variable
     !  x: array of dependent variables
     !
     ! Returns:
     !  Array of derivaties of the dependent variables. Must have the
     !  same size as x.
     !
     FUNCTION rk_fn(t, x) RESULT(dd)
       REAL, INTENT(in) :: t
       REAL, DIMENSION(:), INTENT(in) :: x
       REAL, DIMENSION(SIZE(x, 1)) :: dd
     END FUNCTION rk_fn

     ! Interface of the derivative-generating function used in
     ! leapfrog. Same for `f(v)` and `g(x)`.
     !
     ! Arguments:
     !  z: array of dependent variables (i.e., `x` or `v`)
     !  n: size of the result
     !
     ! Returns:
     !  n derivaties of coordinates or velocities, depending on
     !  whether this is an `f` or a `g` functon
     !
     ! Note:
     !  In this implementation of leapfrog the derivatives cannot
     !  explicitly depend on t.
     FUNCTION lf_fn(z, n) RESULT(dd)
       IMPLICIT NONE
       REAL, DIMENSION(:), INTENT(in) :: z
       INTEGER, INTENT(in) :: n
       REAL, DIMENSION(n) :: dd
     END FUNCTION lf_fn

  END INTERFACE

CONTAINS

  ! Euler's method step.
  !
  ! Arguments:
  !   fn: derivative-generating callback function
  !   t: independent variable
  !   x: array of dependent variables
  !   h: step size
  !
  ! Returns:
  !  Estimated dependent variables corresponding to t+h.
  !
  FUNCTION euler_step(fn, t, x, h) RESULT(xnext)
    PROCEDURE(rk_fn) :: fn
    REAL, INTENT(in) :: t, h
    REAL, DIMENSION(:), INTENT(in) :: x
    REAL, DIMENSION(SIZE(x, 1)) :: xnext

    xnext = x + h * fn(t, x)
  END FUNCTION euler_step

  ! Leapfrog step.
  !
  ! Arguments:
  !  f: `f(v)` function returning the `x` derivaties
  !  g: `g(x)` function returning the `v` derivaties
  !  x: array of `coordinates`
  !  v: array of `velocities`
  !  h: step size
  !
  ! Result:
  !  Updates x and v vectors.
  !
  ! Discusion:
  !  In most if not all scenarios the sizes of `x` and `v` will be the
  !  same. With that in mind, it should be ok to make the contract of
  !  this function the same as that of `euler_step` and
  !  `rk4_step`. I.e., have a single derivative-generating function
  !  and a single array of dependent variables. This interface,
  !  however, seems explicit and intuitive.
  SUBROUTINE leapfrog_step(f, g, x, v, h)
    PROCEDURE(lf_fn) :: f
    PROCEDURE(lf_fn) :: g
    REAL, DIMENSION(:) :: x, v
    REAL :: h

    v = v + h / 2 * g(x, SIZE(v, 1))
    x = x + h * f(v, SIZE(x, 1))
    v = v + h / 2 * g(x, SIZE(v, 1))
  END SUBROUTINE leapfrog_step

  ! 4th order Runge-Kutta step.
  !
  ! Arguments:
  !  fn: derivative-generating callback function
  !  t: independent variable
  !  x: array of dependent variables
  !  h: step size
  !
  ! Returns:
  !  Estimated dependent variables corresponding to t+h.
  !
  ! Note:
  !  This is the most simple implementation of the algorithm. More
  !  efficient variants with adaptive step size exist. Not intended
  !  for `real-world` applications.
  !
  FUNCTION rk4_step(fn, t, x, h) RESULT(xnext)
    PROCEDURE(rk_fn) :: fn
    REAL, INTENT(in) :: t, h
    REAL, DIMENSION(:), INTENT(in) :: x
    REAL, DIMENSION(SIZE(x, 1)) :: xnext
    REAL, DIMENSION(SIZE(x, 1)) :: k1, k2, k3, k4

    k1 = h * fn(t, x)
    k2 = h * fn(t + h/2, x + k1/2)
    k3 = h * fn(t + h/2, x + k2/2)
    k4 = h * fn(t + h, x + k3)
    xnext = x + (k1 + 2 * k2 + 2 * k3 + k4) / 6
  END FUNCTION rk4_step
END MODULE ode
