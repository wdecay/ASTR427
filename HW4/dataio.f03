MODULE dataio
  IMPLICIT NONE
CONTAINS
  ! Loads the input data. Reads a two-column text file where the first
  ! line can be a comment starting with #. Allocates memory for a 2D
  ! array of the appropriate size with the columns corresponding to X
  ! and Y and assigns the pointer to the array to the input parameter
  ! `out`.
  !
  ! NOTE: Fortran stores arrays in the column-major order, so Xs (and
  ! Ys) will be contiguous in memory. Detailed comments are not
  ! provided since I/O is not the main object of the assignment.
  !     
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

END MODULE dataio
