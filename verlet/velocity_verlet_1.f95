! Run the program using:
! gfortran -Wall -Wextra -fcheck=all velocity_verlet.f95 -o velocity_verlet
! ./velocity_verlet
PROGRAM velocity_verlet

  IMPLICIT NONE	
  integer, parameter :: n_steps = 600
  double precision, parameter :: mass = 1.0d0
  double precision, parameter :: tau = 0.2d0
  double precision, parameter :: fx = 0.0d0
  double precision, parameter :: fy = 0.1d0
  double precision, parameter :: fz = 0.0d0
  integer :: k
  double precision :: time
  double precision :: x, y, z
  double precision :: vx, vy, vz

  ! double precision stores real numbers with higher precision
  ! than the default real type (typically 64 bits instead of 32).
  ! It provides approximately 15-16 significant decimal digits.
  ! The suffix d0 indicates a double-precision numerical literal.
  ! It is useful in scientific computing to reduce rounding errors.

  ! Initial Positions
  x = 0.0d0
  y = 0.0d0
  z = 0.0d0

  ! Initial Velocities
  vx = 0.0d0
  vy = 0.0d0
  vz = 0.0d0

  ! Column headings
  print *, "Step", "Time", "X", "Y", "Z"

  ! Velocity Verlet Algorithm
  do k = 1, n_steps
    x = x + vx*tau + (fx/(2.0d0*mass))*tau**2
    y = y + vy*tau + (fy/(2.0d0*mass))*tau**2
    z = z + vz*tau + (fz/(2.0d0*mass))*tau**2

    vx = vx + (fx/mass)*tau
    vy = vy + (fy/mass)*tau
    vz = vz + (fz/mass)*tau

    time = k*tau
    
    print *, k, time, x, y, z

  end do

end program velocity_verlet
