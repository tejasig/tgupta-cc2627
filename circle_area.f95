PROGRAM circle_area
  IMPLICIT NONE
  real :: diameter, radius, area
  real, parameter :: pi = 3.14159265 ! , parameter is used as pi is a constant and we shouldn't overwrite the value of pi

  print *, "Enter the diameter of the circle: "
  read *, diameter

  radius = diameter/2
  area = pi * radius**2
  
  print *, "Area of circle : ", area

END PROGRAM circle_area
