r1=[-2 -2]
r2=[-2 3]
r3=[1 4]
r1/norm(r1)^3+r2/norm(r2)^3 + r3/norm(r3)^3
norm(r1/norm(r1)^3+r2/norm(r2)^3 + r3/norm(r3)^3)


GPU accelerate this.
populate the x direction of 1111x1111 first quadrant
do the same for the y direction
use mirroring and rotating to replicate it for the second quadrant.


{add the like components of the first and second quadrants.
round each to the near integer coordinate
add that contribution back
compare to 3e-12
shift the second quadrant
repeat}