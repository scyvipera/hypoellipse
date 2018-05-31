c cvrtop.for    []
      subroutine cvrtop(x, y, r, theta)
c
c-------- bruce julian
c
c-------- cvrtop - convert from rectangular to polar coordinates
c
c (output - may overlay x, y)
c
c-------- standard fortran funct. required:  atan2
c-------- funct. required:  hypot
c
      r = hypot(x, y)
      theta = 0.
      if ((y .ne. 0.) .or. (x .ne. 0.)) theta = atan2(y, x)
      return
      end
c
c end cvrtop
