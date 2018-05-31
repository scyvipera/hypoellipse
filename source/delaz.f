c delaz.for    []
      subroutine delaz(eqlat, eqlon, dekm, dedeg, az0, slat, slon)
c
c-------- delaz - calculate the distance in km (approx equal to geocentric
c      distance times local radius), and azimuths in radians
c
c input:  slat     station geocentric latitude in radians
c         slon     station longitude in radians
c         eqlat    earthquake geocentric latitude in radians
c         eqlon    earthquake longitude in radians
c output: dekm     distance from earthquake to station in kilometers
c         dedeg    distance from earthqauke to station in degrees
c         az0      azimuth from earthquake to station measured clockwise
c                     from north in degrees
c
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      parameter (equrad = 6378.2064)
      parameter (polrad = 6356.5838)
      parameter (flat = (equrad - polrad)/equrad)
c
      st0 = cos(eqlat)
      ct0 = sin(eqlat)
c use approximation of local radius for derivative of surface
c     distance with geocentric latitude.
c  more accurate formulation would be:
c      drdth = -r**3 * cos(slat)*sin(slat)*( (1.-flat)**(-2) - 1. )/a**2
c      dsd = sqrt(drdth**2 + r**2)
      radius = (cos(eqlat)**2/equrad**2 +
     *sin(eqlat)**2/polrad**2)**(-.5)
      ct1 = sin(slat)
      st1 = cos(slat)
      sdlon = sin(eqlon-slon)
      cdlon = cos(eqlon-slon)
      cdelt = st0*st1*cdlon+ct0*ct1
      call cvrtop(st0*ct1-st1*ct0*cdlon, st1*sdlon, sdelt, az0)
      dedeg = atan2(sdelt, cdelt)*deg
      dekm = radius*atan2(sdelt, cdelt)
      if (az0 .lt. 0.0) az0 = az0 + twopi
      if (az0 .ge. twopi) az0 = az0 - twopi
      az0 = az0*deg
c calculation of back azimuth if needed
c      call cvrtop(st1*ct0 - st0*ct1*cdlon, (-sdlon)*st0, sdelt, az1)
c      if (az1 .lt. 0.0) az1 = az1 + twopi
      return
      end
c end delaz
