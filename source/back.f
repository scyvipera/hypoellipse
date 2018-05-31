c back.for   []
      subroutine back (delat, delon, newlat, newlon, slat, slon)
c
c-------- back - calculate geocentric coordinates of secondary point from
c            step in latitude (km) and longitude(km)
c
c input:  delat     change in earthquake latitude in km (northward positive)
c         delon     change in earthquake longitude in km (westward positive)
c output: newlat    new earthquake geocentric latitude in radians
c         newlon    new earthquake longitude in radians
c
      real newlat,newlon
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      parameter (equrad = 6378.2064)
      parameter (polrad = 6356.5838)
      parameter (flat = (equrad - polrad)/equrad)
c
      st0 = cos(slat)
      ct0 = sin(slat)
      call cvrtop(delat,delon,delta,az1)
      if (az1 .lt. 0.0) az1 = az1 + twopi
c use approximation of local radius for derivative of surface
c     distance with geocentric latitude.
c  more accurate formulation would be:
c      drdth = -r**3 * cos(alat)*sin(alat)*( (1.-flat)**(-2) - 1. )/a**2
c      dsd = sqrt(drdth**2 + r**2)
      radius = (cos(slat)**2/equrad**2 + sin(slat)**2/polrad**2)**(-.5)
      sdelt = sin(delta/radius)
      cdelt = cos(delta/radius)
      cz0 = cos(az1)
      ct1 = st0*sdelt*cz0+ct0*cdelt
      call cvrtop(st0*cdelt-ct0*sdelt*cz0, sdelt*sin(az1), st1, dlon)
      newlat = atan2(ct1, st1)
      newlon = slon + dlon
      if (abs(newlon) .gt. pi) newlon = newlon - sign(twopi, newlon)
      return
      end
c end back
