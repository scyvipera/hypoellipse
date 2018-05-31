c fold2.for    []
      subroutine fold2(alat,alon,la,ins,ala,lo,iew,alo)
c
c-------- given geographic coordinates compute geocentric lat and lon
c
c input:  la     degree portion of latitude in degrees
c         ins    n for north, s for south
c         ala    minutes portion of latitude
c         lo     degree portion of longitude
c         iew    e for east, w for west
c         alo    minutes portion of longitude
c output: alat   geocentric latitude in radians
c         alon   longitude in radians
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      parameter (equrad = 6378.2064)
      parameter (polrad = 6356.5838)
      parameter (flat = (equrad - polrad)/equrad)
      parameter (c1 = (1.0 - flat)**2)
      parameter (c2 = halfpi*(1.0/c1 - 1.0))
c
      character*1 ins, iew, dnstrg
c
      alat = (la + ala*1.6666667e-2)*rad
c ggtogc - convert from geographic to geocentric latitude
      if (halfpi-abs(alat) .ge. 0.02) goto 201
         alat = alat/c1-sign(c2,alat)
         goto 202
  201    alat = atan(c1*tan(alat))
  202    continue
      if (dnstrg(ins) .eq. 's') alat = -alat
      alon = (lo + alo*1.6666667e-2)*rad
      if (dnstrg(iew) .eq. 'e') alon = -alon
      return
      end
c end fold2
