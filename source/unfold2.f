c unfold2.for    []
      subroutine unfold2(alat,alon,la,ins,ala,lo,iew,alo)
c     unfold2
c
c-------- given geocentric lat and lon compute geographic coordinates
c            suitable  for printing
c
c input and output definition just reverse of entry fold2
c
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
      character*1 ins, iew
c
c
c convert from geocentric lat and lon to geographic
c
c     alat     geocentric lat (radians)
c     alon                lon (radians)
c     la,ins,ala          lat in deg and min
c     lo,iew,alo          lon in deg and min
c
c gctogg - convert from geocentric to geographic latitude
      if (halfpi-abs(alat) .ge. 0.02) goto 403
         blat = c1*(alat + sign(c2,alat))
         goto 404
  403    blat = atan(tan(alat)/c1)
  404 continue
      ins = 'n'
      iew = 'w'
      ala1 = blat*deg
      la = ala1
      ala = abs(ala1 - la)*60.
      la = iabs(la)
      if (ala1 .lt. 0.0) ins = 's'
      alo1 = alon*deg
      lo = alo1
      alo = abs(alo1 - lo)*60.
      lo = iabs(lo)
      if (alo1 .lt. 0.0) iew = 'e'
      return
      end
c end unfold2
