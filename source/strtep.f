c strtep.for    []
      subroutine strtep(c, e, kdx, fstlat,fstlon, nrp, nmax, vez,
     *                   vla, vlo, savor, tp, vhalf, w, shiftmax)
c gather together the first nmax p-times for use in determining starting locatio
c dimensions of tim, x, and y must increase if nmax>10
      include 'params.inc' 
c              c(nsn)       x coordinates of stations wrt first station in list
      real     c(nsn)
c              e(nsn)       y coordinates of stations wrt first station in list
      real     e(nsn)
c              fstlat       latitude of first station in station list
      real     fstlat
c              fstlno       longitude of first station in station list
      real     fstlon
c              kdx(npa)     kdx(phase number) = station number
      integer  kdx(npa)
c              key(npa)     gives order of arrival times
      integer  key(npa)
c              nrp          number of p arrivals
      integer  nrp
c              nmax         maximum number of p arrivals to use
c                           in estimating first trial location
      integer  nmax
c              vez          depth of earthquake
      real     vez
c              vla          latitude of earthquake
      real     vla
c              vlo          longitude of earthquake
      real     vlo
c              savor        origin time - if not 99999., then fix
      real     savor
c              tim(10)      short list of p arrival times
      real     tim(10)
c              tp(npa)      p arrival times
      real     tp(npa)
c              tptmp(npa)   temporary array for tp values
      real     tptmp(npa)
c              vhalf        halfspace velocity
      real     vhalf
c              w(npa)       weight of readings
      real     w(npa)
c              x(10)        x location of staton
      real     x(10)
c              xz           x location of earthquake
      real     xz
c              y(10)        y location of station
      real     y(10)
c              yz           y location of earthquake
      real     yz
c	       shiftmax	    maximum shift of epicenter from closest station
      real     shiftmax
c save initial depth
      vezsave = vez
      do 20 i = 1, nrp
        tptmp(i) = tp(i)
20    continue
c sort tptmp
      call sort(tptmp, key, nrp)
c find the first nmax values
      n = 0
c in case there are not 4 or more p-arrivals and
c in addition there are none with out zero weight, define xz, yz now
      xz = c(kdx(1))
      yz = e(kdx(1))
      do 25 i = 1, nrp
        k = key(i)
        if(w(k) .eq. 0.) goto 25
        n = n + 1
        tim(n) = tp(k)
        x(n) = c(kdx(k))
        y(n) = e(kdx(k))
        if(n .eq. nmax) then
          nuse = nmax
          goto 30
        endif
25    continue
      nuse = n
      if(nuse .lt. 4) then
        if(nuse .ge. 1) then
c         there are not enough p arrivals to compute a starting location,
c         so use the first one with weight not equal to zero.
          xz = x(1)
          yz = y(1)
        endif
        if(savor .eq. 99999.) savor = 0.
cd      print *, 'savor, vhalf = ', savor, vhalf
cd      do 28 i = 1, nuse
cd        print *,  x(i), y(i), i, tim(i), ' 2'
cd28     continue
        goto 50
      endif
30    continue
c
c solve for best halfspace solution, given m p arrival times.  fix origin
c time if savor .ne. 99999.
cd    print *, 'savor, vhalf = ', savor, vhalf
cd    do 40 i = 1, nuse
cd      print *,  x(i), y(i), i, tim(i), ' 1'
cd40   continue
      call halfsp(nuse, tim, savor, vhalf, x, xz, y, yz, vez)

c make sure starting location is within shiftmax km of first station
c if not, put back at location of first station.
      if(sqrt( (x(1) - xz)**2 + (y(1) - yz)**2 ) .gt. shiftmax) then 
        xz = x(1)
        yz = y(1) 
        vez = vezsave 
      endif

c
50    continue
cd    print *, 'computed starting x, y, z, origin time '
cd    ivez = vez + .5
cd    print *,  xz, yz, ivez, savor, ' 3'
c     print *, 'with respect to ', fstlat, fstlon
c
c convert (xz, yz) to (vla, vlo)
      call back(yz, -xz, vla, vlo, fstlat, fstlon)
c     print *, 'starting lat, lon = ', vla, vlo

      return
      end
c end strtep
