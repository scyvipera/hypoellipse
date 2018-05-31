c halfsp.for    []
      subroutine halfsp(m, t, tz, vh, x, xz, y, yz, zz)
      common /logfil/ logfil
c solve for best halfspace solution, given m p arrival times and the
c origin time.
c              mx               maximum number of equations (m .eq. mx)
      parameter (mx = 10)
c              a(6)             storage mode 1 version of sum of squares
      real     a(6)
c              m                (input) number of p arrival times
      integer  m
c              c(4, mx)         normal equation coeficients and constants
      real     c(4, mx)
c              ev(3, 3)         eigenvector matrix
      real     ev(3, 3)
c              r(3)             normal equation constants in rotated system
      real     r(3)
c              s(3)             eignevalues
      real     s(3)
c              t(mx)            (input) arrival time at station m
      real     t(mx)
c              tz               origin time
      real     tz
c              vh               halfspace velocity
      real     vh
c              v(4, 4)          symetric tensor  - upper 3x3 to be diagonalized
      real     v(4, 4)
c              x(mx)            (input) x coordinate of station m
      real     x(mx)
c              xyt(3)           eq coordinates in original system
      real     xyt(3)
c              xytr(3)          eq coordinates in rotated system
      real     xytr(3)
c              xz               (output) x coordinate of earthquake
      real     xz
c              y(mx)            (input) y coordinate of station m
      real     y(mx)
c              yz               (output) y coordinate of earthquake
      real     yz
c              zz               (output) z coordinate of earthquake
      real     zz
c
      if (m .gt. mx) then
        print *, 'halfsp can not have more than ', mx, ' equations,'
        print *, 'so ', m, ' is too many.'
        stop
      endif
      n = 3
      vsq = vh*vh
      nfix = 1
      if (tz .eq. 99999.) nfix = 0
c
c set up cooeficients of equations, c
c
cd    print *, 'original equations for ', m, ' arrival times:'
      do 20 i = 1, m-1
        c(1, i) = x(m) - x(i)
        c(2, i) = y(m) - y(i)
        if (tz .eq. 99999.) then
c         in this case, tz is unknown
          c(3, i) = -vsq*(t(m) - t(i))
          c(4, i) = ( x(m)*x(m) + y(m)*y(m) - t(m)*t(m)*vsq
     *             -x(i)*x(i) - y(i)*y(i) + t(i)*t(i)*vsq )/2.
        else
c         in this case, tz is given
          c(3, i) = ( x(m)*x(m) + y(m)*y(m) - t(m)*t(m)*vsq
     *             -x(i)*x(i) - y(i)*y(i) + t(i)*t(i)*vsq )/2.
     *             +vsq*( t(m) - t(i) )*tz
          c(4, i) = 0.0
        endif
cd      print *, i, (c(j, i), j = 1, 4)
20    continue
c
c compute sum of squares matrix from coeficients
c
      do 25 j = 1, 4-nfix
        do 25 k = j, 4-nfix
          v(j, k) = 0.0
          do 23 i = 1, m-1
23          v(j, k) = v(j, k) + c(j, i)*c(k, i)
25    v(k,j) = v(j,k)
cd    print *, 'normal equations:'
cd    do 26 j = 1, 4-nfix
cd      print *, (v(j, k), k = 1, 4-nfix)
cd26   continue
c
c compute determinant
c
cd    det = deter(v, 4, n-nfix)
cd    print *, 'determinant = ', det
c
c compute eigenvalues and eigenvectors of nxn tensor v
c ev is the nxn eigenvector tensor
      call eigen1(a, 3, 4, n-nfix, ev, s, v, 0.0)
c
cd    write (logfil, *) ' eigenvectors'
cd    do 45 i = 1, 3
cd      write (logfil, *) (ev(i, j), j = 1, 3)
cd45   continue
cd    write (logfil, *) ' eigenvalues'
cd    write (logfil, *) s
c
c
c compute response vector in rotated coordinates
c
      do 30 j= 1, n-nfix
        r(j) = 0.0
        do 30 k = 1, n-nfix
          r(j) = r(j) + ev(k, j)*v(k, 4-nfix)
30    continue
cd    print *, 'response vector in rotated coordinates'
cd    print *, r
c
c compute the eq location in rotated system
c
      do 50 i = 1, n-nfix
        if(s(i) .gt. .000001) then
          xytr(i) = r(i)/s(i)
        else
          xytr(i) = 0.0
        endif
50    continue
c
c rotate location back into original coordinates
c
      do 55 j = 1, n-nfix
        xyt(j) = 0.0
        do 55 k = 1, n-nfix
55        xyt(j) = xyt(j) + ev(j,k)*xytr(k)
c
c define output variables
c
      xz = xyt(1)
      yz = xyt(2)
      if (tz .eq. 99999.) tz = xyt(3)
      arg =  vsq*(t(m) - tz)**2 - (x(m) - xz)**2 - (y(m) - yz)**2
      if (arg .ge. 0.) then
        zz = sqrt (arg)
      else
cd      print *, 'arg = ', arg, ' so z set to zero'
        zz = 0.
      endif
cd    print *, 'xz        yz        zz          tz'
cd    print *, xz, yz, zz, tz
c
      return
      end
c end halfsp
