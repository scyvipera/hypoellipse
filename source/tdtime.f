c tdtime.for    []
      subroutine tdtime (delta, depth, vo, ak, ah, vi, tdt, toa1)
c
c model:  linearly increasing velocity over a halfspace
c source: within the halfspace
c path:   direct wave
c bill gawthrop  (modified by j.c. lahr  3/6/91)
c
c   this sub determines the traveltime of a ray from an origin
c   below the moho in constant velocity material passing thru a crust
c   with velocity increasing linearly with depth
c   bill gawthrop
c     xo   delta
c     yo   depth
c     vo   v at surface
c     ak   velocity gradient
c     ah   thickness of linear layer (depth to half space)
c     vi   velocity of half space
c     tdt
c     toa1
      double precision xp, xp2, xp3, a, b, c, d, xt1, xt2, el
      double precision xpold, xt, xr, chx, denom, xpa, xpb
      double precision r2, r
      integer punt
      common /punt/ punt
c     write(punt, *) 'in tdtime: delta, depth, vo, ak, ah, vi'
c     write(punt, *)  delta, depth, vo, ak, ah, vi 
      if ((delta .lt. .001) .or. (delta/depth .lt. .001)) then
        tdt = (depth - ah)/vi + alog((vo + ak*ah)/(vo))/ak
        toa1 = 3.1415926
c	write(punt, *) 'tdt, toa1 ', tdt, toa1
        return
      endif
      a = (vi/ak)**2
      b = a - (vo/ak)**2
      c = a - (ah + vo/ak)**2
      d = a*(depth - ah)**2
ctest
      iwrt = 0
      chx = 0.d0
    6 continue
ctest
      xp = .8d0*delta
      do 15 i = 1, 25
        xpold = xp
        xp2 = xp*xp
        xp3 = xp2*xp
        xt1 = dsqrt(d/xp2 + b)
        xt2 = dsqrt(d/xp2 + c)
        xt = xp + xt1 - xt2
        xr = xt - delta
ctest
        if (iwrt .eq. 1) write(punt, 100)
     *    i, delta, depth, xr, xpold, chx,
     *    xp, xt1, xt2, xp3
100     format(i5, 8f13.7, f13.2)
ctest
        if (dabs(xr).lt..0001d0) then
          xc = delta - xt1
          r2 = a + d/xp2
          r = dsqrt(r2)
          ta = r2 - delta*xp + delta*xc + xp*xc - xc*xc
          tb = r*delta - r*xp
          el = dsqrt((xp2 + (depth - ah)**2)*1.d0)
c	  write(punt, *) 'xc, r2, r, ta, tb, el ', xc, r2, r, ta, tb, el
          tdt = alog((ta + tb)/(ta - tb))/(2.*ak) + el/vi
          toa1 = 1.5707963 + acos(xp/el)
c	  write(punt, *) 'tdt toa1 ', tdt, toa1
c
          return
        endif
        denom = xp3*xt1
        if (denom .eq. 0.d0) then
          xp = -1.d0
        else
          xpa = d/(xp3*xt1)
          xpb = d/(xp3*xt2)
          denom = 1.d0 - xpa + xpb
          if (denom .eq. 0.) then
            xp = -1.d0
          else
            chx = xr/(1.d0 - xpa + xpb)
            xp = xp - chx
          endif
        endif
   10   continue
        if (xp .lt. 0.000001) xp = xpold/4.
   15 continue
ctest
      tdt = 900000.
      if(iwrt .eq. 1) return
      write(punt, 200) delta, depth, vo, ak, ah, vi, tdt, toa1,
     * xr, xpold, chx, xp, xt1, xt2, xp3
  200 format(' sub. tdtime convergence problem', /,
     * '         delta           z          vo           ak
     *             ah            vi         tdt         toa1', /,
     * 5x, 8f13.7, /,
     *  '    xr          xpold          chx          ',
     *  ' xp          xt1          xt2          xp3', /, 5x, 7f13.7)
      iwrt = 1
      goto 6
ctest
      end
c end tdtime
