c tbtime.for    []
      subroutine tbtime (delta,depth,vo,ak,ah,vi,tbt,toa2)
c model:  linearly increasing velocity over a halfspace
c source: within the layer
c path:   refracted wave
c   bill gawthrop  (modified by j.c. lahr  3/6/91)
c     vo   v at surface
c     ak   velocity gradient
c     ah   thickness of linear layer (depth to half space)
c     vi   velocity of half space
      character*55 mess(3)
      integer punt
      common /punt/ punt
      common /logfil/ logfil
      a = (depth + vo/ak)**2
      b = (ah + vo/ak)**2
      c = (vo/ak)**2
      r = vi/ak
      r2 = r*r
      if(b .le. r2) then
        xc1 = sqrt(r2 - a)
        xi = xc1 - sqrt(r2 - b)
        xc2 = sqrt(r2 - c)
        x2 = xc2 - xc1 + xi
        if ((delta - xi - x2) .gt. 0.0) then
          d = r2 + xi*xc1 - xc1*xc1
          e = r2 + x2*xc2 - xc2*xc2
          tbt = alog(
     *     (d + r*xi)*(e + r*x2)/((d - r*xi)*(e - r*x2))
     *          )/(2.*ak) + (delta - xi - x2)/vi
          toa2 = acos(xc1/r)
        else
          tbt = 900000.
        endif
      else
        mess(1) =  ' for a linear increase over halfspace model the'
        mess(2) =  ' velocity at the base of the linear model is '
        mess(3) =  ' greater than the half space velocity, so stop.'
        write(punt, '(a, /, a, /, a)') mess
        write(logfil, '(a, /, a, /, a)') mess
        stop 'abort from tbtime'
      endif
      return
      end
c end tbtime
