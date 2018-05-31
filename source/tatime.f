c tatime.for    []
      subroutine tatime (delta,depth,vo,ak,ah,tat,toa1)
c model:  linearly increasing velocity over a halfspace
c source: within the layer
c path:   direct wave
c   bill gawthrop  (modified by j.c. lahr  3/6/91)
c     vo   v at surface
c     ak   velocity gradient
c     ah   thickness of linear layer (depth to half space)
      if (delta.ge. 0.1) then
        a = (vo/ak)**2
        b = (vo/ak + depth)**2
        xc = (delta*delta + a - b)/(2.*delta)
        r = sqrt(xc*xc + b)
        toa1 = acos(xc/r)
        if ((r - vo/ak).gt.ah.and.toa1.le.1.57) then
          tat = 900000.
          return
        endif
        c = r*r + delta*xc - xc*xc
        tat = alog((c + r*delta)/(c - r*delta))/(2.*ak)
      else
        tat = alog((vo + depth*ak)/(vo))/ak
        toa1 = 3.1415926
      endif
      return
      end
c end tatime
