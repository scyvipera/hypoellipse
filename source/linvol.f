c linvol.for    []
      subroutine linvol(delta, depthsv, stzsv, ak, voa, aha, vi, t,
     *  ain, dtdd, dtdh)
c model:  linearly increasing velocity over a halfspace
c bill gawthrop  (modified by j.c. lahr  3/6/91)
c     delta  epicentral distance (km)
c     depthsv eq depth (km)
c     stzsv   station depth (km)
c     ak     velocity gradient
c     voa    v at surface
c     aha    thickness of linear layer (depth to half space)
c     vi     velocity of half space
c     t      travel time
c     ain    angle of incidence
c     dtdd   partial derivative of tt wrt distance
c     dtdh   partial derivative of tt wrt depth
      integer punt
      common /punt/ punt 
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      logical flip
      dist = sqrt(delta**2 + (depthsv - stzsv)**2)
      if (dist .lt. .0001) then
c       distance less than 10 cm
        t = 0.0
        dtdd = 1./(voa + ak*depthsv)
        dtdh = 1./(voa + ak*depthsv)
        ain = 90.
        return
      endif
      if((stzsv .ge. aha - .0001) .and.
     *  (depthsv .ge. aha - .0001)) then
c both eq and station are within (or within 10 cm) of the
c constant velocity halfspace
c       write(punt, *) 'eq and station both in halfspace'
        t = dist/vi
        dtdd = delta/(dist*vi)
        dtdh = (depthsv - stzsv)/(dist*vi)
        if (stzsv .eq. depthsv) then
          ain = 90.0
        else
          if(stzsv .lt. depthsv) then
c           upward
            ain = 180 + deg*atan(delta/(stzsv - depthsv))
          else
c           downward
            ain = deg*atan(delta/(stzsv - depthsv))
          endif
        endif
        return
      endif
      if(depthsv .ge. stzsv) then
        flip = .false.
        deptha = depthsv
        stza = stzsv
      else
        flip = .true.
        deptha = stzsv
        stza = depthsv
      endif
c if near boundary, then move to boundary
      if(abs(deptha - aha) .le. .0001) deptha = aha
      if(abs(stza - aha) .le. .0001) stza = aha
c set vsource
      if(flip) then
        if(stza .le. aha) then
          vsource = voa + stza*ak
        else
          vsource = vi
        endif
      else
        if(deptha .le. aha) then
	  vsource = voa + deptha*ak
	else
	  vsource = vi
	endif
      endif
c create modified model with station at surface
      ah = aha - stza
      vo = voa + ak*stza
      depth = deptha - stza
      stz = 0.0
      if (depth .le. ah) then
c source: within the layer
c       write(punt, *) ' source: within the layer'
        vz = vo + depth*ak
c       compute time of direct wave
        call tatime(delta,depth,vo,ak,ah,tat,aina)
c       compute time of refracted wave
        call tbtime(delta,depth,vo,ak,ah,vi,tbt,ainb)
        if (tbt .lt. tat) then
c         refracted wave is faster
          t = tbt
          if(flip) then
c           downward ray
            anin = vsource*sin(ainb)/vz
            ain = asin(anin)
          else
c           downward ray
            anin = sin(ainb)
            ain = ainb
          endif
        else if (tat .eq. 900000.) then
c         neither direct nor refracted wave could be computed!
          print *,
     *      'neither direct nor refracted wave could be computed!'
          print *, 'so stop'
	  print *, 'delta,depth,vo,ak,ah,vi'
	  print *, delta,depth,vo,ak,ah,vi
          stop '1: abort from linvol'
        else
c         direct wave is faster
          t = tat
          if(flip) then
c           downward ray
            anin = vsource*sin(aina)/vz
            ain = asin(anin)
          else
c           upward or downward ray
            anin = sin(aina)
            ain = aina
          endif
        endif
      else
c source: within the halfspace
c       write(punt, *) ' source: within the halfspace'
c	write(punt, *) 'delta,depth,vo,ak,ah,vi'
c	write(punt, *)  delta,depth,vo,ak,ah,vi 
        call tdtime(delta,depth,vo,ak,ah,vi,td,aina)
        if (td.eq.900000.) then
c         direct wave could not be computed!
          print *,
     *      'direct wave could not be computed, so stop!'
          stop '2: abort from linvol'
        else
c         direct wave from halfspace
          t = td
          if(flip) then
c           downward ray
            anin = vsource*sin(aina)/vi
            ain = asin(anin)
          else
c           upward ray
            anin = sin(aina)
            ain = aina
          endif
        endif
      endif
      dtdd = anin/vsource
      dtdh = -cos(ain)/vsource
      ain = deg*ain
      return
      end
c end linvol
