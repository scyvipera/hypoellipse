c hytab.for    []
      subroutine hytab(md, dx, travti, ain, dtdd, dtdh, vt, vs)
c    written by f. klein for hypoinverse
c    modified slightly for hypoellipse - j.c. lahr
c    further improvements - j.a. snoke - january 1991 - spherical earth
c--md is model for this station
c--dx is station distance
      parameter (ln=3)
c                            the number of linear-grad models allowed
c
c  Changed nlyr from 12 to 20 on 2/9/2000 jcl
      parameter (nlyr=20)
      character modnam*20
      common /mc/ modnam(ln)
c                            model name or label
      common /m/ lay(ln)
c                            number of layers or v-d points
      common /m/ d(nlyr,ln)
c                            depth to layer top or velocity point
      common /m/ vel(nlyr,ln)
c                            point velocity
      common /m/ thk(nlyr,ln)
c                            thickness of homogeneous layer
      common /m/ vsq(nlyr,ln)
c                            squared velocity of homogeneous layer
      common /m/ modtyp(ln)
c                            mdl type (-1=undef, 0=grad, 1=homo layer)
c--data used only for linear gradient travel time tables
      integer kt
      logical gd1,gd2,gz1,gz2
      common /m/ redv(ln)
c                            one over the reducing velocity
      common /m/ nz(ln)
c                            number of depth grid points
      common /m/ nz1(ln),dz1(ln),nz2(ln),dz2(ln)
c                            depth grid params
      common /m/ nd(ln)
c                            number of distance grid points
      common /m/ nd1(ln),dd1(ln),nd2(ln),dd2(ln)
c                            dist grid params
      common /m/ gd1(ln),gd2(ln),gz1(ln),gz2(ln)
c                            grid flags
      common /m/ kdhr(ln,28)
c                            for each model (ln) and for each depth,
c                            the distance at which horizontal ray emerges
c                            (in units of 0.1 km)
      common /m/ kt(ln,28,42)
c                            the travel times (up to 28 z's & 42 dist's)
      common /tlook/ vh(ln)
c                            velocity at hypocenter
      common /tlook/ dhrz(ln)
c                            dist at which horiz ray emerges
      common /tlook/ tz(ln,42)
c                            tt at each distance point
      common /tlook/ dtz(ln,42)
c                             tt depth deriv at each distance point
c--now find travel time, dtdr, dtdz, and angle of emergence for
c--start distance interpolation
      ndm=nd(md)
      nd1m=nd1(md)
      nd2m=nd2(md)
      dd1m=dd1(md)
      dd2m=dd2(md)
      temp=nd1m*dd1m
c--find distance index = index of table entry nearest the station dist
c--also distance spacing h and fraction of interval x
      if ((.not.gd1(md) .or. dx.gt.temp) .and. gd2(md)) then
c--hypo in far part of table
        j=nd1m+(dx-temp)/dd2m+1.5001
        if (j.ge.ndm) j=ndm-1
        if (j.lt.nd1m+2) j=nd1m+2
        h=dd2m
        x=(dx-temp)/dd2m-(j-nd1m-1)
      else
c--hypo in near part of table
        j=dx/dd1m+1.5
        if (j.gt.nd1m) j=nd1m
        if (j.lt.2) j=2
        h=dd1m
        x=dx/dd1m-(j-1)
      end if
c--distance interpolation
      if (dx.le.temp+dd2m*nd2m) then
c--use 3 point interpolation
        anin=x*.5*(x-1.)
        cb=1.-x**2
        cc=x*.5*(x+1.)
        da=x-.5
        db=-2.*x
        dc=x+.5
      else
c--use linear extrapolation
        anin=0.
        cb=1.-x
        cc=x
        da=0.
        dc=1.
        db=-1.
      end if
c--interpolate tt and its 2 derivatives
      travti=anin*tz(md,j-1)+cb*tz(md,j)+cc*tz(md,j+1)
      dtdh  =anin*dtz(md,j-1)+cb*dtz(md,j)+cc*dtz(md,j+1)
      dtdd=(da*tz(md,j-1)+db*tz(md,j)+dc*tz(md,j+1))/h
      if (dtdd.lt.0.) dtdd=0.
c--calc emergence angle
c-- for spherical models, the following equation would be more accurate:
c--   anin=vh(md)*dtdd*radius/(radius-depth)
c   however in most situations this change will be quite small.
      anin=vh(md)*dtdd
      if (anin.gt..99999) anin=.99999
      ain=57.29578*asin(anin)
      if (dx.lt.dhrz(md)) ain=180.-ain
      if (dx.ge.dhrz(md)) anin = -anin
      vt = vel(1, md)
      vs = vh(md)
      return
      end
c end hytab
