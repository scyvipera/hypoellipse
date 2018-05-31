c hyset.for    []
      subroutine hyset(nttab, z1)
c    written by f. klein for hypoinverse
c    modified slightly for hypoellipse - j.c. lahr
c    further improvements - j.a. snoke - january 1991 - spherical earth
c--initialize values for a given depth, z1.
c--uses a condensed and reduced travel time table generated
c--by the program ttgen.
      parameter (ln=3)
c                            the number of linear-grad models allowed
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
c                            tt depth deriv at each distance point
      data scfac/2000./
c--loop over the ln possible models, skipping loop if model not used.
      do 35 md=1,nttab
c--perform some preliminary calcs and interpolations which depend only on
c  depth. do this for each linear grad model once.
      nzm=nz(md)
      nz1m=nz1(md)
      nz2m=nz2(md)
      dz1m=dz1(md)
      dz2m=dz2(md)
      laym=lay(md)
      nd1m=nd1(md)
c--perform depth interpolation first
c--find depth index = index of table entry nearest hypocenter
c--also depth spacing h and fraction of interval x
      temp=nz1m*dz1m
      if ((.not.gz1(md) .or. z1.gt.temp) .and. gd2(md)) then
c--hypo in lower part of table
        i=nz1m+(z1-temp)/dz2m+1.5001
        if (i.ge.nzm) i=nzm-1
        if (i.lt.nz1m+2) i=nz1m+2
        h=dz2m
        x=(z1-temp)/dz2m-(i-nz1m-1)
        i1 = nz1m + (z1-temp)/dz2m + 1.0001
        if (i1.ge.nzm) i1 = nzm - 1
        y = (z1 - temp)/dz2m - (i1 - nz1m - 1)
      else
c--hypo in upper part of table
        i=z1/dz1m+1.5001
        if (i.gt.nz1m) i=nz1m
        if (i.lt.2) i=2
        h=dz1m
        x=z1/dz1m-(i-1)
        i1 = z1/dz1m + 1.0001
        y = z1/dz1m - (i1 - 1)
      end if
c--find exact velocity at hypocenter
      l = 1
      do 20 k=1,laym
      if (d(k,md).lt.z1) l=k
20    continue
      vh(md)=vel(laym,md)
      if (l.lt.laym) vh(md)=vel(l,md)+(vel(l+1,md)-vel(l,md))
     *   *(z1-d(l,md))/(d(l+1,md)-d(l,md))
c--interpolate dist at which a horizontal ray emerges
      dhrz(md)=(kdhr(md,i1)+y*(kdhr(md,i1+1)-kdhr(md,i1)))*.1
c--depth interpolation
      if (z1.le.temp+dz2m*nz2m) then
c--use 3 point interpolation
        ca=x*.5*(x-1.)
        cb=1.-x**2
        cc=x*.5*(x+1.)
        da=(x-.5)/h
        db=-2.*x/h
        dc=(x+.5)/h
      else
c--use linear extrapolation
        ca=0.
        cb=1.-x
        cc=x
        da=0.
        dc=1./h
        db=-dc
        dhrz(md)=1000.
      end if
c--interpolate tt and its depth derivative for all distance grid points
      temp=nd1m*dd1(md)
      do 30 j=1,nd(md)
      dx=(j-1)*dd1(md)
      if (j.gt.nd1m+1) dx=temp+(j-nd1m-1)*dd2(md)
      tz(md,j)=(ca*kt(md,i-1,j)+cb*kt(md,i,j)
     * +cc*kt(md,i+1,j)+32000.)/scfac+dx*redv(md)
30    dtz(md,j)=(da*kt(md,i-1,j)+db*kt(md,i,j)+dc*kt(md,i+1,j))/scfac
35    continue
      return
      end
c end hyset
