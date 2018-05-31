c hycrt.for    []
      subroutine hycrt (mod, itable, vpvs)
c    written by f. klein for hypoinverse
c    modified slightly for hypoellipse - j.c. lahr
c    further improvements - j.a. snoke - january 1991 - spherical earth
c--reads a travel time table for a linear gradient crust model
c--called by hypoinv
c--data used only for linear gradient travel time tables
c--ln is number of models
      parameter (ln=3)
c  Changed nlyr from 12 to 20 on 2/9/2000 jcl
      parameter (nlyr=20)
      integer punt
      common /punt/ punt
      character modnam*20
      common /mc/ modnam(ln)
c                            model name or label
      common /m/ lay(ln)
c                            number of layers or v-d points
      common /m/ d(nlyr,ln)
c                            depth to layer top or velocity point
      common /m/ vel(nlyr,ln)
c                            layer or point velocity
      common /m/ thk(nlyr,ln)
c                            thickness of homogeneous layer
      common /m/ vsq(nlyr,ln)
c                            squared velocity of homogeneous layer
      common /m/ modtyp(ln)
c                            mdl type (-1=undef, 0=grad, 1=homo layer)
c                            the number of linear-grad models allowed
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
c--identify the model mod as a linear gradient table
      modtyp(mod)=0
c
c--read header info
      rewind(itable)
      read (itable,1000) modnam(mod),lay(mod),redv(mod),vpvs,radius
1000  format (a20, i5, 2f10.5, f10.3)
      write(punt, '(1x, a20, 5x, a, f10.5)')
     *  modnam(mod), 'vp/vs =', vpvs
c
c--read depths & velocities of model
      read (itable,1001) (d(i,mod),i=1,lay(mod))
      read (itable,1001) (vel(i,mod),i=1,lay(mod))
1001  format (3x, 15f7.2)
c
c--read distance & depth grid info
      read (itable,1003) dd1(mod),nd1(mod),dd2(mod),nd2(mod)
      read (itable,1003) dz1(mod),nz1(mod),dz2(mod),nz2(mod)
1003  format (3x, 2(f10.4, i5))
      write(punt, '('' z '', 15f7.2)') (d(i, mod), i = 1, lay(mod))
      write(punt, '('' v '', 15f7.2)') (vel(i, mod), i = 1, lay(mod))
      write(punt, '('' dd '', 2(f10.4, i5))')
     *  dd1(mod), nd1(mod), dd2(mod), nd2(mod)
      write(punt, '('' dz '', 2(f10.4, i5))')
     *  dz1(mod), nz1(mod), dz2(mod), nz2(mod)
      nd(mod)=nd1(mod)+nd2(mod)+1
      nz(mod)=nz1(mod)+nz2(mod)+1
c
c--read reduced travel times, grouped by depth
      do 20 j=1,nz(mod)
      read (itable,1004) kdhr(mod,j)
1004  format (20x,i10)
20    read (itable,1005) (kt(mod,j,i),i=1,nd(mod))
1005  format (15i8)
      gd1(mod)=nd1(mod).ne.0 .and. dd1(mod).ne.0
      gd2(mod)=nd2(mod).ne.0 .and. dd2(mod).ne.0
      gz1(mod)=nz1(mod).ne.0 .and. dz1(mod).ne.0
      gz2(mod)=nz2(mod).ne.0 .and. dz2(mod).ne.0
      return
      end
c end hycrt
