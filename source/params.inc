c     HYPOELLIPSE params.inc for the SUN version
c
c     (npa - 1) is the maximum number of phases (P plus S plus S-P) for
c               a single earthquake.
      parameter (npa = 1501)

c     nsn is the maximum number of stations that can be included in the
c               station list.
      parameter (nsn = 1024)

c     lmax is the maximum number of VELOCITY records allowed to define
c               the velocity models.
      parameter (lmax = 96)

c     mmax is the maximum number of velocity models that can be specified.
c     remember that a model with variable vp/vs ratio counts as two.
      parameter (mmax = 25)
      parameter (mmaxp3 = mmax+3)

c     lmmax is the maximum number of records that can be used to define
c     a single velocity model.  A model defined by N records will have
c     N-1 layers over a half space.
      parameter (lmmax = 20) 

c     lmax2 and narray are other array dimensions computed from lmax
      parameter (lmax2 = lmax + 2)
      parameter (narray = 2*lmmax*lmax)

