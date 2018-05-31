c line3.for    []
      subroutine line3(nr, p, s, pwt, swt, test,
     *                         vpvs1, sint1, orig1, se1, ses, oses,
     *                         vpvs2, sint2, orig2, se2, sep, osep,
     *                         vpvs3, sint3, orig3, se3)
c bi-weighted regression:
c assume p and s both have errors.  regress s on p.  (madansky, 1959)
c
      include 'params.inc' 
c calling arguments:
      integer           nr
c                                 ! number of arrival time pairs
      real              p(nr)
c                                 ! p arrival times
      real              s(nr)
c                                 ! s arrival times
      real              pwt(nr)
c                                 ! p weights
      real              swt(nr)
c                                 ! s weights
      real              vpvs1
c                                 ! [output] vpvs ratio   (s vs p)
      real              vpvs2
c                                 ! [output] vpvs ratio   (p vs s)
      real              vpvs3
c                                 ! [output] vpvs ratio   (bi-weight)
      real              sint1
c                                 ! [output] s intercept  (s vs p)
      real              sint2
c                                 ! [output] s intercept  (p vs s)
      real              sint3
c                                 ! [output] s intercept  (bi-weight)
      real              orig1
c                                 ! [output] orig time  (s vs p)
      real              orig2
c                                 ! [output] orig time  (p vs s)
      real              orig3
c                                 ! [output] orig time  (bi-weight)
      real              oses
c                                 ! rms difference between s obs & comp
      real              osep
c                                 ! rms difference between p obs & comp
      real              se1
c                                 ! [output]standard error (s vs p)
      real              se2
c                                 ! [output]standard error (p vs s)
      real              se3
c                                 ! [output]standard error (bi-weight)
c
c                                 ! sep & ses are not allowd to go below test(29
      real              sep
c                                 ! [output]s.e. of p used in s.e. of vp/vs (p v
      real              ses
c                                 ! [output]s.e. of s used in s.e. of vp/vs (s v
      real              test(50)
c                                 ! test parameters
c
      double precision  dubl
c                                 ! statement function
      double precision  arg
c                                 ! variance
      double precision  dslop1
c                                 ! vpvs ratio (s vs p)
      double precision  dslop2
c                                 ! vpvs ratio (p vs s)
      double precision  eqwt(npa)
c                                 ! equation weight
      double precision  dorig1
c                                 ! origin (s vs p)
      double precision  dorig2
c                                 ! origin (p vs s)
      double precision  dorig3
c                                 ! origin (bi-weight)
      double precision  dp(npa)
c                                 ! p
      double precision  ds(npa)
c                                 ! s
      double precision  dse1
c                                 ! se (s vs p)
      double precision  dse2
c                                 ! se (p vs s)
      double precision  dse3
c                                 ! se (bi-weight)
      double precision  dsint1
c                                 ! sinter (s vs p)
      double precision  dsint2
c                                 ! sinter (p vs s)
      double precision  dsint3
c                                 ! sinter (bi-weight)
      double precision  dvpvs
c                                 ! increment in vpvs
      double precision  pav
c                                 ! bi-weighted average of p
      double precision  pavp
c                                 ! average value of p weighted with p weights
      double precision  pavs
c                                 ! average value of p weighted with s weights
      double precision  pssum
c                                 ! sum weighted p*s
      double precision  psum
c                                 ! sum p times
      double precision  p2sum
c                                 ! sum weighted p**2
      double precision  sav
c                                 ! bi-weighted average of s
      double precision  savp
c                                 ! average value of s weighted with p weights
      double precision  savs
c                                 ! average value of s weighted with s weights
      double precision  ssum
c                                 ! sum s times
      double precision  s2sum
c                                 ! sum weighted s**2
      double precision  tsum
c                                 ! trial sum to be minimized
      double precision  tsumin
c                                 ! minimum sum
      double precision  vpvsp
c                                 ! current preferred value of vpvs
      double precision  vpvsp1
c                                 ! next preferred value of vpvs
      double precision  vpvst
c                                 ! trial lower limit of vpvs
      double precision  wpsum
c                                 ! bi-weighted sum of p
      double precision  wpsump
c                                 ! sum of p times weighted with p weights
      double precision  wpsums
c                                 ! sum of p times weighted with s weights
      double precision  wssum
c                                 ! bi-weighed sum of s
      double precision  wssump
c                                 ! sum of s times weighted with p weights
      double precision  wssums
c                                 ! sum of s times weighted with s weights
      double precision  wtsum
c                                 ! sum of bi-weights
      double precision  wtsump
c                                 ! sum of p weights
      double precision  wtsums
c                                 ! sum of s weights
c
c initialize some variables
      psum = 0.d0
      ssum = 0.d0
      wpsum = 0.d0
      wpsump = 0.d0
      wpsums = 0.d0
      wssump = 0.d0
      wssums = 0.d0
      wssum = 0.d0
      p2sum = 0.d0
      s2sum = 0.d0
      pssum = 0.d0
      wtsum = 0.d0
      wtsump = 0.d0
      wtsums = 0.d0
c
c main loop to compute vp/vs and origin time
      do 40 i=1,nr
        pi = p(i)
        si = s(i)
        dp(i) = dubl(pi)
        ds(i) = dubl(si)
        wpsump = wpsump + pwt(i)*dp(i)
        wpsums = wpsums + swt(i)*dp(i)
        wtsump = wtsump + pwt(i)
        wssump = wssump + pwt(i)*ds(i)
        wssums = wssums + swt(i)*ds(i)
        wtsums = wtsums + swt(i)
40    continue
c compute weighted and unweighted averages
      pavp = wpsump/wtsump
      pavs = wpsums/wtsums
      savp = wssump/wtsump
      savs = wssums/wtsums
c
c compute slop (vp/vs) based on s dependent on p
      do 50 i=1,nr
        p2sum = p2sum + swt(i)*(dp(i)-pavs)**2
        pssum = pssum + swt(i)*(dp(i)-pavs)*(ds(i)-savs)
        s2sum = s2sum + swt(i)*(ds(i)-savs)**2
50    continue
cd    print *, 's2sum, savs, pssum, pavs, p2sum'
cd    print *,  s2sum, savs, pssum, pavs, p2sum
c
      dslop1 = pssum/p2sum
c compute s.e. of s readings with weight code of zero (ses)
      arg = ( s2sum - dslop1*pssum ) / (nr - 2.d0)
      if (arg .lt. 0.d0) then
        print *, 'probable round off error, arg = ', arg
        print *, ' but arg should not be less than 0.'
        arg = 0.d0
      endif
      oses = dsqrt(arg)
c use the larger estimate of ses
      ses = oses
      if (ses .lt. abs(test(29))) ses = abs(test(29))
      dse1 = ses/dsqrt(p2sum)
cd    print *, 'ses, test(29) ', ses, test(29)
c
      dsint1 = savs - dslop1*pavs
      if(dslop1 .eq. 1.0) dslop1 = 1.01
      dorig1 = (dslop1*pavs-savs)/(dslop1-1.0)
cd     write(7, 51) dslop1, dse1
cd51    format(' for s vs p regression, vpvs = ', d10.4, ' and se = ',
cd   *   d10.4)
c
c compute slope (vp/vs) based on p dependent on s
      p2sum = 0.d0
      pssum = 0.d0
      s2sum = 0.d0
      do 52 i=1,nr
        p2sum = p2sum + pwt(i)*(dp(i)-pavp)**2
        pssum = pssum + pwt(i)*(dp(i)-pavp)*(ds(i)-savp)
        s2sum = s2sum + pwt(i)*(ds(i)-savp)**2
52    continue
cd    print *, 's2sum, savp, pssum, pavp, p2sum'
cd    print *,  s2sum, savp, pssum, pavp, p2sum
c
      dslop2 = pssum/s2sum
c compute s.e. of p readings with weight code of zero (sep)
      arg = ( p2sum - dslop2*pssum ) / (nr - 2.d0)
      if (arg .lt. 0.d0) then
        print *, 'probable round off error, arg = ', arg
        print *, ' but arg should not be less than 0.'
        arg = 0.d0
      endif
      osep = dsqrt(arg)
      sep = osep
      if (sep .lt. abs(test(29))) sep = abs(test(29))
      dse2 = sep/( dsqrt(s2sum)*(dslop2**2.d0) )
      dslop2 = 1.d0/dslop2
      dsint2 = savp - dslop2*pavp
      if(dslop2 .eq. 1.0) dslop2 = 1.01
      dorig2 = (dslop2*pavp-savp)/(dslop2-1.0)
cd     write(7, 53) dslop2, dse2
cd53    format(' for p vs s regression, vpvs = ', d10.4, ' and se = ',
cd   *   d10.4)
c
c find minimum sum
      dvpvs = .6
c     initial central value
      vpvsp = (dslop1 + dslop2)/2.d0
      tsumin = 99999.
c the central value need only be computed the first time
      do 60 j = 1, 8
        do 54 i = 1, 5
          vpvst = vpvsp - dvpvs*(-3 + i)
          tsum = 0.d0
c         compute averages
          do 43 k = 1, nr
            eqwt(k) = 1./( 1./swt(k) + (vpvst**2.d0)/pwt(k) )
            wpsum = wpsum + eqwt(k)*dp(k)
            wssum = wssum + eqwt(k)*ds(k)
            wtsum = wtsum + eqwt(k)
43        continue
          pav = wpsum/wtsum
          sav = wssum/wtsum
          do 44 k = 1, nr
            tsum = tsum + eqwt(k)*
     *                ( ds(k)-sav - vpvst*(dp(k)-pav) )**2
44        continue
          if (tsum .lt. tsumin) then
            tsumin = tsum
            vpvsp1 = vpvst
          endif
cd        print *, vpvst, tsum, tsumin
54      continue
cd     write(injump, 55) vpvsp1, tsum
cd55    format(d12.4, 3x, d12.4)
      vpvsp = vpvsp1
      dvpvs = dvpvs*.4
60    continue
c
      dse3 = dsqrt(dse1**2.d0 + dse2**2.d0)
      dsint3 = sav - vpvsp1*pav
      if(vpvsp1 .eq. 1.0) vpvsp1 = 1.01
      dorig3 = (vpvsp1*pav-sav)/(vpvsp1-1.0)
c convert to real for output
      vpvs1 = dslop1
      sint1 = dsint1
      se1 = dse1
      orig1 = dorig1
c
      vpvs2 = dslop2
      sint2 = dsint2
      se2 = dse2
      orig2 = dorig2
c
      vpvs3 = vpvsp1
      sint3 = dsint3
      se3 = dse3
      orig3 = dorig3
c
      return
      end
c end line3
