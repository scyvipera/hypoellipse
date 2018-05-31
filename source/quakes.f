c quakes.for    []
      subroutine quakes
c controls the iteration for one earthquake.
c arrays
c     at(3)        computed shift if hypocenter -- in rotated coordinate sys
c     az(npa)      azimuth measured clockwise from north of station with
c                  ith index as measured from current trial epicenter.
c     azmv(5)      azimuth's divideing quadrants for azimuthly weighted
c                  jeffreys weighting.
c     b(4)         hypocenter shift computed by regres (longitude, latitude,
c                  depth, in kilometers).
c     delta(npa)   distance to station with ith index.
c     dly(ndly, nsn)  p-phase delay for model i at station j.
c     duse(npa)    distance array (km) only for stations to be used.
c     iqdo(npa)    quadrant assignment in jeffreys weighting - printed in output
c                  after distance.
c     itest(100)   integer versions of test variables -- itest(i) = int(test(i)
c     iuse(npa)    if the critical station option is used, then stations to be
c                  used have iuse = 1 whereas all others have iuse = 0.
c     kdx(npa)     kdx(phase index number) = station index number.
c     keyd(npa)    sort index for all stations - ie the first station
c                  has index keyd(1).
c     klas(5, nsn) station response types.
c     ksmp(npa)    0 if the reading is an smp reading, 1 otherwise.
c     kwr(npa)     character*1 variable printed after residual to indicate
c                  something about the reading or weight.
c     lat(nsn)     latitude of station.
c     ldx(npa)     0 if no s reading, else array index position for s phase.
c     lon(nsn)     longitude of station.
c     msta(npa)    character*5 -- station name.
c     nsta(nsn)    character*5 -- station name.
c     se(4)        standard error ellipsoid semimajor axes, lon, lat, z, t.
c     test(100)    reset test values - see manual for definition.
c     tl(3)        ratio of step to standard error.
c     tp(npa)      arrival times.
c     w(npa)       assigned phase weight.
c     wf(51)       jeffreys weighting function weights.
c     wt(npa)      final weight of phase, including such factors as
c                  distance weight.
c     x(4, npa)    partial derivatives of travel time with respect to
c                  longitude, latitude, depth, and origin time.
c     xmean(4)     xmean(i) = average of x(i, j) over j
c     y(4)         final step in longitude, latitude, depth, and origin time,
c                  including effects of any limitions.
c variables
c type name          attributes    use
c
c r*4  aar           comm          average of the absolute value of residuals
c r*4  absgr                       absolute value of largest component of
c                                  hypocenter shift.
c r*4  absy1                       absolute value of y1
c r*4  absy2                       absolute value of y2
c r*4  absy3                       absolute value of y3
c r*4  adjsq         comm          square of vector hypocenter shift
c                                  (y1**2 + y2**2 + y3**2)
c r*4  aveaj         comm          average of the 3 eigenvalues of ss matrix
c r*4  avr           comm          average residual
c r*4  avrps         comm          average residual of all p and s phases but
c                                  excluding s-p residuals
c r*4  avuse         comm          equals avrps unless origin time is fixed
c                                  (inst = 8) in which case it equals zero
c r*4  avwt          comm          average weight
c r*4  azi                         azimuth of change in location when only
c                                  critical stations are used
c r*4  been          comm          in rplain, compute certain constants if
c                                  been = 0. and then set been to 1.  input1
c                                  resets been to 0.
c r*4  blank         comm          blank = 1.e20.  variables set to blank will
c                                  not be printed in the output.
c r*4  damp          comm          damping constant to be used in inversion.
c r*4  del                         distance solution changes when only critical
c                                  stations are used.
c r*4  deldeg                      unused argument of qdist
c r*4  dmax          comm          maximum distance used in distance weighting
c r*4  dz                          depth change when only critical stations
c                                  are used.
c l*4  freor         comm          true only if origin time is not fixed.
c r*4  gap           comm          azimuthal gap in stations
c char iahead        comm          heading for this run
c i*4  iboxc         comm          used in weight to control boxcar weighting
c char icard         comm          dummy input record used in many subs
c i*4  icrit                       equals 0 normally.  equals 1 when only
c                                  critical stations are being used.
c l*4  idclc                       if true, the compute new distances in qdist.
c                                  otherwise just reset reading wts & sort by
c                                  distance.
c i*4  iexit         comm          normally 0 but set to 1 if no solution can
c                                  be computed.
c i*4  iglob         comm          if 0 find global minimum, dup and dwn.
c                                  otherwise just find normal solution.
c i*4  igo           comm          if igo = 1 and test(47) ne 0 then add
c                                  extra equation to fix solution on a plane.
c i*4  incdmp                      incdmp is the number if times in a row
c                                  damping is increased.
c i*4  infor         comm          if not 0 on return from weighting then
c                                  there are not enough readings for a solution.
c i*4  inst          comm          column 19 of instruction record.
c                                  also set = -9 in boxau for calculation of rms
c i*4  logfil        comm          unit number for printed output (equals 6).
c i*4  iph           comm          controls printing heading for step output.
c i*4  iprn          comm          print control variable from print option.
c i*4  ipun          comm          punch control variable from punch option.
c i*4  ivlr          comm          number of the layer with variable thickness.
c i*4  kno           comm          delay model (1-5) being used.
c i*4  knst          comm          column 18 from instruction record.
c i*4  kz            comm          if zero, then compute full inversion.  if
c                                  1, 2, or 3 fix longitude, latitude, or depth.
c                                  if negative, do not add damping equations.
c r*4  latep         comm          geocentric latitude of hypocenter in radians.
c i*4  lbastm        comm          base origin time in minutes.
c r*4  lonep         comm          longitude of hypocenter in radians.
c i*4  ndwt          comm          number of stations with p or s used.
c i*4  near          comm          index number of nearest staton.
c i*4  ni            comm          counter for the number of iterations.
c i*4  nprwt                       # stations with p or s during last iteration.
c i*4  nr            comm          total # of p, s, and s-p phases.
c i*4  nrp           comm          total # of p and s-p phases.
c i*4  nrwt          comm          total # of phases with weight not equal zero.
c i*4  nsmp          comm          total # of s-p phases.
c i*4  nswt          comm          total # of s phases with weight not zero.
c r*4  oldy1                       previous step in longitude (km).
c r*4  oldy2                       previous step in latitude (km).
c r*4  oldy3                       privious step in depth.
c r*4  org           comm          origin time in seconds with respect to lbastm
c r*4  pdrms         comm          predicted rms after next shift in hypocenter.
c r*4  prms                        previous rms value.
c r*4  ratio                       reduction ratio applied to hypocenter shift
c                                  because depth change exceeded test(22).
c r*4  rms           comm          root mean square residual.
c r*4  savez         comm          initial depth.
c r*4  savla         comm          initial latitude.
c r*4  savlo         comm          initial longitude.
c r*4  savor         comm          initial origin time.
c l*4  supout        comm          if true, supress all output.
c r*4  sv30                        saved value of test(30).
c r*4  svla                        saved value of latitude during critical run.
c r*4  svlo                        saved value of longitude during critical run.
c r*4  svz                         saved value of depth during critical run.
c r*4  xadjsq                      square of vector hypocenter shift
c r*4  z             comm          earthquake depth.
c r*4  zdn           comm          maximum downward shift of depth within 1
c                                  standard deviation limit of rms.
c r*4  zer           comm          not used.
c r*4  zup           comm          maximum upward shift of depth within 1
c                                  standard deviation limit of rms.
      include 'params.inc' 
      parameter (ndly = 11)
      real lat,lon,latep,lonep
      logical idclc, supout, backup
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /anox/ keyd(npa)
      common /bqz/ avrps,avuse
      common /dhin/ iglob, zup, zdn
      common /dmost/ ipun,ivlr,blank
      common /gmost/ az(npa)
      common /ghnq/ iexit
      logical freor
      common /hopq/ savla,savlo,savez,savor,freor
      common /imost/ test(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /ihfgpq/ itest(100)
      common /iclmpq/ lat(nsn),lon(nsn)
      common /iiq/ wf(51)
      common /logfil/ logfil
      common /ohq/ gap, supout
      common /oqr/ y(4),kz,at(3),tl(3),pdrms,b(4)
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /pgqv/ w(npa)
      common /phoqn/ inst,knst
      character*1 kwr, iqdo
      common /pbnoq/ kwr(npa),iqdo(npa)
      common /pnoqtx/ ldx(npa)
      common /qmost/ wt(npa),z
      common /qmost1/ lonep,ni,latep
      common /qgo/ org
      common /qgnotx/ delta(npa)
      common /qn/ zer
      common /qo/ dmax,ndec,adjsq,iph
      common /qw/ azmv(5), iuse(npa), duse(npa), ndwt, iboxc
      logical ddone, adone, tdone, bdone, jdone
      common /qw1/ ddone, adone, tdone, bdone, jdone
      common /rioq/ been,damp,dmpinc,igo
      common /pqt/ near
      common /rfgnoq/ se(4)
      common /rq/ xmean(4),avwt,aveaj
      common /tmost/ x(4,npa)
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp

c begin code
      if (iprn .ge. 5) write(punt, '(a)') ' begin subroutine quakes'
c -----------------------------------------------------------------------
c initialize various values
c -----------------------------------------------------------------------
      it21 = itest(21)
      backup = .false.
      sv30 = test(30)
      ddone = .false.
      adone = .false.
      tdone = .false.
      bdone = .false.
      jdone = .false.
      incdmp = 0
      idclc = .true.
      icrit = 0
      azmv(1) = 0.0
      do 10 i = 2,5
   10 azmv(i) = azmv(i-1) + 90.
      do 20 i = 1,nr
   20 iuse(i) = 1
      nrwt = 0.0
      avwt = 1.0
      avrps = 0.0
      iexit = 0
      adjsq = 0.
      iph = 0
      prms = 100000.0
      dmpmn = test(35)
      damp = dmpmn
      igo = 1
c -----------------------------------------------------------------------
c use only critical stations unless called from boxaux [inst = -9]
c -----------------------------------------------------------------------
      if ((itest(44) .ge. 100) .and. (inst .ne. -9)) then
        icrit = 1
        if (nrp .le. 6) then
          write(punt, '(a)') ' too few phases to rerun with '
          write(punt, '(a)') '  only critical phases'
          iexit = 2
          return
        endif
        ni = itest(37)
        if(iprn .ge. 5) write(punt, '(a)') ' call subroutine critic'
        call critic(delta,az,w,iuse,nrp,nr,ldx)
c reset weights to reading weights
        do 22 i = 1,nrp
          wt(i) = w(i)*iuse(i)
          kwr(i) = ' '
          if (iuse(i) .eq. 0) kwr(i) = 'x'
          if (ldx(i) .gt. 0) then
            k = ldx(i)
            kwr(k) = ' '
            wt(k) = w(k)*iuse(k)
            if (iuse(k) .eq. 0) kwr(k) = 'x'
            if ( (knst.ne.1) .and. (knst.ne.6) ) wt(k) = 0.
          endif
22      continue
        svz = z
        svla = latep
        svlo = lonep
        if(inst .ne. 9) then
          if (z .lt. 10.) z = 10.
        endif
        call qdist(az, deldeg, delta, duse, .true., kdx, keyd,
     *  lat, lon, latep, lonep, ldx, near,
     *  ndwt, nprwt, nrp, nrwt, wt)
        call trvdrv
        call zstats
        if (nrwt .lt. 3) goto 96
        if (inst .ne. 9) then
          goto 110
        else
c fixed location, so go ahead and compute steps and standard errors
c set damping = 0.0 and kz = 0
          kz = 0
          damp = 0.0
          sv34 = test(34)
          test(34) = -1.0
          call regres
          test(34) = sv34
          return
        endif
      endif

c -----------------------------------------------------------------------
c initialize trial hypocenter
c -----------------------------------------------------------------------
      ni = 1
      if(iprn .ge. 5) write(punt, '(a)')
     *                  ' set ni=1 and init. hypocenter'
      latep = savla
      lonep = savlo
      z = savez
      org = savor
c -----------------------------------------------------------------------
c set weights to reading weights unless called from boxau
c -----------------------------------------------------------------------
      if(inst .ne. -9) then
        if(iprn .ge. 5) write(punt, '(a)') ' set wts to reading wts'
        do 42 i = 1,nrp
          wt(i) = w(i)*iuse(i)
          kwr(i) = ' '
          if (iuse(i) .eq. 0) kwr(i) = 'x'
          if (ldx(i) .gt. 0) then
            k = ldx(i)
            kwr(k) = ' '
            wt(k) = w(k)*iuse(k)
            if (iuse(k) .eq. 0) kwr(k) = 'x'
            if ( (knst.ne.1) .and. (knst.ne.6) ) wt(k) = 0.
          endif
42      continue
        if (iprn .ge. 5) write(punt, '(a)') ' call zstats'
        call zstats
      endif

      if (iprn .ge. 5) write(punt, 99) latep, lonep, z, lbastm
c
c -----------------------------------------------------------------------
c inst = 9 -- fixed solution
c -----------------------------------------------------------------------
      if (iabs(inst) .eq. 9) then
        if(iprn .ge. 5) write(punt, '(a)') ' fixed solution'
        ni = itest(37)
        call qdist(az, deldeg, delta, duse, .true., kdx, keyd,
     *  lat, lon, latep, lonep, ldx, near,
     *  ndwt, nprwt, nrp, nrwt, wt)
        call trvdrv
c do not call weight if called from boxau.  inst = -9
        if(inst .eq. 9) call weight
        if (iprn .ge. 5) then
          write(punt, *) ' freor = ', freor
          write(punt, *) ' org 	 = ', org
	  write(punt, *) ' avrps = ', avrps
	endif
        if (freor) then
          org = org + avrps
          do 92 i = 1, nr
            if (ksmp(i) .eq. 1) x(4,i) = x(4,i) - avrps
92        continue
          if (iprn .ge. 5) write(punt, '(a)')
     *    ' call zstats'
          call zstats
        endif
c return now if called from boxau
        if(inst .eq. -9) return
c otherwise, go ahead and compute steps and standard errors
c   set dmping = 0.0 and kz = 0
c   prevent scaling by setting test(34) = -1.0
        sv34 = test(34)
        test(34) = -1.0
        kz = 0
        damp = 0.0
        call regres
        test(34) = sv34
        return
      endif
c
c -----------------------------------------------------------------------
c exit if there is not enough data to calculate a solution
c -----------------------------------------------------------------------
c     if (nr .lt. 3) then
      if (nr .ge. 3) goto 110
        gap = 0.0
96      continue
        write(punt,99) latep,lonep,z,lbastm
99      format(/'        latep       lonep           z  base time',/
     *  ,1x,3f12.6,i10)
        write(punt,97) nr
97      format(' xxxxx insufficient data with weights not equal',
     *  ' to zero to calculate a solution', /,
     *  1x, i5, ' readings'/)
        iexit = 1
        rms = 0.
        call qdist(az, deldeg, delta, duse, .true., kdx, keyd,
     *  lat, lon, latep, lonep, ldx, near,
     *  ndwt, nprwt, nrp, nrwt, wt)
        return
c     endif
c -----------------------------------------------------------------------
c begin main iteration loop for hypocentral determination
c -----------------------------------------------------------------------
  110 continue
      if (iprn .ge. 5) then
        write(punt, '(a)') ' top of quakes loop'
        write(punt, '(a, e15.7, /, a)')
     *  ' damp = ', damp, ' call qdist next'
      endif
        call qdist(az, deldeg, delta, duse, .true., kdx, keyd,
     *  lat, lon, latep, lonep, ldx, near,
     *  ndwt, nprwt, nrp, nrwt, wt)
c calculate traveltimes and derivatives from hypocenter to station
      if (iprn .ge. 5) write(punt, '(a)')
     *                   ' call trvdrv to compute ttimes'
cd    print *, 'compute travel times'
      call trvdrv
      if (iprn .ge. 5) write(punt, '(a)')
     *' call weight to compute residuals and rms'
      call weight
      if (iprn .ge. 5) write(punt, '(a, f20.4, a, f20.4)')
     *' old rms = ', prms, ' new rms = ', rms
      if (nrwt .lt. 3) goto 96
c----
c check if solution is better than previous one
cd    print *, 'check if solution is better'
cd    print '(a, 3i8)', '  ni, nprwt, nrwt ',
cd   *          ni, nprwt, nrwt
cd    print *, '  prms, rms, damp, backup ', prms, rms, damp, backup
      if (backup) goto 170
      if (nrwt .gt. nprwt) then
        incdmp = 0
        goto 170
      endif
      if (rms .le. prms) then
        incdmp = 0
        if ((ni .gt. 1) .and. (damp .gt. dmpmn)) then
          damp = damp/2.
          if (damp .lt. dmpmn) damp = dmpmn
cd        print *, '  **** decreased (possibly) damp to ', damp
        endif
        goto 170
      endif
cd    print *, 'rms gt prms and nrwt le nprwt '
c allow rms to increase if distance or azimuthal weighting just applied
      if ((ni .eq. test(10)) .or. (ni .eq. test(13))) goto 170
c -----------------------------------------------------------------------
c solution rms increased, so reverse last step and increase damping
c -----------------------------------------------------------------------
c allow an extra iteration to reverse last step (up to test(21)*1.3)
      it21 = it21 + 1
      if(it21 .gt. itest(21)*1.3) it21 = itest(21)*1.3
      if (iprn .ge. 5) write(punt, '(a)')
     *' solution rms increased, so increase damping'
      incdmp = incdmp + 1
      if(incdmp .eq. 3) then
        if (iprn .gt. -2) then
          write(punt, 176)
  176     format(' *** experiencing convergence problems, ')
        endif
      endif
      dmpnew = damp + aveaj
      if(dmpnew .gt. damp*100.) dmpnew = damp*100.
      if(dmpnew .lt. damp*10.) dmpnew = damp*10.
      damp = dmpnew
cd    print *, '  **** increased damp to ', damp
      pdrms = 0.0
      do 177 i = 1,3
        b(i) = blank
  177 continue
      y(1) = -y(1)
      y(2) = -y(2)
      y(3) = -y(3)
      y(4) = -y(1)*xmean(1)-y(2)*xmean(2)-y(3)*xmean(3)
      xadjsq = y(1)**2+y(2)**2+y(3)**2
      backup = .true.
      goto 325
c -----------------------------------------------------------------------
c regression analysis of travel time residuals
c -----------------------------------------------------------------------
  170 continue
      if (iprn .ge. 5) write(punt, '(a)')
     *' compute next step with regres'
      prms = rms
      if (inst .eq. 1) goto 250
      if ((nrwt .eq. 3) .and. (nsmp .lt. 3)) goto 250
c free solution
      if (iprn .ge. 5) write(punt, '(a)')
     *' free solution with kz = 0'
      kz = 0
      call regres
c do not change depth if horizontal change is large
      if (abs(y(1))+abs(y(2)) .lt. test(25)) goto 300
      if (y(1)**2+y(2)**2 .lt. test(25)**2) goto 300
      if (iprn .ge. 5) write(punt, '(a)')
     *' large epicenter change, so call regres with fixed depth'
c fixed depth solution
  250 kz = 3
cd    print *, 'y1, y2 = ', y(1), y(2)
cd    print *, 'large horiz. shift or inst=1,'
cd    print *, 'or too few phases, so fix depth with kz = ', kz
      if (iprn .ge. 5) write(punt, '(a)')
     *' fixed depth solution with kz = 3'
      call regres
c limit focal depth change
  300 oldy1 = y(1)
      oldy2 = y(2)
      oldy3 = y(3)
      absy3 = abs(y(3))
c if damp > dmpmn reduce y by a factor of 2
c     if (damp .gt. dmpmn) then
c       y(1) = y(1)*.5
c       y(2) = y(2)*.5
c       y(3) = y(3)*.5
c     endif  
      if (absy3 .gt. test(22)) then
        ratio = test(22)/absy3
        y(1) = y(1)*ratio
        y(2) = y(2)*ratio
        y(3) = test(22)*sign(1., y(3))
      endif
c limit horizontal adjustment of epicenter
      absy1 = abs(y(1))
      absy2 = abs(y(2))
      if (absy1 .gt. absy2) then
        absgr = absy1
      else
        absgr = absy2
      endif
      if (absgr .gt. test(24)) then
        ratio = test(24)/absgr
        y(1) = y(1)*ratio
        y(2) = y(2)*ratio
        y(3) = y(3)*ratio
      endif
c avoid hypocenter in air
      if ((z+y(3)) .lt. 0.0) then
        y(3) = -z*test(23) + .000001
      endif
c recompute origin time shift
      y(4) = y(4)-(y(3)-oldy3)*xmean(3)-(y(1)-oldy1)*xmean(1)
     * -(y(2)-oldy2)*xmean(2)
      xadjsq = y(1)**2+y(2)**2+y(3)**2
      if (iprn .ge. 5) write(punt, '(a, 4e15.7)')
     *' final y(1), y(2), y(3), y(4) = ', y(1), y(2), y(3), y(4)
      backup = .false.
  325 if (iprn .ge. 1) call output(0)
c terminate iteration if hypocenter adjustment .lt. test(26)
      if (xadjsq .lt. test(26)) then
c if termination occurs before itest(37) iteration,
c set ni = itest(37) and continue
        if (iprn .ge. 5) write(punt, '(a, e10.5, a, e10.5)')
     *   ' adjustment of ', sqrt(xadjsq), ' < sqrt(test(26)) = ',
     *   sqrt(test(26))
        if (ni .ge. itest(37)) goto 500
        ni = itest(37)
        goto 110
      endif
      ni = ni + 1
      if (ni .gt. it21) goto 500
      if (iprn .ge. 5) write(punt, '(a)') 'adjust hypocenter ********'
      call back(y(2), y(1), latep, lonep, latep, lonep)
      z = z + y(3)
      org = org + y(4)
      adjsq = xadjsq
cd    print *, 'ni, prms, rms ', ni, prms, rms
      goto 110
c ---------------------------------------------------------------------
c termination
c ---------------------------------------------------------------------
  500 if (inst .ne. 8) then
c       reset origin time
        org = org + avrps
        do 507 i = 1,nr
          if (ksmp(i) .eq. 1) x(4,i) = x(4,i) - avrps
  507   continue
        if (iprn .ge. 5) write(punt, '(a, e15.7, a, e15.7)')
     *  ' just increased origin time by avrps = ', avrps, ' to ', org
      endif
      if (iprn .ge. 5) write(punt, '(a)')
     *' call zstats'
      call zstats
c compute error estimates by solving full normal equations
      if (iprn .ge. 5) write(punt, '(a)')
     *  ' compute error est by solving full normal equations'
      damp = 0.0
      sv34 = test(34)
      test(34) = -1.0
      if(inst .eq. 1) then
        kz = 3
      else
        kz = 0
      endif
      call regres
      damp = dmpmn
      test(34) = sv34
c ---------------------------------------------------------------------
c if this is the first time through (because igo = 1) & test(30) < 0
c continue run without fixing epicenter
c ---------------------------------------------------------------------
      if ((igo .eq. 1) .and. (test(28) .lt. 0.0) .and.
     * (test(30) .lt. 0.0)) then
        if (iprn .ge. 5) write(punt, '(a)')
     *  ' continue run without fixing epicenter'
        igo = 0
c temporarily change test(30) to 0.0 so epicenter will not be fixed
        test(30) = 0.0
        ni = itest(37)
        ndec = 0
        prms = 100000.
        iph = 0
        avrps = 0.0
        goto 110
      endif
      test(30) = sv30
c ---------------------------------------------------------------------
c if this is the first time through (igo = 1) & 
c test(28) < 0 & test(47) >0, then 
c continue run without fixing on a plane
c ---------------------------------------------------------------------
      if ((igo .eq. 1) .and. (test(28) .lt. 0.0) .and.
     * (test(47) .gt. 0.)) then
        if (iprn .ge. 5) write(punt, '(a)')
     *  ' continue run without fixing on a plane'
        igo = 0
        ni = itest(37)
        ndec = 0
        prms = 100000.
        iph = 0
        avrps = 0.0
        goto 110
      endif
c -----------------------------------------------------------------------
c for critical solution, compare with normal solution
c -----------------------------------------------------------------------
      if (icrit .ne. 0) then
        dz = z - svz
        call delaz(latep,lonep,del,deldeg,azi,svla,svlo)
        azi = azi + 180.
        if (azi .gt. 360.) azi = azi - 360.
        write(punt,2025) del,azi,dz
 2025   format(/,' using only critical stations the solution changes ',
     *  /,' by:   delta   az     z',/,
     *  6x, 3(1x,f5.1),' km'/)
      endif
      return
      end
c end quakes
