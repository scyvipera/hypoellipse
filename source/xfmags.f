c xfmags.for    []
      subroutine xfmags
c compute f-magnitude and x-magnitude
c added xmag correction to UofA magnitudes.  3/16/94  jcl
      include 'params.inc' 
      parameter (ndly = 11)
      real mag
      dimension xtemp(npa), ftemp(npa)
      character*4 iahead*60, msta*5, nsta*5, icard*110, uacal*50
      character*1 dnstrg
      common /char/ iahead, msta(npa), nsta(nsn), icard
      logical medmag 
      common /dinx/ imag,imagsv,medmag
      character*1 bksrc
      common /dipu/ ipkdly, igsum, bksrc
      common /dix/ iexcal, uacal
      common /dmost/ ipun,ivlr,blank
      common /ihfgpq/ itest(100)
      common /ilpx/ calr(5, nsn),fmgc(nsn),xmgc(nsn)
      common /imost/ test(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /iox/ prr(nsn),iuses
      integer fmwt, xmwt
      common /ioxl/ fmwt(nsn), xmwt(nsn)
      common /ix/      ir,qspa(9,40)
      common /olnx/ xmag(npa),fmag(npa),avxm,avfm,xmmed,fmmed
      common /onx/ kluse(npa), cluse(npa), peruse(npa), ampuse(npa)
      common /ox/ sysmag(npa), gndmot(npa)
      common /pfnoqv/ kdate,khrmn,khr
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      character phcard*117, krm*2, seq*5
      common /pno1/ phcard(npa), krm(npa), seq
      common /pnoqtx/ ldx(npa)
      common /povx/ amx(npa)
      common /pot/ ts(npa),ihr,model(npa), keyphi(npa)
      common /pox/ fmp(npa),prx(npa),icent1,icent2
      integer punt
      common /punt/ punt
      common /qmost/ wt(npa),z
      common /qgnotx/ delta(npa)
      common /tonxb/ t(npa),fms(npa)
      common /tmost/ x(4,npa)
      common /xfmno/ mag
      character*1 mgndx
      common /xfmno1/ mgndx
      common /xo/ nm,sdxm,nf,sdfm
      character*1 kodsym
      common /xo1/ kodsym(npa)
      character*1 ampsc
      dimension rspa(8,20)
      data zmc1,zmc2,pwc1,pwc2/0.15,3.38,0.80,1.50/
      data rspa/-0.02, 1.05, 0.60, 0.26, 0.66, 0.55, 0.17, 0.42,
     *           0.14, 1.18, 0.67, 0.23, 0.79, 0.66, 0.27, 0.64,
     *           0.30, 1.29, 0.74, 0.19, 0.90, 0.76, 0.35, 0.84,
     *           0.43, 1.40, 0.79, 0.12, 1.00, 0.86, 0.43, 0.95,
     *           0.55, 1.49, 0.84, 0.05, 1.08, 0.93, 0.49, 1.04,
     *           0.65, 1.57, 0.89,-0.03, 1.16, 1.00, 0.55, 1.13,
     *           0.74, 1.63, 0.94,-0.10, 1.23, 1.07, 0.63, 1.24,
     *           0.83, 1.70, 1.00,-0.16, 1.30, 1.15, 0.72, 1.40,
     *           0.92, 1.77, 1.06,-0.20, 1.38, 1.25, 0.83, 1.50,
     *           1.01, 1.86, 1.11,-0.24, 1.47, 1.35, 0.95, 1.62,
     *           1.11, 1.96, 1.14,-0.28, 1.57, 1.46, 1.08, 1.73,
     *           1.20, 2.05, 1.15,-0.33, 1.67, 1.56, 1.19, 1.84,
     *           1.30, 2.14, 1.14,-0.39, 1.77, 1.66, 1.30, 1.94,
     *           1.39, 2.24, 1.11,-0.46, 1.86, 1.76, 1.40, 2.04,
     *           1.47, 2.33, 1.06,-0.53, 1.95, 1.85, 1.50, 2.14,
     *           1.53, 2.41, 1.00,-0.62, 2.03, 1.93, 1.58, 2.24,
     *           1.56, 2.45, 0.92,-0.71, 2.07, 1.97, 1.62, 2.31,
     *           1.53, 2.44, 0.84,-0.80, 2.06, 1.96, 1.61, 2.31,
     *           1.43, 2.36, 0.75,-0.89, 1.98, 1.88, 1.53, 1.92,
     *           1.25, 2.18, 0.66,-0.99, 1.82, 1.72, 1.37, 1.49/
      mgndx = ' '
      mag = blank
      zsq = z**2
      nm = 0
      avxm = 0.0
      sdxm = 0.
      nf = 0
      anf = 0.0
      avfm = 0.0
      sdfm = 0.
c----
c start loop through each station
      if(iprn .ge. 5) write(punt, '(a)') 'begin xfmags'
      do 40 i = 1,nrp
c   xmag calculation
c 	print *, ' station = ', phcard(keyphi(i))(1:4)
        xmag(i) = blank
	sysmag(i) = blank
	gndmot(i) = blank
        rad2 = delta(i)**2 + zsq
        cluse(i) = -1.
c set period
        peruse(i) = prx(i)
        if (peruse(i) .lt. 0.01) peruse(i) = prr(kdx(i))
c      print *, ' rad2 = ', rad2
c       if ((rad2.lt.1.) .or. (rad2.gt.360000.)) goto 30
        amxi = abs(amx(i))
c      print *, 'amxi = ', amxi
        if (amxi.lt.0.01) goto 30
c       take care of a1vco gain range state
        if (phcard(keyphi(i))(62:62) .eq. '1') then
          ampuse(i) = amxi*10.
        else if (phcard(keyphi(i))(62:62) .eq. '2') then
          ampuse(i) = amxi*500.
        else
          ampuse(i) = amxi
        endif
c the amplitude source is phcard(i)(108:108)
        ampsc = phcard(keyphi(i))(108:108)
c      print *, 'amplitude source code = ', ampsc
        if(ampsc .eq. ' ') ampsc = bksrc
        ampsc = dnstrg(ampsc)
        if(ampsc .eq. 'j' .or. ampsc .eq. 'x') ampsc = 'd'
c      print *, 'amplitude source code = ', ampsc
c      print *, 'klas = ', klas(1, kdx(i))
        if (klas(1, kdx(i)) .eq. 18) then
c Use the masscomp calibration for PC data also.
	  if((ampsc .eq. 'p' .or.
     *	      ampsc .eq. 'o' .or.
     *	      ampsc .eq. 'u' .or.
     *	      ampsc .eq. 'i' .or.
     *	      ampsc .eq. 'g' .or.
     *	      ampsc .eq. 'q' .or.
     *	      ampsc .eq. 'k')) ampsc = 'd'
c Leave z alone, as this is entered in caldata.prm as z.  jcl 9/1/95
c         use university of alaska magnitude subroutine
          kluse(i) = klas(1, kdx(i))
c      print *, ' call uamag( msta ampsc kdate rad2 ampuse prx )'
c      print *,  msta(i), ampsc, kdate, rad2, ampuse(i), prx(i)
          call uamag (icent2, msta(i), ampsc, kdate, rad2, ampuse(i),
     &        prx(i), xmag(i), cluse(i), sysmag(i), gndmot(i),
     &        test(52), punt, blank)
          if (cluse(i) .eq. -1) then
	    xmag(i) = blank
	    goto 30
	  endif
	  xmag(i) = xmag(i) + xmgc(kdx(i))
c if xmgc is .ge. 6, then subtract 10 and do not
c include in average.
          if (xmgc(kdx(i)) .ge. 6.) then
            xmag(i) = xmag(i) - 10.
            goto 30
          endif
c         print *, 'xmag cluse = ', xmag(i), cluse(i)
        else
c all other klas values are processed here
c take care of semens playback setting
          if ((phcard(keyphi(i))(61:61) .eq. '1') .and.
     *      (ampsc .eq. 's')) then
            ampuse(i) = ampuse(i)*4.
          endif
          if ( (ampsc .eq. 'e') .or. (ampsc .eq. 'v') .or.
     *         (ampsc .eq. '1') .or. (ampsc .eq. '4') .or.
     *         (ampsc .eq. ' ') .or. (ampsc .eq. '*') ) then
c use standard usgs film calibration, system response in first position
c this is necessary because the usgs data does not distinguish between
c 20x film/202-vco data (response=1) and 20x film/a1vco data (response=9).
c this also alows other hypoellipse users to use the first position to
c specify calibrationc parameters and to leave the amplitude source codes blank.
            kluse(i) = klas(1, kdx(i))
cd          print *, 'amp. source was e, v, 1, 4, or blank'
cd          print *, 'kluse = ', kluse(i)
          else if (ampsc .eq. '2') then
c use a1vco/cusp fm tape calibration, system response 10
            kluse(i) = 10
          else if ( (ampsc .eq. 'p') .or. (ampsc .eq. 'o') .or.
     *              (ampsc .eq. 'g') .or. (ampsc .eq. 'k')) then
c use a1vco/pcelog calibration, system response 11
            kluse(i) = 11
          else if (ampsc .eq. 's') then
c use a1vco/semens playback, system response 12
            kluse(i) = 12
          else if ( (ampsc .eq. 'd') .or. (ampsc .eq. 'j') .or.
     *              (ampsc .eq. 'x') .or. (ampsc .eq. 'a')) then
c use a1vco/u of a masscomp, system response 13
            kluse(i) = 13
          else
c odd amplitude source, so do not compute xmag
            goto 30
          endif
c now find a c10 value that corresponds to this system response
          do 17 j = 1, 5
            if (kluse(i) .eq. klas(j, kdx(i))) then
              cluse(i) = calr(j, kdx(i))
cd            print *, 'use c10 = ', cluse(i)
              goto 18
            endif
17        continue
c no c10 for this k value, so skip xmag
cd        print *, 'no c10 for this k value, so skip xmag'
          goto 30
c
18        if (cluse(i) .lt. 0.01) goto 30
          k = kluse(i)
cd        print *, 'use response function # ', k
          if ((k.lt.0) .or. (k.gt.17)) goto 30
          xlmr = 0.
          if (k .eq. 0) goto 20
          if (k .le. 8) then
            if ((peruse(i).gt.3.162) .or. (peruse(i) .lt. 0.040))
     *      goto 30
            fq = 10.*alog10(1./peruse(i)) + 6.
            ifq = fq
            xlmr = rspa(k,ifq)
            if(ifq .lt. 20) then
              xlmr = xlmr + (fq-ifq)*(rspa(k,ifq+1) - rspa(k,ifq))
            endif
          else
            if ((peruse(i) .gt. 79.432) .or.
     *      (peruse(i) .lt. 0.010)) goto 30
            fq = 10.*alog10(1./peruse(i)) + 20.
            ifq = fq
            n = k - 8
            if (n .gt. iexcal) then
cd            print *,
cd   *        'can"t use a calibration curve that hasn"t been defined!'
cd            print *, 'n, k, iexcal = ', k, n, iexcal
              goto 30
            endif
            xlmr = qspa(n, ifq)
	    if(qspa(n, ifq) .eq. 0.) goto 30
            if(ifq .lt. 40) then
	      if(qspa(n, ifq+1) .eq. 0.) goto 30
              xlmr = xlmr + (fq-ifq)*(qspa(n,ifq+1) - qspa(n,ifq))
            endif
          endif
c compute magnification of station for period peruse(i)
c magnification (sysmag) is in measurement units per mm of ground displacement
c note that log(magnification in counts/mm) = xlmr + log(wood anderson magnification)
          alwamag = alog10( wa_magn(test(52), peruse(i)) )
c         print *, 'For station = ', phcard(keyphi(i))(1:4)
c         print *, 'amp = ', ampuse(i), 'counts'
c         print *, 'distance (rad2) = ', rad2
c 	  print *, 'WA magnification for period ', peruse(i), ' is ', 
c      *    wa_magn(test(52), peruse(i))
c         print *, ' xlmr = ', xlmr, ' cluse(', i,') = ', cluse(i)
          almgnif = xlmr + alwamag
          sysmag(i) = cluse(i)*(10.**almgnif)
c 	  print *, 'sysmag = ', sysmag(i)
c compute ptp gound motion in microns
	  gndmot(i) = (ampuse(i)*10**3)/sysmag(i)
c	  print *, 'ground motion in microns = ', gndmot(i)
c         
20        blac = alog10( ampuse(i)/(2.*cluse(i)) ) - xlmr
          rld2 = alog10(rad2)
          blnt = zmc1 - pwc1*rld2
          if (rad2 .ge. 40000.) blnt = zmc2 - pwc2*rld2
c xmag based on maximum amplitude
          xmag(i) = blac - blnt + xmgc(kdx(i))
c         print *, ' xmag = ', blac, ' - ', blnt, ' + ', xmgc(kdx(i))
c if xmgc is .ge. 6, then subtract 10 and do not
c include in average.
          if (xmgc(kdx(i)) .ge. 6.) then
            xmag(i) = xmag(i) - 10.
            goto 30
          endif
        endif
c       if ((rad2.lt.1.) .or. (rad2.gt.360000.)) then
        if ((rad2.lt.1.) .or. (rad2.gt.2250000.)) then
          xmag(i) = blank
	  goto 30
        endif
        if (xmwt(kdx(i)) .ne. 0) then
          nm = nm + 1
          avxm = avxm + xmag(i)
          sdxm = sdxm + xmag(i)**2
	  xtemp(nm) = xmag(i)
        endif

c fmag calculation
30      fmag(i) = blank
        kodsym(i) = ' '
        if (delta(i) .eq. 0.0) goto 40
        fms(i) = 0.
        if (fmp(i) .eq. blank) goto 40
        if (fmgc(kdx(i)) .eq. 0.) goto 40
        fms(i) = fmp(i) - t(i)*(test(1) - 1.)
        if (ksmp(i) .eq. 0) fms(i) = fmp(i) - t(i)
        if (ldx(i) .eq. 0) goto 31
        k = ldx(i)
        if ((abs(x(4,i))+abs(x(4,k))) .gt. fmp(i)/10.) goto 31
        if ((wt(i) .ne. 0.) .and. (wt(k) .ne. 0.)) fms(i) =
     *  fmp(i) - (tp(k) - tp(i))
31      if (fms(i) .lt. 0.1) fms(i) = 0.1
        if ((fmp(i)/fms(i)) .lt. 5.) goto 32
        kodsym(i) = 's'
        goto 40
32      fmpors = fmp(i)
        if (iuses .eq. 1.) fmpors = fms(i)
        ftouse = fmpors*fmgc(kdx(i))
        alogf = alog10(ftouse)
c fmag based on duration of signal
        fmag(i) = test(31) + test(32)*alogf + test(33)*delta(i) +
     *  test(40)*z + test(43)*alogf*alogf
        if (fmwt(kdx(i)) .ne. 0) then
          nf = nf + 1
c 5/27/92 do simple on or off rather than weighted mean
c         avfm = avfm + fmag(i)*fmwt(kdx(i))
c         sdfm = sdfm + (fmag(i)*fmwt(kdx(i)))**2
          avfm = avfm + fmag(i)
          sdfm = sdfm + fmag(i)**2
c         anf = anf + fmwt(kdx(i))
	  ftemp(nf) = fmag(i)
	endif
40    continue
c finish loop
c----
      if (nm .eq. 0) then
        avxm = blank
	xmmed = blank
      else
c avxm is average of nm xmags
        avxm = avxm/nm
        if (nm .ge. 2) then
          sdxm = sqrt(abs(sdxm/nm-avxm**2))
        else
          sdxm = 0.
        endif
c xmmed is the median xmag
	call median(xtemp, nm, xmmed, ierr)
      endif
      if (nf .eq. 0) then
        avfm = blank
	fmmed = blank
      else
c avfm is weighted average of nf fmags
c       avfm = avfm/anf
        avfm = avfm/nf
        if (nf .ge. 2) then
c         sdfm = sqrt(abs(sdfm/anf-avfm**2))
          sdfm = sqrt(abs(sdfm/nf-avfm**2))
        else
          sdfm = 0.0
        endif
c fmmet is the median fmag
	call median(ftemp, nf, fmmed, ierr)
      endif
      if ((nm .eq. 0) .and. (nf .eq. 0)) return
c----
c use all stations in preferred magnitude calculation

      if((imag .eq. 0) .or.
     *  ((imag .eq. 3) .and. (nf .eq. 0)) .or.
     *  ((imag .eq. 4) .and. (nm .ne. 0)) .or.
     *  ((imag .eq. 2) .and. (nf .eq. 0))) then
c xmag
        mag = avxm
        if(medmag) mag = xmmed

        if (nm .gt. 0) mgndx = 'x'
      else if((imag .eq. 1) .or.
     *       ((imag .eq. 4) .and. (nm .eq. 0)) .or.
     *       ((imag .eq. 3) .and. (nf .ne. 0)) .or.
     *       ((imag .eq. 2) .and. (nm .eq. 0))) then
c fmag
        mag = avfm
        if(medmag) mag = fmmed
        if (nf .gt. 0) mgndx = 'f'
      else if(imag .eq. 2) then
c average
        mag = 0.5*(avxm + avfm)
        if(medmag) mag = 0.5*(xmmed + fmmed)
        mgndx = 'a'
      endif
200   return
      end

	real function wa_magn(wa_static_mag, period)
c use Wood Andersion natural frequency = 1.25 Hz
	parameter (fzsq = 1.25*1.25)
c use Wood Anderson beta = 0.8
	parameter (betasq = 0.8*0.8)

	if(period .eq. 0.) then
	  wa_magn = -1.0
	else
	  fsq = 1./(period*period)
	  anumer = wa_static_mag*fsq
	  denom = sqrt((fzsq - fsq)**2 + 4.*betasq*fzsq*fsq)
	  wa_magn = anumer/denom
	endif

	return
	end
c end xfmags
