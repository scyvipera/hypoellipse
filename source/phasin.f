c phasin.for    []
      subroutine phasin
c get phase records for one earthquake and initialize related arrays
      include 'params.inc' 
      parameter (ndly = 11)
      character*1 poldev, pdev, comp
      character*1 isnla, isnlo, jsnla, jsnlo
      character*4 mamaz, mamla, mamor*6, fmit, rshft, dnstrg*5
      character*4 malaz, malla, malor*6
c       poldev specifies device from which p-polarity was obtained
c       pdev specifies device from which p-reading was made -
c            must have same telemetry delay as s-reading, because
c            the same telemetry delay is assumed for both
c       device codes currently used by usgs/uagi are:
c        v,*,1,4 - uags film viewed at 20x
c              v - viewer, geotech or kodak
c              * - assumed to be 20x film
c              1 - one-film digitizer
c              4 - four-film digitizer
c          %,a,f - uagi film viewed at 20x
c              % - assumed to be uagi 20x film
c              a - readings provided to usgs by uagi
c              f - uagi films read by usgs at uagi
c              w - atwc film viewed at 20x
c          d,j,x - daq or dan digital data
c        e,2 - usgs digitized from magnetic tape
c              e - elipse processing
c              2 - cusp processing
c        s - usgs siemens playback from magnetic tape
c        h - usgs helicorder
c        r - uagi helicorder
c        5 - five-day recorder
c        m - sma1 film recorder
c        l - usgs elog
c        b - published bulletin
c        c - unpublished canadian data (eg. tape)
c        t - atwc teletype (corrected for telemetry delay)
c        n - neis (corrected for telemetry delay)
c        p,o,g,k,i,u - pc recorders
c
      character*1 revers
      character seqins*5
      logical eoff, setmast, scatnow
      integer phaind
      real lat,lon,latr,lonr,mag
      character*4 iahead*60, msta*5, msta4*4, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      character*4 ipro, ichec, evtype*1, evstat*1, root*256
      common /dbhip/ ipro, ichec, evtype, evstat, root
      character*1 bksrc
      common /dipu/ ipkdly, igsum, bksrc
      common /dmost/ ipun,ivlr,blank
      common /dph/ noswt, eoff
      common /gmost/ az(npa)
      real*8 time1, time2
      common /hop/ time1,time2,nopu,notim
      common /hpn/ wslope
      logical freor
      common /hopq/ savla,savlo,savez,savor,freor
      common /iclmpq/ lat(nsn),lon(nsn)
      common /imost/ test(100)
      common /ihfgpq/ itest(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /ilmpu/ ns
      common /ilt/ vthk(2,nsn),ipdly(nsn),mod(nsn),ipthk(nsn)
      common /ilpu/ sw(nsn),ndate(nsn),nhr(nsn),mno(nsn),ielv(nsn)
      common /ilpx/ calr(5, nsn),fmgc(nsn),xmgc(nsn)
      common /ilv/ c(nsn),e(nsn)
      common /iox/ prr(nsn),iuses
      common /logfil/ logfil
      common /in/ irmo,iryr,odmag
      character*1  magke
      common /in1/ magke
      common /int/ thk(lmax+1),lbeg(mmax),lend(mmax),vi(lmax),vsq(lmax),
     *  vsqd(lmmax,lmax),f(lmax,lmmax),kl,ivway,sthk(mmax),sthk1(mmax),
     *  sdly(ndly,nsn)
      integer fmwt, xmwt
      common /ioxl/ fmwt(nsn), xmwt(nsn)
      logical fsteq
      character*1 revp, exdly
      common /ip/ latr,lonr,tpdly(2,nsn),infil,iofil,indexs,
     *  iscod(nsn),ipcod(nsn), fsteq, exdly(4, nsn), revp(6, nsn)
      common /ip1/ rsew(4)
      common /dhip/ inpt,isa,ilis,inmain,injump
      common /olnx/ xmag(npa),fmag(npa),avxm,avfm,xmmed,fmmed
      character*1 kwr, iqdo
      common /pbnoq/ kwr(npa),iqdo(npa)
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      character msym*1
      common /pfo/ msym(npa)
      common /pfnoqv/ kdate,khrmn,khr
      common /pgnov/ dt(npa)
      common /pgoq/ s(npa)
      common /pgnoqv/ p(npa),jmin(npa)
      common /pgqv/ w(npa)
      common /ph/ nfirst
      common /phoqn/ inst,knst
      common /pm/ jdx(nsn)
      common /pn/ itpused(npa)
      common /pnl/ ww(npa)
      common /pno/ lph,keyph(npa),nsum
      character phcard*117, krm*2, seq*5
      common /pno1/ phcard(npa), krm(npa), seq
      common /pnoqtx/ ldx(npa)
      character*4 krmp
      common /pnoqv/ krmp(npa)
      character*4 krms
      common /po/ krms(npa)
      common /pot/ ts(npa),ihr,model(npa), keyphi(npa)
      common /povx/ amx(npa)
      common /pox/ fmp(npa),prx(npa),icent1,icent2
      common /pt/ nlay(npa)
      common /pqt/ near
      common /reloc/ irelo, nreloc
      common /qmost/ wt(npa),z
      common /xfmno/ mag
      dimension ws(npa),wws(npa)
      character*1 ifmsm(8)
      character*36 scatph(nsn)
      data ifmsm/'c','d','+','-','d','c','-','+'/
      data scatp/0./, scats/0./, amplif/1.0/, fract/-1./
      data scatnow/.false./, icent2/0/
      if(icent2 .eq. 0) icent2 = itest(55)
      icent1 = icent2
      rewind (16)
      if (iprn .ge. 5) write(punt, '(a)') ' begin phasin'
      kdate = 0
      odmag = -10.
      setmast = .false.
      write(16, 19)
19    format('$beginning of phasin error file')
192   seq = '     '
      magke = ' '
      near = 1
      nfirst = 0
      iskip = 0
      pmin = 9999.
      do i = 1,ns
        jdx(i) = 0
      enddo
      nsktm = 0
      nsum = 0
      ntried = 0
      m = 0
      mm = 1
      mag = blank
      avxm = blank
      avfm = blank
c check value of eoff prior to calling phagt
21    if(eoff) goto 79
      call phagt(phcard, keyph, lph, inpt, injump, eoff, icent2, 
     *  itest)
cd    print *, 'phagt read ', lph, ' phase records'
      if(eoff) then
        if(inpt .eq. inmain) then
c         reading from batch input, so end of input job is near
          if(lph .le. 1) then
c this is the end
            write(punt, '(4a)') 
            write(logfil, *) 
     *      ' completed reading input phase file'
            if(punt .ne. logfil) then
              write(punt, '(4a)') 
     *        ' sound bell ', char(7), char(7), char(7)
              write(punt, *) 
     *        ' completed reading input phase file'
            endif
            goto 79
          else
c process one more event
            write(logfil, *) 'process one more event'
            if(punt .ne. logfil)
     *      write(punt, *) 'process one more event'
            goto 22
          endif
        else if(inpt .eq. injump) then
c         reading from jump file, so try main batch stream again
          eoff = .false.
          inpt = inmain
          write(logfil, 215)
          if(punt .ne. logfil)
     *    write(punt, 215)
215       format(' jump back to main input stream.')
          if(lph .eq. 1) goto 192
c         process the last jump event
          goto 22
        endif
      endif
22    continue
c
c first:  (mm = 1) - define time based on the 1st phase record
c second: (mm = 2) - try the first rejected time
25    do 27 l = 1, lph
      if( ( (keyph(l) .ge. 0)  .and. (mm .eq. 1) )  .or.
     *    ( (keyph(l) .eq. -4) .and. (mm .eq. 2) ) )then
        msta4 = phcard(l)(1:4)
        msta4 = dnstrg(rshft(msta4))
	comp = dnstrg(phcard(l)(6:6))

c for 4-letter codes, if the last letter is 'e' or 'n' then
c assume this is a horizontal component station!
	if(test(53) .eq. 1.0) then
	  if(msta4(1:1) .ne. ' ') then
	    if((msta4(4:4) .eq. 'n') .or. (msta4(4:4) .eq. 'e')) then
              comp = msta4(4:4)
	    endif
	  endif
	endif

	if((comp .eq. 'e') .or. (comp .eq. 'n')) then
	  msta(1) = msta4//comp
	else
	  msta(1) = msta4//'z'
	endif
c       skip station if not on station list
        if(phaind(msta(1), nsta, ns, i, ierr) .ne. 0) goto 27

	if((scatnow) .and. (scatph(i) .ne. ' ')) then
	  phcard(l)(18:24) = scatph(i)(18:24)
	  phcard(l)(32:36) = scatph(i)(32:36)
	endif

        read(phcard(l), 26, err = 265) jtime, jmin1, p1
26      format(bz, 9x, i8, i2, f5.2)
c define base time in minutes
        lbastm = jmin1
        ktime = jtime
        kdate = ktime/100
        ihr = ktime - kdate*100
	kday = kdate - (kdate/100)*100
c       kbstm = ihr*3600 + jmin1*60 + p1
        kbstm = 
     *    (kdate - (kdate/100)*100)*24*3600 + ihr*3600 + jmin1*60 + p1
        khrmn = 100*ihr + jmin1
        goto 28
c decode error
265     keyph(l) = -11
      endif
27    continue
28    m = 0
c
c begin main loop, dealing with each phase record ------------------
      do 78 l = 1, lph
cd    print *, ' '
cd    print *, 'main phasin loop, l = ', l
cd    print *, phcard(l)
c     phcard(l)(105:109) = dnstrg( phcard(l)(105:109) )
      msym(l) = ' '
c key for goto:  keyph   type of record
c                       0 --- phase record
c                      -1 --- comment record
c                      -2 --- summary record
c                      -3 --- instruction record
c                             (begins with more, dist, or 4 blanks)
c                      -4 --- bad time
c                      -5 --- reset record
c                      -6 --- save record - no longer used
c                      -7 --- rerun record - no longer used
c                      -8 --- scatter record
c                      -9 --- not on station list
c                     -10 --- jump record
c                     -11 --- decode error
c                     -12 --- sleep record
c                     -13 --- stop record
c                     -14 --- nongaus record
c		      -15 --- master scatter record
c
      if(keyph(l) .ge. 0) goto 29
c           comm  sum   inst  pha   reset save  rerun scatter
c           not-on jump  decode-er sleep stop nongaus master
      goto (78,   54,   56,   29,   62,   66,   66,   73,  
     *      29,    71,   76,       772,  774, 752,    757),
     *        abs(keyph(l))
c
c phase record
29      m = m + 1
        ldx(m) = 0
        keyph(l) = m
        keyphi(m) = l
        ksmp(m) = 1
cd      write(punt, '(a, i5)') 'beginning to process phase record # ', m
c if there is a decimal in 59-62, then this is an old c10 value
c so delete
        do 291 i = 59, 62
          if(phcard(l)(i:i) .eq. '.') then
            phcard(l)(59:62) = '    '
            goto 292
          endif
291     continue
292     msta4 = phcard(l)(1:4)
        msta4 = dnstrg(rshft(msta4))
	comp = dnstrg(phcard(l)(6:6))

c for 4-letter codes, if the last letter is 'e' or 'n' then
c assume this is a horizontal component station!

	if(test(53) .eq. 1.0) then
	  if(msta4(1:1) .ne. ' ') then
	    if((msta4(4:4) .eq. 'n') .or. (msta4(4:4) .eq. 'e')) then
              comp = msta4(4:4)
	    endif
	  endif
	endif

	if((comp .eq. 'n') .or. (comp .eq. 'e')) then
	  msta(m) = msta4//comp
	else
	  msta(m) = msta4//'z'
	endif
c find station in station list
        if(phaind(msta(m), nsta, ns, i, ierr) .ne. 0) then
          iskip = iskip + 1
          if(iskip .gt. 200) then
            write(logfil,83)
            if(punt .ne. logfil) write(punt,83)
            stop 'abort from phasin'
          endif
          write(16, 34) msta(m), phcard(l)
34        format(' ***>',a5, ' is not on station list, so next record ',
     *    ' will not be used:', /, a)
          keyph(l) = -9
          m = m - 1
          goto 78
        endif
c       kdx(phase number) = station number
        kdx(m) = i
	if(setmast) scatph(i) = phcard(l)
        if(setmast) write(punt, '(a, i5, 2a)') 
     *  ' station ', i, ' phase set to ', scatph(i)

c
c if this is a scatter run, where all events will have the same
c fixed arrival times but different added errors, change the p and s
c arrival times to the fixed values now.
	if((scatnow) .and. (scatph(i) .ne. ' ')) then
c         assume at this time that the yr mo dy hr of each phase is the same!
	  if(phcard(l)(18:24) .ne. ' ')
     *	   phcard(l)(18:24) = scatph(i)(18:24)
	  if(phcard(l)(32:36) .ne. ' ')
     *	   phcard(l)(32:36) = scatph(i)(32:36)
cd	  write(punt, '(a)') ' reset phase to: ', phcard(l)(1:36)
	endif
	

        read(phcard(l), 30, err = 76 )
     *  krmp(m),jtime,jmin(m),p(m),s(m),
     *  krms(m),amx(m),prx(m),krm(m),dt(m),fmp(m),
     *  poldev, pdev
30      format(bz, 4x, a4, 1x, i8, i2, f5.2, 7x, f5.2, a4, 3x, f4.0,
     *  f3.2, 12x, a2, 1x, f5.2, f5.0, 29x, 2a1)
        if(poldev .eq. ' ') poldev = bksrc
        poldev = dnstrg(poldev)
        if(pdev .eq. ' ') pdev = bksrc
        pdev = dnstrg(pdev)
        if(fmp(m) .le. 0.) fmp(m) = blank
        read(phcard(l), 31, err = 76)
     *        msym(m),ipwc,nlay(m),    iswc
31      format(6x, a1,  i1,     i1, 30x, i1)
cd      print *, 'original ipwc = ', ipwc
c convert msym to lower case
        msym(m) = dnstrg(msym(m))
c fix up amplitude.  if negative, then multiply by -10000
        if(amx(m) .lt. 0.) amx(m) = -amx(m)*10000.
c change first motion symbol to blank for horizontals
c (convention is to use n for north-south component, and e for e-w)
c also, remove magnitude information from horizontals
        if((dnstrg(phcard(l)(6:6)) .eq. 'n') .or.
     *  (dnstrg(phcard(l)(6:6)) .eq. 'e')) then
          fmp(m) = blank
          msym(m) = ' '
          amx(m) = 0.0
        endif

        if((scatp .ne. 0.0) .and. (.not. setmast)) then
          if(phcard(l)(20:24) .ne. '     ') then
c decide if this is the 1 in 10 with 10x greater error!
            fctor = 1.0
            if(ipwc .lt. 4) fctor =  rsew(ipwc+1)
            tail = ran3(0)
            if(tail .le. fract) then
              fctor = fctor*amplif
            endif 
            p(m) = p(m) + rnd()*scatp*fctor*fctmagp
            call riorbk(p(m), ipm, fmit, 5, 2)
            write(phcard(l)(20:24), fmit) ipm
          endif
        endif
        if((scats .ne. 0.0) .and. (.not. setmast)) then
          if(phcard(l)(32:36) .ne. '     ') then
            fctor = 1.0
            if(iswc .lt. 4) fctor =  rsew(iswc+1)
            tail = ran3(0)
            if(tail .le. fract) then
              fctor = fctor*amplif
            endif 
            s(m) = s(m) + rnd()*scats*fctor*fctmags
            call riorbk(s(m), ism, fmit, 5, 2)
            write(phcard(l)(32:36), fmit) ism
          endif
        endif


c compare time of current station to first station's time
c first compute equivalent of kbstm to see what offset is in seconds
	jhr = jtime - (jtime/100)*100
	jdate = jtime/100
	jday = jdate - (jdate/100)*100
	jbstm = 
     *  (jdate - (jdate/100)*100)*24*3600 + jhr*3600 + jmin(m)*60 + p(m)
	if((iabs(jbstm - kbstm) .gt. 600) .or. 
     *    (kdate/100 .ne. jdate/100)) then
          write(16, 37) phcard(l)
37        format(' ***> the next phase record has a deviant time',
     *    ', so it will not be used:', /, a)
          keyph(l) = -4
          m = m - 1
          nsktm = nsktm + 1
          if(nsktm .gt. lph/2) then
            if(ntried .eq. 1) goto 78
            seq = '     '  
            magke = ' '
            norem = 0
            iskip = 0
            pmin = 9999.
            do 38 iii = 1, ns
              jdx(iii) = 0
38          continue
            nsum = 0
            mm = 2
            write(16, 39) nsktm, lph
39          format(' ***> skipped', i5, ' out of', i5,' records due to',/
     *      ' deviant times, so will try a new base time once')
            nsktm = 0
            ntried = 1
            goto 25
          endif
          goto 78
        endif
c
c adjust jmin(m) for possible difference in day or hour
	jmin(m) = jmin(m) + (jday - kday)*24*60 + (jhr - ihr)*60

c       make sure station record has not expired.
40      ihrmn = 100*jhr + jmin(m)
        call diftim(icent2,jdate,ihrmn,ndate(i),nhr(i),iporm,difhr)
        if(difhr .gt. 0) then
c station has expired
c
cd        write(punt, '(a, 2i10)') ' expired on ', ndate(i), nhr(i)
      call update(indexs, nsta, ielv,
     *         lat, lon, c, e, sw,
     *         klas, calr, xmgc, fmwt, xmwt, fmgc, ipcod, iscod, tpdly,
     *         revp, ndate, nhr, ns,
     *         exdly, icent2, kdate, ihrmn,
     *         infil, iofil)
cd          write(punt,333) indexs,nsta(i),lat(i),lon(i),kdate,ihrmn,
cd   1     sw(i),klas(1, i),calr(1, i),xmgc(i),xmwt(i),fmwt(i),
cd   2     fmgc(i),ipcod(i),iscod(i),tpdly(1,i),(exdly(kk,i),kk=1,4),
cd   2     tpdly(2,i),(revp(kk,i), kk = 1, 6), ndate(i),
cd   3     nhr(i),ielv(i),mod(i),ipthk(i),vthk(1,i),vthk(2,i),
cd   4     ipdly(i),dly(1,i),dly(2,i),dly(3,i),dly(4,i),dly(5,i),
cd   5     sdly(1,i),sdly(2,i),sdly(3,i),sdly(4,i),sdly(5,i),
cd   6     c(i),e(i),prr(i)
cd333      format(       1x,i3,     a4,        2f10.2,       2i10,/
cd   1     1x,f5.2,        i5,                   2f10.2, 2i5/
cd   2     1x,f10.2,            2i10,      f6.2,                  4a1,
cd   2         f6.2, /, 1x,               6a1,      i10,/
cd   3                             1x,4i10,              2f10.2,/
cd   4       1x,i10,                                     5f10.2,/
cd   5     1x,                                        5f10.2,/
cd   6     1x, 3f10.2/)
        endif
c*******
c for purposes of telemetry delay, some sources are equivalent
c (the same reduction is done on reading the station list)
c reduce all sources equivalent to usgs develocorder to 'v'
        if(pdev .eq. '*' .or. pdev .eq. '1' .or. pdev .eq. '4')
     *  pdev = 'v'
c reduce all sources equivalent to uagi develocorder to 'f'
        if ( pdev .eq. '%' .or. pdev .eq. 'a' ) pdev = 'f'
c reduce all sources equivalent to usgs mag tape to 's'
        if (pdev .eq. 'e' .or. pdev .eq. '2') pdev = 's'
c reduce all sources equivalent to uagi daq/dan or pc's to 'd'
c       if (pdev .eq. 'j' .or. pdev .eq. 'x'
c do not include 'p' because yakutat pc is 'p' and has no satellite delay. jcl 6/3/94
cc   *  .or. pdev .eq. 'p'
c    *  .or. pdev .eq. 'o' .or. pdev .eq. 'g'
c    *  .or. pdev .eq. 'k' .or. pdev .eq. 'i'
c    *  .or. pdev .eq. 'u') pdev = 'd'
c reduce all sources equivalent to uagi daq/dan 'd'
        if (pdev .eq. 'j' .or. pdev .eq. 'x') pdev = 'd'
c reduce all sources equivalent to uagi pc's to 'g'
        if (pdev .eq. 'o' 
     *  .or. pdev .eq. 'k' .or. pdev .eq. 'i'
     *  .or. pdev .eq. 'u') pdev = 'g'
c nhop related code has been removed, now that the station history
c allows more than one telemetry delay (tpdly) per station
c must fix up history to allow for no delay for yakutat pcelog
c         if(phcard(l)(110:110) .ne. ' ') then
c         read(phcard(l)(110:110), '(i1)') nhop
c         tpuse = -nhop*.27
c check the source of the p-phase (pdev) to see if tpdly 1 or 2 should be used
c use second delay if source matches any one of the 4 extra codes
c otherwise use the first delay
        if (pdev .eq. exdly(1, i) .or. pdev .eq. exdly(2, i) .or.
     *      pdev .eq. exdly(3, i) .or. pdev .eq. exdly(4, i)) then
          itpused(m) = 2
          tpuse = tpdly(itpused(m), i)
        else if ((pdev .eq. 't') .or. (pdev .eq. 'n')) then
c         for atwc teletype source "t", set telemetry delay to zero
c         for neis source "n", set telemetry delay to zero
          itpused(m) = 0
          tpuse = 0.0
        else
          itpused(m) = 1
          tpuse = tpdly(itpused(m), i)
        endif
        dt(m) = dt(m) + tpuse
cd        write(punt, '(a)') 'exdly, pdev, itpused(m), tpdly(1,i), tpdly(2,i)'
cd        write(punt, '(4a, 1x, a, i2, 2f6.2)') (exdly(kk, i), kk=1,4),
cd   *    pdev, itpused(m), tpdly(1,i), tpdly(2,i)
c******************************************************************
c take account of polarity reversals
c allow lower case, etc
        if(msym(m) .eq. 'u') msym(m) = 'c'
        if(msym(m) .eq. '.') msym(m) = ' '
        if(msym(m) .eq. 'c' .or. msym(m) .eq. 'd' .or.
     *     msym(m) .eq. '+' .or. msym(m) .eq. '-') then
c reduce all sources equivalent to usgs develocorder to 'v'
          if(poldev .eq. '*' .or. poldev .eq. '1' .or. poldev .eq. '4')
     *      poldev = 'v'
c reduce all sources equivalent to uagi develocorder to 'f'
          if ( poldev .eq. '%' .or. poldev .eq. 'a')
     *      poldev = 'f'
c reduce all sources equivalent to usgs mag tape to 's'
          if (poldev .eq. 'e' .or. poldev .eq. '2'
     *      .or. poldev .eq. 'p' .or. poldev .eq. 'o'
     *      .or. poldev .eq. 'g' .or. poldev .eq. 'k'
     *      .or. poldev .eq. 'i' .or. poldev .eq. 'u')
     *      poldev = 's'
c reduce all sources equivalent to uagi daq/dan
          if (poldev .eq. 'j' .or. poldev .eq. 'x')
     *      poldev = 'd'
          if (poldev .eq. 'v') then
            revers = revp(1, i)
          else if (poldev .eq. 's') then
            revers = revp(2, i)
          else if (poldev .eq. 'w') then
            revers = revp(3, i)
          else if (poldev .eq. 'f') then
            revers = revp(4, i)
          else if (poldev .eq. 'd') then
            revers = revp(5, i)
          else
c any other polarity device (could include ' ' if blank source = ' ')
            revers = revp(6, i)
          endif
c fix first motion symbols if reversed or uncertain
          if      ( revers .eq. 'n' .or. revers .eq. '+') then
c normal or probably normal polarity, so do nothing
          else if ( revers .eq. 'r' .or. revers .eq. '-') then
c reversed or probably reversed polarity, so reverse reading
            do 44 ifm = 1, 4
              if(msym(m) .eq. ifmsm(ifm)) then
                msym(m) = ifmsm(ifm + 4)
                goto 46
              endif
44          continue
46          continue
          else if ( revers .eq. ' ') then
c assume, for now, that blank polarity reversal indicator = probably normal
          else
c unknown is the only possibility left, so do not use reading
            msym(m) = '?'
          endif
        else if (msym(m) .eq. ' ' .or. msym(m) .eq. 'z' .or.
     *           msym(m) .eq. 'n') then
c blank, nodal, or noisy, so do nothing
        else
c any other code, set equal to ?
          write(16, 3) nsta(i), msym(m)
    3     format(/' station ', a4, ' first motion symbol (', a1,
     *    ') is not correct.  Reset to "?"')
          msym(m) = '?'
        endif
c put the corrected first motion into column 65 of the phase record
        phcard(l)(65:65) = msym(m)
        if((msym(m) .eq. 'c') .or. (msym(m) .eq. 'd') .or. 
     *     (msym(m) .eq. '+') .or.
     *     (msym(m) .eq. '-')) nfirst = nfirst + 1
c ldx = 0 if there is no s arrival or if the s arrival is
c used in an s-p interval.
        ldx(m) = 0
        jdx(i) = 1
        if(ndate(i) .eq. 99999998) then
c error, station has expired
          write(16, 466) nsta(i), kdate, ihrmn
466       format(' ***> xxxerrorxxx ', a4, ' has expired ',
     *    'and can not be used on ',i6.6,1x,i4)
          keyph(l) = -4
          m = m - 1
          goto 78
        endif
467     tp(m) = 60.*(jmin(m)-lbastm)+p(m)+dt(m)
        kwr(m) = '    '
        iqdo(m) = '    '
c
c fix up weights
c force weight code if ipcod or iscod on station record is not equal 10
c but set ww and wws equal to weights that would have been used
c do not use reading if field is blank
        if(phcard(l)(20:24) .eq. '     ') then
          ipwc = 8
cd       print *, 'ipwc changed to ', ipwc
          phcard(l)(8:8) = '4'
          krmp(m)(4:4) = '4'
        endif
        if(phcard(l)(32:36) .eq. '     ') then
          iswc = 4
        else
c set up s arrival pointer and time
          ldx(m) = 1
          ts(m) = 60.*(jmin(m)-lbastm)+s(m)+dt(m)
        endif
        ww(m) = 0.0
        wws(m) = 0.0
cd     print *, 'station weight sw(i) = ', sw(i)
        if(sw(i) .eq. 0.) then
c in this case, save original weights and ignore ipcod & iscod
c ignore for now case of s-p interval
          if(ipwc .lt. 4) ww(m) = rsew(ipwc+1)**(-2)
          w(m) = 0.
          ipwc = 5
          if(iswc .lt. 4) wws(m) = rsew(iswc+1)**(-2)
          ws(m) = 0.
          iswc = 5
cd       print *, 'w(m), ws(m) = ', w(m), ws(m)
          goto 78
        endif
c
cd     print *, 'p and s replacement codes = ', ipcod(i), iscod(i)
cd     print *, 'ipwc, iswc = ', ipwc, iswc
        if((ipwc .lt. 4) .or. (ipwc .eq. 9)) then
c if the reading was to be used, then check ipcod
          if(ipcod(i) .eq. 10) then
          else if((ipcod(i) .gt. 3) .and. (ipcod(i) .lt. 9)) then
c keep track of original weight for lissum (unless it was 9)
            if(ipwc .ne. 9) ww(m) = rsew(ipwc+1)**(-2)
            ipwc = ipcod(i)
          else if(ipcod(i) .eq. 9) then
            ipwc = 9
          else if(ipcod(i) .lt. 4) then
            ipwc = ipcod(i)
          endif
        endif
        if(iswc .lt. 4) then
c if the reading was to be used, then check iscod
          if(iscod(i) .eq. 10) then
          else if(iscod(i) .gt. 3) then
            wws(m) = test(39) * (rsew(iswc+1)**(-2))
            iswc = iscod(i)
          else if(iscod(i) .lt. 4) then
            iswc = iscod(i)
          endif
        endif
c
        if(ipwc .le. 3) then
c normal p arrival
          w(m) = rsew(ipwc+1)**(-2)
cd	  print *, 'normal p arrival ipwc, w(m) = ', ipwc, w(m)
c find earliest arrival time with w(m) .ne. 0.
          if(tp(m) .lt. pmin) then
            pmin = tp(m)
            near = m
          endif
        else if(ipwc .eq. 9) then
c
c s-p interval
cd       print *, 's-p interval'
          ksmp(m) = 0
c reset ldx, because s is being used in s-p interval
          ldx(m) = 0
          ts(m) = 60.*(jmin(m)-lbastm)+s(m)+dt(m)
c in this case use s weight code for setting weight of s-p interval
c normal s-p case in which there is an s phase arrival
          ws(m) = 0.0
          if(iswc .ge. 4) then
            w(m) = 0.0
          else
            w(m) = test(39) * (rsew(iswc+1)**(-2))
          endif
c do not use negative or zero s-p interval
          if(ts(m) .le. tp(m)) then
            w(m) = 0.0
          endif
          if( (tp(m) .lt. pmin) .and. (w(m) .gt. 0.0) ) then
            pmin = tp(m)
          endif
cd       print *, 's-p interval weight = w(m) = ', w(m)
          goto 78
        else
c
c p arrival with weight code of 4,5,6,7, or 8.
          w(m) = 0.
cd       print *, 'code of 4-8 gives w(m) = ', w(m)
        endif
c
c s arrival (except for s-p case that never reaches here)
        if(iswc .lt. 4) then
          ws(m) = test(39) * (rsew(iswc+1)**(-2))
        else
          ws(m) = 0.
        endif
        goto 78
c
c summary record
54      continue
        nsum = nsum + 1
	write(punt, '(a, i5)') 'nsum = ', nsum
        if(nsum .eq. 1) then
          seq = phcard(l)(92:96)
          evstat = phcard(l)(72:72)
          evtype = phcard(l)(90:90)
c only read date and hrmn if none was found on the phase records
          if(kdate .eq. 0) then
 	    read(phcard(l), '(i6, i4, t35, f2.1)') kdate, khrmn, odmag
	  else
 	    read(phcard(l), '(t35, f2.1)') odmag
	  endif
          magke = dnstrg(phcard(1)(78:78))
        endif
c
c special code for redoubt study - vary errors with magnitude
	if (scatp .lt. 0.0) then
          fctmagp = 0.005 + 0.165*exp(-1.87*odmag)
          fctmags = fctmagp
	  write(punt, '(a, 2f10.3, a, f5.2)') 
     *    ' add errors to p & s with standard errors = ', 
     *    fctmagp, scats*fctmags, ' for mag = ', odmag
	endif
c end of special section
c
        if((nsum .gt. 1) .or. (igsum .eq. 0)) goto 78
	if(magke .eq. 'k') then
c this is a "fake" summary record, so ignore starting location
	  malor = ' '
	  malla = ' '
	  malaz = ' '
	  goto 78
	endif
        read(phcard(l), 55)
     *  org1,org2,ala1,isnla,ala2,alo1,isnlo,alo2,zres,zres1
55      format(bz,8x,f2.0,f4.2,f2.0,a1,f4.2,f3.0,a1,f4.2,f5.2,
     *    t111, f5.2)
        isnla = dnstrg(isnla)
        isnlo = dnstrg(isnlo)
        malor = phcard(l)(9:14)
        malla = phcard(l)(15:18)
        malaz = phcard(l)(31:34)
 	if(phcard(l)(111:115) .ne. ' ') then
	  malaz = phcard(l)(112:115)
	  zres = zres1
	endif
c	write(punt, '(a, i2, a)') 'phcard(', l, ') is a summary record'
c	write(punt, '(a)') phcard(l)
c	write(punt, '(4a)') 'malor, malla, malaz = ', 
c    *    malor, malla, malaz
        goto 78
c
c instruction record
56      read(phcard(l), 57)
     *  ipro,ichec,knst,inst,szres,sla1,jsnla,sla2,slo1,jsnlo,slo2,
     *  srg1,srg2
57      format(2a4,9x,2i1,f5.2,16x,f2.0,a1,f5.2,5x,f3.0,a1,f5.2,11x,
     *  f2.0,f5.2)
        ipro = dnstrg(ipro)
        ichec = dnstrg(ichec)
        jsnla = dnstrg(jsnla)
        jsnlo = dnstrg(jsnlo)
	if(test(38) .eq. 4) then
	  knst = 1
	  inst = 7
	endif
        if (nsum .eq. 0) then
          evstat = phcard(l)(9:9)
          evtype = phcard(l)(10:10)
	  icent2 = itest(55)
        endif
        mamaz = phcard(l)(21:24)
        mamla = phcard(l)(43:46)
        mamor = phcard(l)(74:79)
        seqins = phcard(l)(92:96)
58      if(seq .eq. '     ') seq = seqins
c if the instruction rec is blank and there is a summary record
c     and the summary record was not ignored then use the summary
c     record.  conversely, if the instruction record is not blank or
c     there was no summary record or the summary record was ignored,
c     then use the instruction record.
59      if((mamla .eq. '    ') .and. (nsum .ge. 1) .and.
     *  (igsum .ne. 0)) goto 60
        malla = mamla
        ala1 = sla1
        isnla = jsnla
        ala2 = sla2
        alo1 = slo1
        isnlo = jsnlo
        alo2 = slo2
60      if((mamor .eq. ' ') .and. (nsum .ge. 1) .and.
     *  (igsum .ne. 0)) goto 61
        malor = mamor
        org1 = srg1
        org2 = srg2
61      if((mamaz .eq. '    ') .and. (nsum .ge. 1) .and.
     *  (igsum .ne. 0)) goto 80
        zres = szres
        malaz = mamaz
        goto 80
c
c reset record
62      continue
        if(inpt .eq. injump) then
c error- can not reset from jump file
          write(logfil, 622)
          if(punt .ne. logfil)
     *    write(punt, 622)
622       format(' xxxerrorxxx can not use a reset record in ',
     *    'a jump file')
          stop 'abort from phasin'
        endif
        write(punt, '(1x, a)') phcard(l)
        ipro = dnstrg(phcard(l)(1:4))
        ichec = dnstrg(phcard(l)(5:8))
        nr = lph
        if(nr .eq. 1) goto 90
        write(logfil,65) phcard(l)
        if(punt .ne. logfil)
     *  write(punt,65) phcard(l)
65      format(' ***** logic error at statement 28 of phasin *****',
     *  /,1x,a, /, ' so stop')
        stop 'abort from phasin'
c
c save and rerun
66      write(logfil, 67)
        if(punt .ne. logfil)
     *  write(punt, 67)
67      format(' xxxerrorxxx save and rerun are no longer valid ',
     *  'options.')
        stop 'abort from phasin'
c
c jump record
71      if(l .ne. injump) then
          write(logfil, 712)
          if(punt .ne. logfil)
     *    write(punt, 712)
712       format(' xxxerrorxxx jump record out of place.')
          stop 'abort from phasin'
        endif
        goto 21
c
c scatter record
73      read(phcard(l), 74) scatp, scats
74      format(19x, f5.2, 7x, f5.2)
	if(scatp .ge. 0.) then
	  fctmagp = 1.0
	  fctmags = 1.0
          write(punt,75) scatp,scats
75        format(1x,'scatter: p and s standard errors = ', 2f10.3)
	else
	  scatp = -1.0
          write(punt,'(a, /, a, f10.3)') 
     *     ' set p std err = 0.005 + 0.165*exp(-1.87*odmag)',
     *     ' set s std err = (p std err) * ', scats
        endif
        goto 21
c
c nongaus record
752     read(phcard(l), 74) fract, amplif
754     format(19x, f5.2, 7x, f5.2)
        write(punt,75) amplif, fract
756     format(1x,'nongaus: multiply p and s standard errors by ', 
     *  f10.3, 1x, f10.3, ' fraction of the time.')
        goto 21
c
c master scatter record
757     setmast = .true.
	scatnow = .false.
	write(punt, '(a)') 
     *  ' use this event as a master for random scatter.'
	do 758 i = 1, ns
	  scatph(i) = ' '
758	continue
	goto 21
c
c decode error in phase record
76      write(16, 77) phcard(l)
77      format(' ***> decode error reading this phase record:',/,1x, a)
        if(keyph(l) .ne. -11) then
          keyph(l) = -11
          m = m - 1
          goto 78
        endif
c
c sleep record
772     write(punt, '(1x, a)') phcard(l)
        nr = lph
        ipro = dnstrg(phcard(l)(1:4))
        ichec = dnstrg(phcard(l)(5:8))
        if(nr .eq. 1) goto 90
        write(punt,'(a,/,a)')
     *     ' *** error in phase data input structure ***',
     *     'sleep must be the first record of a new event'
        write(logfil,'(a,/,a)')
     *     ' ** error in phase data input structure **',
     *     'sleep must be the first record of a new event'
        stop 'abort from phasin'
c
c stop record
774     write(punt, '(1x, a)') phcard(l)
        nr = lph
        ipro = dnstrg(phcard(l)(1:4))
        ichec = dnstrg(phcard(l)(5:8))
        if(nr .eq. 1) goto 90
        write(punt,'(a,/,a)')
     *     ' *** error in phase data input structure ***',
     *     'stop must be the first record of a new event'
        write(logfil,'(a,/,a)')
     *     ' ** error in phase data input structure **',
     *     'stop must be the first record of a new event'
        stop 'abort from phasin'
c
78    continue
c
c  end of main loop -------------------------------------------------
c     if m = 0, then terminate program
c
79    nr = m
      if(nr .ge. 1) goto 84
      goto 90
c
80    nr = m
      if((nr .eq. 0) .and. (lph .gt. 1)) goto 90
      if(nr .ge. 1) goto 84
      write(logfil,81) phcard(lph)
      if(punt .ne. logfil)
     *write(punt,81) phcard(lph)
81    format(' xxxx this record skipped - out of place xxxx ',/,a)
      iskip = iskip + 1
      if(iskip .le. 200) then
c write out this event so that the phase file is preserved, for
c whatever that is worth!
        if((ipun .eq. 2) .or. (ipun .eq. 3)) then
          do 815 l = 1, lph
            write(11, '(a)') phcard(l)
815       continue
        endif
        goto 21
      else
82      write(logfil,83)
        if(punt .ne. logfil)
     *  write(punt,83)
83      format(' more than 200 records skipped, so stop')
        stop 'abort from phasin'
      endif
c
84    nrp = nr
      if((itest(38) .eq. 1) .or. (itest(38) .eq. 2)) knst = 1
      if(itest(38) .eq. 3) knst = 0
c define starting savla and savlo
      if(malla .ne. '    ') then
c define starting location from summary record or instruction record
        la = ala1 + .00001
        lo = alo1 + .00001
        call fold2(savla,savlo,la,isnla,ala2,lo,isnlo,alo2)
      else if((abs(test(3))+abs(test(4))) .gt. 0.00001) then
c       define starting location from test(3) and test(4)
        savla = latr
        savlo = lonr
      else
c       define starting location from first 10 p arrival times
c       do this in main routine if savla=savlo=99999.
        savla = 99999.
      endif
c begin definition of starting depth
      savez = 99999.
      if(malaz .ne. '    ') then
        savez = zres + test(8)
      endif

c define starting origin time
      savor = 99999.
      freor = .true.
      if(malor .ne. ' ') then
        savor = 60.*(org1 - lbastm) + org2
        freor = .false.
      else if(inst .eq. 8) then
        inst = 0
      endif
      noswt = 0
      nopwt = 0
      nos = 0
      notim = 0
      otim = 0.0
      do 89 i = 1,nrp
      ji = kdx(i)
      w(i) = w(i)*sw(ji)
      if(w(i) .ne. 0.0) nopwt = nopwt + 1
      if(ldx(i) .ne. 0) then
c   add s data to end of p data matrix
        nos = nos+1
        nrs = nrp+nos
c       ldx(p phase number) = s phase number
        ldx(i) = nrs
        ldx(nrs) = 0
        nlay(nrs) = 0
        tp(nrs) = ts(i)
        ww(nrs) = wws(i)
        if(ws(i) .ne. 0.0) then
          if(ts(i) .lt. tp(i)) then
c           do not use p or s if s time is earlier than p time
	    write(16, '(2a)') 
     *       ' ***> do not use p or s because s is earlier than p for ',
     *       nsta(ji)
            wws(i) = 0.
            ws(i) = 0.
            ww(nrs) = 0.
            w(i) = 0.
            ww(i) = 0.
          endif
        endif
        w(nrs) = ws(i)*sw(ji)
        if(w(nrs) .ne. 0.) noswt = noswt + 1
        if((w(i) .ne. 0.) .and. (w(nrs) .ne. 0.)) then
c     calculate origin time from p and s time
          otim = otim + tp(i) - (tp(nrs) - tp(i))/(test(1) - 1.0)
          notim = notim + 1
        endif
        ksmp(nrs) = 1
        dt(nrs) = dt(i)
        jmin(nrs) = jmin(i)
        kdx(nrs) = kdx(i)
        kwr(nrs) = '    '
        iqdo(nrs) = '    '
        msta(nrs) = msta(i)
        p(nrs) = p(i)
        s(nrs) = s(i)
      endif
89    continue
      nr = nrp+nos
      if((notim .ne. 0) .and. (savor .eq. 99999.)) then
        savor = otim/notim
        if(test(38) .lt. 0.0) inst = 8
      endif
c convert inst=7 to inst=9 with origin free - this is
c the option to use for quary blasts with unknown origin time.
      if(inst .eq. 7) then
        inst = 9
        freor = .true.
      endif
90    write(16, '(a)') 'end'
      if(setmast) scatnow = .true.
      return
      end
c end phasin
