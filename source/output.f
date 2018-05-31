c output.for    []
      subroutine output(kp)
c---- output hypocenter information
      include 'params.inc' 
      parameter (ndly = 11)
      real bat2,bon2,latep,lonep,mag
      logical good, supout
c                 comp - z, n, or e component
      character*1 comp, dnstrg
c                 pha  - 3 letter phase description
      character*3 pha
      character*1 krmo, krm4, krm5, iprmk, iqs, iqd
      character erout*120, isorp*3
      character*133 fline, aline, bline, fmit*6, fmit1*6, fmit2*6, fmit3*24
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /dhip/ inpt,isa,ilis,inmain,injump
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /amo/ tempg(npa), ntgap
      common /anox/ keyd(npa)
      character*4 ipro, ichec, evtype*1, evstat*1, root*256
      common /dbhip/ ipro, ichec, evtype, evstat, root
      common /dbiln/ ioldq
      character*1 iclass
      common /dbio/ iclass(0:4)
      common /dhio/ nedit,good
      common /dio/ rmsmx,presmx,sresmx,noutmx,nimx,iprun,semx
      common /dmost/ ipun,ivlr,blank
      common /gmost/ az(npa)
      real*8 time1, time2
      common /hop/ time1,time2,nopu,notim
      logical freor
      common /hopq/ savla,savlo,savez,savor,freor
      common /hpn/ wslope
      common /ilpx/ calr(5, nsn),fmgc(nsn),xmgc(nsn)
      common /imost/ test(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /idno/ ksel,ksort
      common /ihfgpq/ itest(100)
      common /ilotv/ elvdly(npa)
      common /in/ irmo,iryr,odmag
      character*1  magke
      common /in1/ magke
      common /int/ thk(lmax+1),lbeg(mmax),lend(mmax),vi(lmax),vsq(lmax),
     *  vsqd(lmmax,lmax),f(lmax,lmmax),kl,ivway,sthk(mmax),sthk1(mmax),
     *  sdly(ndly,nsn)
      common /iot/ flt(2,nsn),thks(nsn)
      common /iox/ prr(nsn),iuses
      integer fmwt, xmwt
      common /ioxl/ fmwt(nsn), xmwt(nsn)
      common /logfil/ logfil
      common /it/ vpvs(lmax2),vpvsm(mmax+3),nttab
      common /omnfh/ dmin,dmin3,sminp
      common /onx/ kluse(npa), cluse(npa), peruse(npa), ampuse(npa)
      common /ox/ sysmag(npa), gndmot(npa) 
      common /obcfn/ ain(npa)
      common /ocfn/ adj,seh,lat1,bat2,lon1,bon2,igap
      character*1 isnla, isnlo, krm1, krm2
      common /ocfn1/ isnla, isnlo, krm1, krm2
      common /ofnv/ kmin
      character*1 nq
      common /ofnv1/ nq
      common /ohq/ gap, supout
      common /ohbl/ jav
      common /ofln/ sec
      common /olnx/ xmag(npa),fmag(npa),avxm,avfm,xmmed,fmmed
      common /on/ ix,iy,iz,ax1,axz
      character iqax*1
      common /on1/ iqax
      common /oqr/ y(4),kz,at(3),tl(3),pdrms,b(4)
      common /orz/ onf
      common /pfnoqv/ kdate,khrmn,khr
      character msym*1
      common /pfo/ msym(npa)
      common /pgoq/ s(npa)
      common /pgnoqv/ p(npa),jmin(npa)
      character*1 kwr, iqdo
      common /pbnoq/ kwr(npa),iqdo(npa)
      common /pgnov/ dt(npa)
      common /phoqn/ inst,knst
      common /pno/ lph,keyph(npa),nsum
      character phcard*117, krm*2, seq*5
      common /pno1/ phcard(npa), krm(npa), seq
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /pnoqtx/ ldx(npa)
      character*4 krmp
      common /pnoqv/ krmp(npa)
      character*4 krms
      common /po/ krms(npa)
      common /pot/ ts(npa),ihr,model(npa), keyphi(npa)
      common /povx/ amx(npa)
      common /pox/ fmp(npa),prx(npa),icent1,icent2
      common /pt/ nlay(npa)
      common /qmost/ wt(npa),z
      common /qmost1/ lonep,ni,latep
      common /qgo/ org
      common /qgnotx/ delta(npa)
      common /qo/ dmax,ndec,adjsq,iph
      common /rbno/ idip(3),iaaz(3),a(3,3)
      common /rfgnoq/ se(4)
      common /ro/ yse,seorg,phi
      common /rioq/ been,damp,dmpinc,igo
      common /rob/ v(4,4),noaxp
      common /rq/ xmean(4),avwt,aveaj
      common /tmost/ x(4,npa)
      common /tonxb/ t(npa),fms(npa)
      common /xo/ nm,sdxm,nf,sdfm
      character*1 kodsym
      common /xo1/ kodsym(npa)
      common /xfmno/ mag
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      dimension demp(npa)
      dimension pruse(npa),sruse(npa),keyi(npa)
      dimension vv(2,2), ael(3), vec(2,2), eval(2), key(npa)
      data dum/1.0/
      if(iprn .ge. 5) write(punt, '(a)') 'begin subroutine output'
      iqax = ' '
      noaxp = 0
      rpd = 1.74533e-2
c---- initialize some remarks and calculate magnitudes if necessary
      krm1 = ' '
      krmo = ' '
c---- onf is sum of p and s weights. if onf = 0. put * next to origin time
      if(onf .eq. 0.) krmo = '*'
      krm2 = ' '
c---- kz is the number of fixed component in regress
      if(kz .eq. 3) krm2 = '*'
c---- define ix, iy, and iz in order of dip
      ix = 1
      iy = 2
      iz = 3
      if(idip(1) .le. idip(2)) goto 600
      ix = 2
      iy = 1
  600 if(idip(iy) .le. idip(3)) goto 610
      iz = iy
      iy = 3
  610 if(idip(ix) .le. idip(iy)) goto 620
      k = ix
      ix = iy
      iy = k
  620 continue
c---- convert lat and long to degrees and minutes
c---- convert lat and long to degrees and minutes
      if(iprn .ge. 5) then
        write(punt, '(a)') ' convert lat and long to deg & min'
        write(punt, *) ' latep, lonep = ', latep, lonep   
      endif 
      call unfold2(latep,lonep,lat1,isnla,bat2,lon1,isnlo,bon2)
      if(iprn .ge. 5) write(punt, *) lat1, isnla, bat2, lon1, 
     *                               isnlo, bon2 
c---- calculate time
      khr = ihr
      kmin = lbastm
      sec = org
      kkdate = kdate
c     write(punt, '(a, 2i10, f10.2, 2i10)') 
c    *  'ihr, lbastm, org, kdate, kkdate',
c    *  ihr, lbastm, org, kdate, kkdate
      call tshift(kkdate,khr,kmin,sec,dum)
      adj = sqrt(adjsq)
      if((se(ix) .eq. blank) .or. (se(iy) .eq. blank)) then
        eqh = 10.0
      else
        eqh=sqrt(se(ix)**2+se(iy)**2)
      endif
c---- calculate gap
  431 j=0
      do 10 i = 1,nrp
        if (wt(i) .gt. 0.) then
c         continue on because there is p data
        else
c         check for s data
          if (ldx(i) .eq. 0) then
c           no s data
            goto 10
          else
c           if the s data has zero weight, skip it
            if (wt(ldx(i)) .eq. 0.) goto 10
          endif
        endif
        j=j+1
        tempg(j)=az(i)
   10 continue
      ntgap = j
      gap = 360.
      if(j .lt. 2) goto 22
      call sort(tempg,key,j)
      gap=tempg(1)+360.-tempg(j)
      do 20 i=2,j
      dtemp=tempg(i)-tempg(i-1)
      if(dtemp .gt. gap) gap=dtemp
   20 continue
   22 igap = gap + 0.5001
c---- calculate minimum distance
c---- sruse and pruse are arrays of s and p residuals used in solution
c---- nwtout is number of readings weighted out by the program
      iminp = 1
      dminp = 999.
      imins = 1
      dmins = 999.
      sruse(1) = 0.0
      pruse(1) = 0.0
      nwtout = 0
      iwt = 0
      ipwt = 0
      iswt = 0
      do 26 i = 1,nrp
        ianywt = 0
        kk = ldx(i)
        if(wt(i) .eq. 0.0) goto 23
        ianywt = 1
        ipwt = ipwt + 1
        if(delta(i) .lt. dminp) then
          dminp = delta(i)
          iminp = i
        endif
        pruse(ipwt) = abs(x(4,i))
   23   if (kwr(i) .ne. ' ' .and. dnstrg(kwr(i)) .ne. 'g') 
     *   nwtout = nwtout + 1
        if(kk .eq. 0) goto 24
        if (kwr(kk) .ne. ' ' .and. dnstrg(kwr(i)) .ne. 'g') 
     *   nwtout = nwtout + 1
        if(wt(kk) .eq. 0.0)  goto 24
        ianywt = 1
        iswt = iswt + 1
        if(delta(i) .lt. dmins) then
          dmins = delta(i)
          imins = i
        endif
        sruse(iswt) = abs(x(4,kk))
   24   if(ianywt .eq. 0) goto 26
        iwt = iwt + 1
        demp(iwt) = delta(i)
   26 continue
      dmin = dminp
      if(dmins .lt. dmin) dmin = dmins
      dmin3=dmin
      if(iwt .gt. 1) call sort(demp,key,iwt)
      if(iwt .ge. 2) dmin3 = demp(2)
      if(iwt .ge. 3) dmin3 = demp(3)
c---- compute s minus p time for closest station used in solution
      sminp = blank
      if((ipwt .eq. 0) .or. (iswt .eq. 0)) goto 950
c---- dminp/dmins is distance to closest station with p/s used
c---- iminp/imins is index of the closest stations with p/s
      if(msta(iminp) .ne. msta(imins)) goto 950
      sminp = s(imins) - p(iminp)
      if((sminp .gt. 99.9) .or. (sminp .lt. 0.)) sminp = 99.9
  950 if(ipwt .eq. 0) ipwt = 1
      if(iswt .eq. 0) iswt = 1
c---- calculate magnitude
      mag = blank
      if((iprn .ge. 2) .or. (kp .eq. 1)) call xfmags
      rmag = mag
c---- calculate q, iqs, and iqd
      ofd=z
      tfd=2.*z
      if(ofd .lt. 5.) ofd=5.
      if(tfd .lt. 10.) tfd=10.
      js=4
      if((rms.lt.0.50).and.(eqh.le.5.0)) js=3
      if(se(iz) .eq. blank) goto 28
      if((rms.lt.0.30).and.(eqh.le.2.5).and.(se(iz).le.5.0)) js=2
      if((rms.lt.0.15).and.(eqh.le.1.0).and.(se(iz).le.2.0)) js=1
   28 jd = 4
      if(nrwt .lt. 6) goto 30
      if((gap.le.180.).and.(dmin.le.50.)) jd=3
      if((gap.le.135.).and.(dmin.le.tfd)) jd=2
      if((gap.le. 90.).and.(dmin.le.ofd)) jd=1
   30 jav=(js+jd+1)/2
      nq=iclass(jav)
      iqs=iclass(js)
      iqd=iclass(jd)
      idmin=dmin + .5
c---- write heading and check if eq is out of order
      time2=1.d+0*sec+1.d+02*kmin+1.d+04*khr+1.d+06*kkdate
      if(iprn .lt. 0) goto 720
      if((iprn .eq. 0) .or. (inst .eq. 9)) goto 52
      if(ni .ne. 1) goto 60
      if(ndec .ge. 1) goto 60
   52 continue
      if((icent2 .eq. icent1 .and. time2 .lt. time1 - 20.d0) .or.
     *  (icent2 .lt. icent1)) then
        write(punt,54) icent2, kkdate, khr, kmin, sec
        write(logfil,54) icent2, kkdate, khr, kmin, sec
   54   format(' xxxxx this event is out of order xxxxx', /,
     *  i2, i6.6, 1x, i2, ':', i2, 1x, f6.2)
      endif
c---- write step output
   60 if(iprn .eq. 0) goto 720
      if(iph .eq. 1) goto 62
c---- step line  output
      write(punt,61)
   61 format(/,84x,
     *'( adjustments )(adjst.  taken)', /,
     *'  i  lat long depth    rms   prms    damp     no ',
     *'  phi  --------eigenvalues---------',
     *' dlat dlon   dz dlat dlon   dz')
      if(iprn .eq. 1) iph=1
c
   62 continue
      aline = ' '
c      write(aline, 162) ni,bat2,bon2,z-test(8),krm2,rms,nrwt
c  162 format(1x ,i2, f5.1, f5.1, f6.1, a1, f7.4, i3)
      write(aline, 162) ni,bat2,bon2,z-test(8),krm2
  162 format(1x ,i2, f5.1, f5.1, f6.1, a1)
c
      call formit(rms, rrms, fmit, 6, 1)
      write(aline(22:27), fmit) rrms

      if (pdrms .ne. blank) then
        call formit(pdrms, rpdrms, fmit, 6, 1)
        write(aline(29:34), fmit) rpdrms
      endif
c
      write(aline(36:42), '(e7.2)') damp
      write(aline(46:84), '(i3, 1x, f5.1, 3(1x, e9.4))') 
     *  nrwt, phi, a(1,1), a(2,2), a(3,3)
c
      if (b(2) .ne. blank) then
        call formit(b(2), rb, fmit, 4, 1)
        write(aline(86:89), fmit) rb
      endif
c
      if (b(1) .ne. blank) then
        call formit(b(1), rb, fmit, 4, 1)
        write(aline(91:94), fmit) rb
      endif
c
      if (b(3) .ne. blank) then
        call formit(b(3), rb, fmit, 4, 1)
        write(aline(96:99), fmit) rb
      endif
c
      call formit(y(2), rb, fmit, 4, 1)
      write(aline(101:104), fmit) rb
      call formit(y(1), rb, fmit, 4, 1)
      write(aline(106:109), fmit) rb
      call formit(y(3), rb, fmit, 4, 1)
      write(aline(111:114), fmit) rb

      if( (ni .eq. 1) .or. (ni .eq. itest(37)) )
     *  write(punt, 64) lat1, lon1
   64 format(3x, 2i5)
c
c     step output
      write(punt, '(a132)') aline
c
      if((test(41) .eq. 1.0) .and. (iabs(ipun) .eq. 1))
     *call npunch('final')
      if((kp .eq. 0) .and. (iprn .lt. 3)) goto 100
c---- calculate single variable standard deviations
c---- first calculate shadow on surface
c-------- vv(1,1)*x**2 + vv(1,2)*x*y + vv(2,2)*y**2 = 1
  720 if(v(3,3) .lt. 0.000009) v(3,3) = 0.000009
      vv(1, 1) = (v(1,1)-v(1,3)**2/v(3,3))
      vv(1, 2) =    (v(1,2)-v(1,3)*v(2,3)/v(3,3))
      vv(2, 1) = vv(1, 2)
      vv(2, 2) = (v(2,2)-v(2,3)**2/v(3,3))
c---- compute principal axis
c---- find eigenvalues [eval(2)] and eigenvectors [vec(2,2)] for the
c     upper left nxn portion of vv(2, 2)
      call eigen1(ael, 2, 2, 2, vec, eval, vv, 0.0)
c
      if(eval(2) .lt. 0.000009) then
        ax1 = 99.
      else
        ax1 = yse/sqrt(eval(2))
        if(ax1 .gt. 99.) ax1 = 99.
      endif
c
      if(eval(1) .lt. 0.000009) then
        ax2 = 99.
      else
        ax2 = yse/sqrt(eval(1))
        if(ax2 .gt. 99.) ax2 = 99.
      endif
c
      aze2 = atan2(-vec(1,1),vec(2,1))/rpd
      aze1 = atan2(-vec(1,2),vec(2,2))/rpd
c
c---- compute maximum in z (standard error of z)
      if((v(1,1) .lt. 0.000009) .or. (nrwt .le. 3)) then
        axz = 99.
      else
        dnom = v(2,2) - v(1,2)*v(1,2)/v(1,1)
        if(dnom .lt. 0.000009) then
          axz = 99.
        else
          prob =  v(3,3) - v(1,3)**2/v(1,1)
     *    -( v(2,3) - v(1,3)*v(1,2)/v(1,1) )**2/dnom
          if(prob .lt. 0.000009) then
            axz = 99.
          else
            axz = yse/sqrt(prob)
            if(axz .gt. 99.) axz = 99.
          endif
        endif
      endif
c---- calculate quality based upon ax1 and ax2
      jq = 4
      bigst = axz
      if(ax1 .gt. axz) bigst = ax1
      if(bigst .le. 5.35) jq = 3
      if(bigst .le. 2.67) jq = 2
      if(bigst .le. 1.34) jq = 1
      iqax = iclass(jq)
      if(ioldq .eq. 0) jav = jq
      if(iprn .lt. 0) return
      if((ksel.ne.1) .and. ((nedit.eq.1).or.(nedit.eq.2))) goto 70

c write out az, dip, step and se for the principal directions
      write(punt, '(9x, a)') 
     * '-az/dp--step---se =az/dp==step===se -az/dp--step---se' 
      aline = ' '
      write(aline(10:15), '(i3, 1h/ ,i2)') iaaz(ix),idip(ix)
c
      if (at(ix) .ne. blank) then
        call formit(at(ix), rat, fmit, 5, 1)
        write(aline(17:21), fmit) rat
        call formit(se(ix), rse, fmit, 4, 1)
        write(aline(23:26), fmit) rse
      endif
c
      write(aline(28:33), '(i3, 1h/ ,i2)') iaaz(iy),idip(iy)
c
      if (at(iy) .ne. blank) then
        call formit(at(iy), rat, fmit, 5, 1)
        write(aline(35:39), fmit) rat
        call formit(se(iy), rse, fmit, 4, 1)
        write(aline(41:44), fmit) rse
      endif
c
      write(aline(46:51), '(i3, 1h/ ,i2)') iaaz(iz),idip(iz)
c
      if (at(iz) .ne. blank) then
        call formit(at(iz), rat, fmit, 5, 1)
        write(aline(53:57), fmit) rat
        call formit(se(iz), rse, fmit, 4, 1)
        write(aline(59:62), fmit) rse
      endif
      write(punt, '(a)') aline

c write out comment records
      do 530 i = 1,lph
        if(keyph(i) .eq. -1) write(punt,520) phcard(i)(1:115)
  520   format(1x,a)
  530 continue

      if(iprn .ge. 0)
     * write(punt,65) ax2,ax1,axz,iqax,aze2,aze1
   65 format(/1x,'horizontal and vertical single variable standard',
     *   ' deviations (68% - one degree of freedom; max 99 km)'/,
     *  7x,6hseh = ,f6.2,13x,6hseh = ,f6.2,13x,6hsez = ,f6.2,
     * 13h   quality = ,a1/7x,6haz  = ,f6.0,13x,6haz  = ,f6.0/)
      write(punt,78) seorg,ni,dmax,seq
   78 format(' se of orig = ',f6.2,'; # of iterations = ',
     *       i3,'; dmax = ',f10.2,'; sequence number = ',a5)
      if(sminp .eq. blank) then
        write(punt, 975) evtype, evstat
  975   format(' event type = "', a1, '"',
     *           '; processing status = "', a1, '"', /,
     *           ' closest station did not use both p and s')
      else
        write(punt, 980) evtype, evstat, sminp
  980   format(' event type = "', a1, '"',
     *           '; processing status = "', a1, '"', /
     *    ' s minus p interval for closest station = ', f10.2)
      endif
c
c---- check for debug event
   70 continue
      if(nedit .ne. 0) then
        nopu = 1
        good = .true.
        call sort(pruse,keyi,ipwt)
        call sort(sruse,keyi,iswt)
        if(bigst .gt. semx) goto 107
        if(ni .gt. nimx) goto 107
        if(rms .gt. rmsmx) goto 107
        if(sruse(iswt) .gt. sresmx) goto 107
        if(pruse(ipwt) .gt. presmx) goto 107
        if(nwtout .gt. noutmx) goto 107
c----   event is good
        if(iabs(nedit) .eq. 1) then
          write(punt,74) iqax
   74     format('  quality based on standard errors is ',a1/)
          goto 73
        endif
        if(iabs(nedit) .eq. 3) goto 73
        noaxp = 1
        return
  107   good = .false.
        write(punt,76) ni,rms,pruse(ipwt),sruse(iswt),nwtout,bigst
   76   format(1h0,' d e b u g  e v e n t     ni   rms presmx sresmx',
     *  ' nwtout  bigst   ',/,
     *  24x,                              i5, f6.1,  f7.1,  f7.1,
     *  i7,  f7.1)
        if(nedit .lt. 0) nopu=0
        if(iabs(nedit) .eq. 3) goto 73
      endif
   73 continue
c
c---- check for preferred magnitude
      if((magke .ne. ' ') .and. 
     * (dnstrg(magke) .ne. 'x') .and.
     * (dnstrg(magke) .ne. 'f') .and. 
     * (dnstrg(magke) .ne. 'a')) then
        write(punt,77) odmag,magke
   77   format('  preferred magnitude on summary record will be: ',
     *  f5.2,1x,a1)
      endif
c
c---- set up final output line
      write(punt,75)
   75 format(/'    date    origin      lat      long    depth    mag',
     * ' no d1 gap d  rms    avwt   se')
      fline = ' '
      write(fline, 71) icent2, kkdate, krmo, khr, kmin, sec, 
     *  lat1, isnla, bat2, lon1, isnlo, bon2, krm1, z-test(8), krm2
      jnst = knst*10 + inst
   71 format(1x, i2, i6.6, a1, 2i2, f6.2, i3, a1, f5.2, i4, a1,
     *       f5.2, a1, f6.2, a1)
c---- add magnitude
      if (rmag .ne. blank) then
        call formit(mag, rmag, fmit, 5, 1)
        write(fline(49:53), fmit) rmag
      endif
c----- add up through rms
      write(fline(54:84), 82) nrwt, idmin, igap, kno, rms, avwt, yse
   82 format(2i3, i4, i2, f7.4, f6.2, f6.2)
      write(punt, '(a)') fline(1:84)
      write(punt, '(t22, f7.4, 2x, f8.4)') lat1+bat2/60.,lon1+bon2/60.
      fline = ' '
c
      write(punt, 821)
821   format('    seh  sez q sqd  adj in nr   avr  aar nm',
     * ' avxm mdxm sdxm nf avfm mdfm sdfm   vpvs')
      write(fline(1:29), 822) ax1, axz, nq, iqs, iqd, adj, jnst, nr
822   format( 1x, f6.1, f5.1, 3(1x,a1), f5.2, 2i3)
c----- add avr, aar, and nm
      call formit(avr, avrlm, fmit1, 5, 1)
      call formit(aar, aarlm, fmit2, 4, 1)
      fmit3 = fmit1(1:5)//',1x,'//fmit2(2:5)//',i3)'
      write(fline(31:44), fmit3) avrlm, aarlm, nm
c----- add average xmag
      if (avxm .ne. blank) then
        call formit(avxm, ravxm, fmit, 4, 1)
        write(fline(45:48), fmit) ravxm
      endif
c----- add median xmag
      if (xmmed .ne. blank) then
        call formit(xmmed, ravxm, fmit, 4, 1)
        write(fline(50:53), fmit) ravxm
      endif
c----- add two more
      write(fline(54:62), 83) sdxm,nf
   83 format(f5.1, i3)
c----- add average fmag
      if (avfm .ne. blank) then
        call formit(avfm, ravfm, fmit, 4, 1)
        write(fline(63:66), fmit)ravfm
      endif
c----- add median fmag
      if (fmmed .ne. blank) then
        call formit(fmmed, ravfm, fmit, 4, 1)
        write(fline(68:71), fmit)ravfm
      endif
c----- add rest
      write(fline(72:83), 84) sdfm, wslope
   84 format(f5.1, f7.3)
c
      write(punt, '(a)') fline(1:83)
c
c
c---- write out warning messages
      rewind(16)
      read(16, 97) erout
   96 read(16, 97, end=98) erout
        if(erout(1:3) .eq. 'end') goto 98
        write(punt, 97) erout
        write(logfil, 97) erout
   97   format(a)
      goto 96
   98 continue
c
c for now, remove short-circuit of detailed printout
c     if(nedit .eq. 0) goto 100
c     if( .not. good) goto 108
c     if(iprn .ge. 1) goto 108
c     return
c
  100 if(kp .eq. 1) goto 108
      if(iprn .le. 1) return
  108 isorp = 'fmp'
      if(iuses .eq. 1) isorp = 'fms'
      write(punt,110)
  110 format (/'                      -- travel times and delays --', /,
     *'  stn c pha remk p p-sec s-sec resid  std-er   dist  azm ain',
     *'    tc c vthk  ttob-ttcal-dlay-edly=resid rmk stn pha sources')
czzz
c***     1         2         3         4         5         6         7         8
c***5678901234567890123456789012345678901234567890123456789012345678901234567890
c stn c pha remk p p-sec s-sec resid  std-er   dist  azm ain    tc c vthk  ttob-
cbrse     1 ipu3 d199.25       -0.99 * 1.011  199.3  241 180  -.26 1 4.00 49.32
cbrse   s 1  s 2         49.25 -0.99 r 1.011             180  -.26 1 4.00 49.32
c spu     2 ip+9 -  3.22                      551.1  123  82  -.26 1 2.00 49.32
c spu   s 2  s 2         13.22                                -.26 1 2.00 49.32
c spu   smp                     1.55   1.222                              14.00
c
      if(nrp .eq. 0) return
      do 500 i=1,nrp
      aline = ' '
      k=i
      if(ksort .eq. 0) k = keyd(i)
      kk = ldx(k)
c
c set up component
c     if(dnstrg(krmp(k)(2:2)) .eq. 'p') then
c       comp = 'z'
c       comp = ' '
c     else
c       comp = krmp(k)(2:2)
c     endif
      comp = msta(k)(5:5)
c set up p-phase identifier
c     pha = 'p  '
      pha = '   '
      if(nlay(k) .ne. 0) then
        write(pha(3:3), '(i1)') nlay(k)
      endif
c
c      write(aline(1:24), 91) msta(k),  comp,   pha, krmp(k), msym(k),
c     1 p(k)
      write(aline(1:24), 91) msta(k)(1:4),  comp,   pha, krmp(k), 
     *  msym(k)
91    format(                  1x,a4, 1x,a1, 1x,a3,   1x,a4,   1x,a1)
c add p arrival time
      aline(20:24) = phcard(keyphi(k))(20:24)
c
      pres = x(4,k)
      apres = abs(pres)
      iprmk = kwr(k)
      if(iprun .ne. 1) then
        if(wt(k) .gt. 0.) then
c fix up p residuals for routine processing
          if(iprmk .eq. ' ') then
            if((apres.gt.0.6).and.(i.le.5)) iprmk = '*'
            if((apres.gt.0.9).and.(delta(k).lt.150.)) iprmk = '*'
            if((apres.gt.1.5).and.(delta(k).lt.350.)) iprmk = '*'
          endif
        endif
        if(apres .lt. 2.25) then
          l = (apres + 0.25)/0.5
          pres = l*0.5
        endif
      endif
      if(pres .gt. 999.99) pres = 999.99
      if(pres .lt. -99.99) pres = -99.99
c add p-residual
      if(wt(k) .eq. 0.0) then
        write(aline(31:44), '(f6.2, 1x, a1, a)') pres, iprmk, ' -----'
      else
        setmp = yse/sqrt(wt(k))
        if(setmp .gt. 99.99) setmp = 99.99
        write(aline(31:44), '(f6.2, 1x, a1, f6.2)') pres, iprmk, setmp
      endif
c
      write(aline(45:52), 93) delta(k), iqdo(k)
93    format(                     f7.1,      a1)
c
      call formf(az(k), iaz, 3, 0)
      write(aline(53:56), '(i4)') iaz
c
      call formf(ain(k), iain, 4, 0)
      write(aline(57:60), '(i4)') iain
c add time correction
      if (dt(k) .ne. 0.) then
        dtk = dt(k)
        call formit(dtk, dtk, fmit, 5, 1)
        write(aline(62:66), fmit) dtk
      endif
c add model
      write(aline(67:68), '(i2)') model(k)
c leave one blank
c add variable layer thickness
      if (thks(k) .ne. blank) then
        call formit(thks(k), rthks, fmit, 4, 1)
        write(aline(70:73), fmit) rthks
      endif
c add p-travel time observed
      tpk = tp(k) - org
      if(tpk .lt. -3500.) tpk = tpk + 3600.
      write(aline(74:85), 94) tpk, t(k)
94    format(2f6.2, i2)
c leave one blank
c add p-delay
      kji=kdx(k)
      if (dly(kno, kji) .ne. 0.) then
        call formit(dly(kno,kji), dlyk, fmit, 4, 1)
        write(aline(87:90), fmit) dlyk
      endif
c leave one blank
c add elevation delay
      if (elvdly(k) .ne. 0.) then
        call formit(elvdly(k), eldly, fmit, 4, 1)
        write(aline(92:95), fmit) eldly
      endif
c repeate p-residual and add remark
      write(aline(96:104), '(f6.2, 1x, a2)') pres, krm(k)
c leave one blank
c repeate station name
      aline(106:109) = msta(k)(1:4)
c leave one blank
c repeate phase type
      aline(111:113) = aline(9:11)
c leave two blanks
c give sources and number of hops from phase record
      aline(116:121) = phcard(keyphi(k))(105:110)
c
c aline is now set up correctly for a normal p phase
      if(ldx(k) .eq. 0) then
        if(ksmp(k) .eq. 1) then
c no s data - write p record and go on to next phase
          write(punt, '(a)') aline(1:121)
          goto 500
        else
c s minus p data -- remove residual and std-er
c set up bline as a revised p record and write out
98764     bline = aline
          bline(25:45) = ' '
c compute the p-residual
          pres = tp(k) - t(k) - org - dly(kno, kji) - elvdly(k)
          if(pres .gt. 999.99) pres = 999.99
          if(pres .lt. -99.99) pres = -99.99
          write(bline(96:101), '(f6.2)') pres
c write out the p line for an s-p
          write(punt, '(a)') bline(1:121)
c
c next set up bline as an s record
          bline(9:9) = 's'
          write(bline(13:16), '(a4)') krms(k)
c remove p seconds
          bline(20:24) = ' '
c add s seconds
          bline(26:30) = phcard(keyphi(k))(32:36)
c remove dist, azm, and ain
          bline(47:60) = ' '
c add observed and computed travel times
          tsk = ts(k) - org
          if(tsk .lt. -3500.) tsk = tsk + 3600.
          write(bline(74:86), '(2f6.2)') tsk, vpvsm(model(k))*t(k)
c add s-delay
          if (sdly(kno, kji) .ne. 0.) then
            call formit(sdly(kno,kji), sdlyk, fmit, 4, 1)
            write(bline(87:90), fmit) sdlyk
          else
            bline(87:90) = ' '
          endif
c add elevation delay
          if (elvdly(k) .ne. 0.) then
            call formit(vpvsm(model(k))*elvdly(k), eldly, fmit, 4, 1)
            write(bline(92:95), fmit) eldly
          else
            bline(92:95) = '    '
          endif
c compute the s-residual
98765     sres = ts(k) - org - vpvsm(model(k))*(t(k) + elvdly(k))
     *    - sdly(kno, kji)
          if(sres .gt. 999.99) sres = 999.99
          if(sres .lt. -99.99) sres = -99.99
          write(bline(96:101), '(f6.2)') sres
          bline(111:111) = 's'
          write(punt, '(a)') bline(1:113)
c
c finally set up aline as the s minus p record
          aline(9:30) = 'smp'
          aline(45:73) = ' '
c add observed and computed s-p
          csmp = (vpvsm(model(k))-1.)*(t(k) + elvdly(k))
     *    - dly(kno, kji) + sdly(kno, kji)
          write(aline(74:85), '(2f6.2)') ts(k)-tp(k), csmp
          aline(86:104) = ' '
c repeate phase type
          aline(111:113) = aline(9:11)
          write(punt, '(a)') aline(1:113)
        endif
      else
c
c this is the case of a normal p and s
c first write p record
          write(punt, '(a)') aline(1:121)
c
c next convert aline into an s record
        aline(9:9) = 's'
        aline(13:16) = krms(k)
c remove corrected first motion and p-res
        aline(18:24) = ' '
c add s seconds
        aline(25:30) = phcard(keyphi(k))(32:36)
        sres = x(4,kk)
        krm5 = kwr(kk)
        if(iprun .ne. 1) then
c fix up s residuals for routine processing
          asres = abs(sres)
          if(wt(kk) .gt. 0.0) then
            if(krm5 .eq. ' ') then
              if((asres.gt.0.9).and.(i.le.5)) krm5 = '*'
              if((asres.gt.1.5).and.(delta(k).lt.150.)) krm5 = '*'
              if((asres.gt.2.0).and.(delta(k).lt.350.)) krm5 = '*'
            endif
          endif
          if(asres .lt. 2.25) then
            l = (asres + 0.25)/0.5
            sres = l*0.5
          endif
        endif
        if(sres .gt. 999.99) sres = 999.99
        if(sres .lt. -99.99) sres = -99.99
        if(wt(kk) .eq. 0.0) then
          write(aline(31:44), '(f6.2, 1x, a1, a)') sres, krm5, ' -----'
        else
          setmp = yse/sqrt(wt(kk))
          if(setmp .gt. 99.99) setmp = 99.99
          write(aline(31:44), '(f6.2, 1x, a1, f6.2)') sres, krm5, setmp
        endif
c remove dist and azm
        aline(47:56) = ' '
        call formf(ain(kk), iain, 4, 0)
        write(aline(57:60), '(i4)') iain
        write(aline(67:68), '(i2)') model(kk)
c add observed and computed travel times
        tsk = ts(k) - org
        if(tsk .lt. -3500.) tsk = tsk + 3600.
        write(aline(74:86), '(2f6.2)') tsk, t(kk)
c add s-delay
        if (sdly(kno, kji) .ne. 0.) then
          call formit(sdly(kno,kji), sdlyk, fmit, 4, 1)
          write(aline(87:90), fmit) sdlyk
        else
          aline(87:90) = ' '
        endif
c add elevation delay
        if (elvdly(kk) .ne. 0.) then
          call formit(elvdly(kk), eldly, fmit, 4, 1)
          write(aline(92:95), fmit) eldly
        else
          aline(92:95) = '    '
        endif
c repeate s-residual and leave remark
        write(aline(96:101), '(f6.2)') sres
c repeate phase type
        aline(111:113) = aline(9:11)
        write(punt, '(a)') aline(1:113)
      endif
500   continue
c
c loop through again for magnitude data
c
      write(punt, 550)
550   format(/,
     * '                                     -- magnitude data --',/,
     * '  stn c source sys    c10   amx gr ink     amf    per ',
     * '  unit/mm  gnd mot u xmgc xmag  fmp fmag')
      do 900 i = 1, nrp
        aline = ' '
        k=i
        if(ksort .eq. 0) k = keyd(i)
        kji=kdx(k)
        if (amx(k) .ne. 0.) then
          write(aline(16:17), '(i2)') kluse(k)
          aline(28:31) = phcard(keyphi(k))(44:47)
c add corrected amplitude
          write(aline(39:47), '(f9.0)') ampuse(k)
        endif
        if (cluse(k) .gt. 0.) then
          write(aline(18:25), '(f8.2)') cluse(k)
	endif
	if ((sysmag(k) .ne. blank) .and. (gndmot(k) .ne. blank)) then
          write(aline(55:74), '(1pe10.4, 1x, 1pe9.3)') 
     *    sysmag(k), gndmot(k)
        endif
c a1vco gain range state 0, 1, or 2
        aline(34:34) = phcard(keyphi(k))(62:62)
c semens playback state 0 or 1
        aline(37:37) = phcard(keyphi(k))(61:61)
c add xmag
        if (xmag(k) .ne. blank) then
          write(aline(75:80), '(f5.2, 1x)') xmgc(kji)
          call formit(xmag(k), rxmag, fmit, 4, 1)
          write(aline(81:84), fmit) rxmag
        endif
c flag deviant xmags
        if(xmag(k) .ne. blank) then
	  if(xmwt(kji) .eq. 0) then
c flag excluded xmags
c 	    aline(65:65) = 'e'
	    aline(85:85) = 'e'
	  else if(abs(xmag(k) - avxm) .ge. 0.5) then
c flag deviant xmags 
c           aline(65:65) = '*'
            aline(85:85) = '*'
	  endif
        endif
c add f-p
        rfmp = fmp(k)
        if(iuses .eq. 1) rfmp = fms(k)
        if (rfmp .ne. blank) then
          call riorbk(rfmp, ifmp, fmit, 4, 0)
c         write(aline(66:69), fmit) ifmp
          write(aline(86:89), fmit) ifmp
        endif
c leave 2 blanks
c add fmag
        rfmag = fmag(k)
        if (fmag(k) .ne. blank) then
          call formit(fmag(k), rfmag, fmit, 4, 1)
c         write(aline(71:74), fmit) rfmag
          write(aline(91:94), fmit) rfmag
        endif
c
        if(dnstrg(kodsym(k)) .eq. 's') then
c fmp too short with respect to s-p, so fmag not computed
          krm4 = kodsym(k)
        else
          krm4 = ' '
          if(fmag(k) .ne. blank) then
            if(fmwt(kji) .eq. 0) then
c flag excluded fmags
              krm4 = 'e'
            else if(abs(fmag(k) - avfm) .ge. 0.5) then
c flag deviant fmags with krm4
              krm4 = '*'
            endif
          endif
        endif
c       aline(75:75) = krm4
        aline(95:95) = krm4
	if(aline(1:33) .ne. ' ')
     *    write(aline(49:53), '(f5.2)') peruse(k)
        if((aline(1:33) .ne. ' ') .or. 
     *    (aline(38:133) .ne. ' ')) then
c set up component
c         if(dnstrg(krmp(k)(2:2)) .eq. 'p') then
c           comp = 'z'
c           comp = ' '
c         else
c           comp = krmp(k)(2:2)
c         endif
	  comp = msta(k)(5:5)
          aline(2:5) = msta(k)(1:4)
          aline(7:7) = comp
c add amplitude source
          aline(11:11) = phcard(keyphi(k))(108:108)
c         write(punt, '(a)') aline(1:75)
          write(punt, '(a)') aline(1:95)
        endif
900   continue
      return
      end
c end output
