c npunch.for    [unix]
      subroutine npunch(style)
c----
c print summary record with regress information
c write summary record with regress information
c write summary record without regress information
c write station data
      include 'params.inc' 
      parameter (ndly = 11)
      integer sumout, arcout
      character*5 style, upstrg*1
c style can be 'final' or 'temp '.  if 'temp ', then write output
c on 4 to temporaty file 14 and output on 11 to temporary file 15
      logical sumpr
      real bat2,bon2,mag
      character*1 iqpr, iwrk, fmit*6, dnstrg
      character aline*133, pline*117
      character*4 iahead*60, msta*5, nsta*5, icard*110
      common /an/ n14, n15
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /dbiln/ ioldq
      common /dhin/ iglob, zup, zdn
      logical medmag
      common /dinx/ imag,imagsv,medmag
      common /dmost/ ipun,ivlr,blank
      common /gmost/ az(npa)
      common /omnfh/ dmin,dmin3,sminp
      common /obcfn/ ain(npa)
      common /ocfn/ adj,seh,lat1,bat2,lon1,bon2,igap
      character*1 isnla, isnlo, krm1, krm2
      common /ocfn1/ isnla, isnlo, krm1, krm2
      common /pot/ ts(npa),ihr,model(npa), keyphi(npa)
      common /pox/ fmp(npa),prx(npa),icent1,icent2
      integer punt
      common /punt/ punt
      common /ghnq/ iexit
      common /hpn/ wslope
      common /idno/ ksel,ksort
      common /ilotv/ elvdly(npa)
      common /imost/ test(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /in/ irmo,iryr,odmag
      character*1  magke
      common /in1/ magke
      common /int/ thk(lmax+1),lbeg(mmax),lend(mmax),vi(lmax),vsq(lmax),
     *  vsqd(lmmax,lmax),f(lmax,lmmax),kl,ivway,sthk(mmax),sthk1(mmax),
     *  sdly(ndly,nsn)
      logical fsteq
      character*1 revp, exdly
      common /ip/ latr,lonr,tpdly(2,nsn),infil,iofil,indexs,
     *  iscod(nsn),ipcod(nsn), fsteq, exdly(4, nsn), revp(6, nsn)
c if instset .ne. ' ', then put this new value of inst on summary record
      character*1 instset
      common /hnu/ instset
      common /ofnv/ kmin
      character*1 nq
      common /ofnv1/ nq
      common /ofln/ sec
      common /olnx/ xmag(npa),fmag(npa),avxm,avfm,xmmed,fmmed
      common /on/ ix,iy,iz,ax1,axz
      character iqax*1
      common /on1/ iqax
      common /onx/ kluse(npa), cluse(npa), peruse(npa), ampuse(npa)
      common /pfnoqv/ kdate,khrmn,khr
      common /pgnoqv/ p(npa),jmin(npa)
      common /pgnov/ dt(npa)
      common /pgoq/ s(npa)
      character*4 ipro, ichec, evtype*1, evstat*1, root*256
      common /dbhip/ ipro, ichec, evtype, evstat, root
      common /phoqn/ inst,knst
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /pn/ itpused(npa)
      common /pnl/ ww(npa)
      common /pno/ lph,keyph(npa),nsum
      character phcard*117, krm*2, seq*5
      common /pno1/ phcard(npa), krm(npa), seq
      character*1 kwr, iqdo
      common /pbnoq/ kwr(npa),iqdo(npa)
      common /pnoqtx/ ldx(npa)
      common /qmost/ wt(npa),z
      common /qgnotx/ delta(npa)
      common /qgo/ org
      common /rbno/ idip(3),iaaz(3),a(3,3)
      common /rfgnoq/ se(4)
      common /ro/ yse,seorg,phi
      common /tmost/ x(4,npa)
      common /tonxb/ t(npa),fms(npa)
      common /xfmno/ mag
      character*1 mgndx
      common /xfmno1/ mgndx
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      character*(4) tmpsum_type, tmparc_type, fname*256
      character acent2*2, fooline*133
      integer tmpsum_unit, tmparc_unit
      parameter (tmpsum_type  = '.3sc')
      parameter (tmparc_type  = '.4sc')
      parameter (tmpsum_unit  = 14)
      parameter (tmparc_unit  = 15)

      write(acent2, '(i2.2)') icent2
c* (unix
      close(tmpsum_unit)
      length_root = lentru(root)
      fname = root(1:length_root)//tmpsum_type
      call openfl(tmpsum_unit, fname, 'scratch', 'null', 'none',
     *      'noprint', 300)

      close(tmparc_unit)
      fname = root(1:length_root)//tmparc_type
      call openfl(tmparc_unit, fname, 'scratch', 'null', 'none',
     *      'noprint', 0)
c* unix)
c* (pc
c      rewind tmpsum_unit 
c      rewind tmparc_unit 
c* pc)
c* (vax
c      rewind tmpsum_unit 
c      rewind tmparc_unit 
c* vax)

      sumpr = .false.
      rmag = mag
c n14 and n15 count the number of records saved temporarily, until
c geterr has been run to compute zup and zdn
      n14 = 0
      n15 = 0
c set up unit numbers for output
      if(style .eq. 'final') then
        sumout = 4
        arcout = 11
      else
        sumout = tmpsum_unit
        arcout = tmparc_unit
      endif
c	write(punt, '(a, 2i5)') 'sumout, arcout = ', sumout, arcout
      if((magke .eq. ' ') .or. (magke .eq. 'x') .or.
     *   (magke .eq. 'f') .or. (magke .eq. 'a') .or.
     *   (magke .eq. 'k')) goto 3
c     in this case, save old magnitude value
      rmag = odmag
      mgndx = magke
    3 nfile = 4
      if(ipun .lt. 0 .or. ipun .gt. 4) return
c fix up the date and time 
      khr = ihr
      kmin = lbastm
      sec = org
      kkdate = kdate
      call tshift(kkdate,khr,kmin,sec,dum)

c take care of the fake solution cases
      if(iexit .eq. 1) then
c       don't write out fake arrival data if there wasn't a solution
        if (ipun .eq. 4) return
	if ((inst .eq. 9) .and. (keyph(1) .eq. -2)) then
c         for inst=9, use original summary record if there was one
          pline = acent2//phcard(1)(1:115)
	  goto 137
        else if((dnstrg(evtype) .eq. 't') .or.
     *    (dnstrg(evtype) .eq. 'n') .or.
     *    (dnstrg(evtype) .eq. 'r')) then
c         for tele, nuclear, or regional, use old summary record
          pline = acent2//phcard(1)(1:115)
	  goto 137
	else
          goto 225
	endif
      endif
c
c key to keyph values
c      -1  comment record
c      -2  summary record
c      -3  instruction record
c      -4  deleted station record
c     -11  decode error
c      .ge. 1, index number of phase used in solution
c
c print and write summary information with regress information
   10 call formf(sec,iisec,4,2)
      call formf(bat2,iilat,4,2)
      call formf(bon2,iilon,4,2)
      call formf(z-test(8),iiz,5,2)
      call formf(dmin,iidmin,3,0)
      call formf(rms,iirms,4,2)
      iqpr = iqax
      if(ioldq .eq. 1) iqpr = nq
      if(iprn .lt. 3) goto 130
      write(punt, '(3a)')
     *  '     date   origin      lat      long    depth    mag no',
     *  ' gap  d3   rms az/dp    se az/dp    se az/dp    se  iqpr',
     *  ' isetno nswt  seq  s-p'
c
c*********************create summary record for print file**************
      aline = ' '

c     write(punt, '(a,2i10, f10.2, 2i10 )') 
c    *  'khr, kmin, sec, kdate, kkdate',
c    *  khr, kmin, sec, kdate, kkdate
      write(aline, 126)  kkdate, khr, kmin, sec, lat1, isnla, bat2,
     * lon1, isnlo, bon2, z-test(8)
  126 format(1x, i6.6, 1x ,i2, i2, f6.2, i3, a1, f5.2, i4, a1,
     * f5.2, 1x, f6.2)
c
      if (rmag .ne. blank) then
        call formit(rmag, frmag, fmit, 3, 1)
        write(aline(49:51), fmit) frmag
      endif
c
      write(aline(52:74), 127) nrwt, igap, dmin, rms, iaaz(ix), idip(ix)
  127 format(i3, i4, f5.0, f5.2, i3, '/' ,i2)
c
      if(se(ix) .ne. blank) then
        call formit(se(ix), frsei, fmit, 5, 1)
        write(aline(76:80), fmit) frsei
      endif
c
      write(aline(81:86), 128) iaaz(iy), idip(iy)
  128 format(i3, '/' ,i2)
c
      if (se(iy) .ne. blank) then
        call formit(se(iy), frsej, fmit, 5, 1)
        write(aline(88:92), fmit) frsej
      endif
c
      write(aline(93:98), 128) iaaz(iz), idip(iz)
c
      if (se(iz) .ne. blank) then
        call formit(se(iz), frsek, fmit, 5, 1)
        write(aline(100:104), fmit) frsek
      endif
c
c     write(aline(105:132), 129) iqpr, mgndx, nswt, seq, sminp
c 129 format(5x ,a1, 6x ,a1, 3x ,i2, a5, f5.2)
      write(aline(105:127), 129) iqpr, mgndx, nswt, seq
  129 format(5x ,a1, 6x ,a1, 3x ,i2, a5)
      if(sminp .ne. blank) then
        write(aline(128:132), '(f5.2)') sminp
      endif
c
c     add century
      fooline = aline(2:132)
      aline = acent2//fooline
      write(punt, '(1x, a)') aline
c********************finished with printed summary record****************
c
  130 continue
c*********************create summary record******************************
      pline = ' '
      write(pline, 131) kkdate, khr, kmin, iisec, lat1, isnla, iilat,
     * lon1, isnlo, iilon
  131 format(            i6.6,  i2,   i2,    i4,   i2,    a1,    i4,
     *   i3,    a1,    i4)
      write(pline(111:115), '(i5)') iiz
      if(test(9) .eq. 0.0) then
        write(pline(30:34), '(i5)') iiz
      else
        if(iiz .lt. 0) then
          write(pline(30:34), '(a)') '  -00'
        else
          write(pline(30:34), '(i5)') iiz
        endif
      endif
c
      if (rmag .ne. blank) then
        call riorbk(rmag, jmag, fmit, 2, 1)
        write(pline(35:36), fmit) jmag
      endif
c
      write(pline(37:54), 133) nrwt, igap, iidmin, iirms, iaaz(ix),
     * idip(ix)
  133 format(                    i3,   i3,     i3,    i4,       i3,
     *       i2)
c
      if (se(ix) .ne. blank) then
        call riorbk(se(ix), iisei, fmit, 4, 2)
        write(pline(55:58), fmit) iisei
      endif
c
      write(pline(59:63), '(i3, i2)') iaaz(iy), idip(iy)
c
      if (se(iy) .ne. blank) then
        call riorbk(se(iy), iisej, fmit, 4, 2)
        write(pline(64:67), fmit) iisej
      endif
c
      if(medmag) then
c use median xmag
        if (xmmed .ne. blank) then
          call riorbk(xmmed, iavxm, fmit, 2, 1)
          write(pline(68:69), fmit) iavxm
        endif
      else
c use average xmag
        if (avxm .ne. blank) then
          call riorbk(avxm, iavxm, fmit, 2, 1)
          write(pline(68:69), fmit) iavxm
        endif
      endif
c
      if(medmag) then
c use median fmag
        if (fmmed .ne. blank) then
          call riorbk(fmmed, iavfm, fmit, 2, 1)
          write(pline(70:71), fmit) iavfm
        endif
      else
c use average fmag
        if (avfm .ne. blank) then
          call riorbk(avfm, iavfm, fmit, 2, 1)
          write(pline(70:71), fmit) iavfm
        endif
      endif
c
      write(pline(72:72), '(a1)') evstat
c
      if (se(iz) .ne. blank) then
        call riorbk(se(iz), iisek, fmit, 4, 2)
        write(pline(73:76), fmit) iisek
      endif
c
      write(pline(77:96), 134) iqpr, mgndx, nswt,    ipro,
     * irmo, iryr, evtype, phcard(lph)(19:19), seq
  134 format(                    a1,    a1,   i2, '/', a4,
     *   i2,   i2,     a1,                 a1,  a5)
      if(instset .ne. ' ') pline(91:91) = instset
c
      if(sminp .ne. blank) then
        call riorbk(sminp, isminp, fmit, 4, 2)
        write(pline(97:100), fmit) isminp
      endif
c
c     if(iglob .eq. 0) then
      if((iglob .eq. 0) .and. ((inst .eq. 0) .or. 
     *  (inst .eq. 8))) then
        call formal(zup, izup, 2, 0, fmit, azup)
        if (fmit .eq. ' ') then
          write(pline(101:102), '(i2)') izup
        else
          write(pline(101:102), fmit) azup
        endif
        call formal(zdn, izdn, 2, 0, fmit, azdn)
        if (fmit .eq. ' ') then
          write(pline(103:104), '(i2)') izdn
        else
          write(pline(103:104), fmit) azdn
        endif
      endif
c
      if (wslope .ne. 0.) then
        call riorbk(wslope, iiwslp, fmit, 4, 2)
        write(pline(105:108), fmit) iiwslp
      endif
c
c compute the number of phases weighted out due to large residuals
      nwtout = 0
      do 136 i = 1, lph
        if(keyph(i) .gt. 0) then
          k = keyph(i)
c	  print *, 'nwtout, k, kwr(k) ', nwtout, k, kwr(k)
c p phases
          if((kwr(k) .eq. 'b') .or. (kwr(k) .eq. 'm') .or.
     *      (kwr(k) .eq. 'j')) nwtout = nwtout + 1
          if(ldx(k) .ne. 0) then
c s phases
            kk = ldx(k)
            if((kwr(kk) .eq. 'b') .or. (kwr(kk) .eq. 'm') .or.
     *        (kwr(kk) .eq. 'j')) nwtout = nwtout + 1
          endif
        endif
136   continue
      if(nwtout .gt. 99) nwtout = 99
      write(pline(109:110), '(i2)') nwtout
c
c     for now (12/21/91) keep upper case characters
      pline(17:17) = upstrg(pline(17:17))
      pline(25:25) = upstrg(pline(25:25))
      pline(77:77) = upstrg(pline(77:77))
      pline(78:78) = upstrg(pline(78:78))
c
c     add century
      fooline = pline(1:115)
      pline = acent2//fooline
c
c*************write out current summary record parameters********
c	write(punt, '(a, i5)') 'ipun = ', ipun
137   if (ipun .lt. 3) then	
        write(sumout, '(a)') pline
c keep the summary file up to date
c* (unix
	if(sumout .eq. 4) call flush(sumout)
c* unix)
c* (pc 
c* pc) 
c* (vax 
c* vax) 
c	write(punt, '(a, i5)') 'just flushed sumout unit = ', sumout
        if(style .ne. 'final') n14 = n14 + 1
      endif
      if (ipun .eq. 1) return
      goto 260
c
c****************come here if no solution was found**************
  225 continue
c before writing out a fake summary record, check if the current
c summary record (if there is one) is also a fake, based on having
c a blank rms field (cols 46:49)
      if(((phcard(1)(46:49) .ne. '    ') .and.
     *   (keyph(1) .eq. -2)) .or.
     *   (keyph(1) .ne. -2)) then
c there is a valid summary record from a previous run to preserve, or
c there is no summary record from previous run, so
c generate a fake summary record
        ibegin = 1
        pline = ' '
	if(keyph(1) .eq. -2) then
	  pline(1:10) = phcard(1)(1:10)
	else
          write(pline, '(i6.6, i4)') kkdate, khrmn
	endif
c
        pline(17:17) = 'n'
        pline(25:25) = 'w'
        if(rmag .ne. blank) then
          call riorbk(rmag, jmag, fmit, 2, 1)
          write(pline(35:36), fmit) jmag
        endif
c
        if(avxm .ne. blank) then
          call riorbk(avxm, iavxm, fmit, 2, 1)
          write(pline(68:69), fmit) iavxm
        endif
c
        if (avfm .ne. blank) then
          call riorbk(avfm, iavfm, fmit, 2, 1)
          write(pline(70:71), fmit) iavfm
        endif
c
        write(pline(72:72), '(a1)') evstat
c
        pline(78:81) = 'k  /'
c
        write(pline(82:96), 227) ipro, irmo, iryr, evtype,
     *  phcard(lph)(19:19), seq
  227   format(a4, 2i2, 2a1, a5)
        if(instset .ne. ' ') pline(91:91) = instset

c       for now (12/21/91) keep upper case characters
        pline(17:17) = upstrg(pline(17:17))
        pline(25:25) = upstrg(pline(25:25))
        pline(77:77) = upstrg(pline(77:77))
        pline(78:78) = upstrg(pline(78:78))
	fooline = acent2//pline(1:115)
	pline = fooline
      else
c there was already a fake summary record, so 
c do not generate another fake summary record
        ibegin = 2
        if(phcard(1)(81:81) .eq. '/') then
	  pline = acent2//phcard(1)
	else
	  pline = phcard(1)
	endif
      endif
c
      if(ipun .lt. 3) then
        write(sumout, '(a)') pline
c keep the summary file up to date
c* (unix
	if(sumout .eq. 4) call flush(sumout)
c* unix)
c* (pc 
c* pc) 
c* (vax 
c* vax) 
        if(style .ne. 'final') n14 = n14 + 1
      endif
c
      if(ipun .eq. 1) return
c for archive option, write out first old summary record if it was
c real, followed by all privious summary and phase records
      write(arcout, '(a)') pline(1:lentru(pline))
      if(style .ne. 'final') n15 = n15 + 1
      do 228 i = ibegin, lph
	if(keyph(i) .eq. -2) then
c* (unix
          phcard(i)(81:81) = '\\'
c* unix)
c* (vax
c* (pc
c         phcard(i)(81:81) = '\'
c* pc)
c* vax)
          write(arcout, 144) acent2//phcard(i)(1:lentru(phcard(i)))
	else
          if(upstrg(phcard(i)(1:2)) .ne. 'C*')
     *      phcard(i)(65:65) = upstrg(phcard(i)(65:65))
          write(arcout, 144) phcard(i)(1:lentru(phcard(i)))
          if(style .ne. 'final') n15 = n15 + 1
	endif
  228 continue
      return
c
c **************output archive-phase data for each station*************
  260 iqpr = iqax
      if(ioldq .eq. 1) iqpr = nq
c archive current summary record parameters
      write(arcout, '(a)') pline(1:lentru(pline))
      if(style .ne. 'final') n15 = n15 + 1
      nott = 0
c
c********************** loop through stations
      do 310 i = 1, lph
      pline = ' '
c skip on down if ipun = 4 and generate perfect data
      if(ipun .eq. 4) then
        if(keyph(i) .lt. 1) goto 310
        goto 302
      endif
c old summary records, skip the first one, be sure col 81 has \
c     for the secondary summary records
      if(keyph(i) .eq. -2) then
        nott = nott + 1
        if(nott .eq. 1) goto 310
c* (unix
        phcard(i)(81:81) = '\\'
c* unix)
c* (vax
c* (pc
c        phcard(i)(81:81) = '\'
c* pc)
c* vax)
        pline = acent2//phcard(i)(1:115)  
        write(arcout, 144) pline(1:lentru(pline))
  144   format(a)
        if(style .ne. 'final') n15 = n15 + 1
        goto 310
      endif
      if(keyph(i) .le. 0) then
c   write out comment record, deleted station record or instruction record
        write(arcout, 144) phcard(i)(1:lentru(phcard(i)))
        if(style .ne. 'final') n15 = n15 + 1
        goto 310
      endif
      k = keyph(i)
      kji = kdx(k)
      call formf(x(4,k),ipr,5,2)
      iain = ain(k)  + 0.5001
      iwrk = '    '
      isr = 0
      isse = 0
      if(ldx(k) .ne. 0) then
c s data
        kk = ldx(k)
        iwrk = kwr(kk)
        call formf(x(4,kk),isr,5,2)
        if(wt(kk) .ne. 0.0) then
          setmp = yse/sqrt(wt(kk))
          call formal(setmp, isse, 3, 2, fmit, xsse)
          if(fmit .eq. '      ') then
            write(pline(81:83), '(i3)') isse
          else
            write(pline(81:83), fmit) xsse
          endif
        else
          write(pline(81:83), '(a)') '99.'
        endif
        goto 300
      endif
      if(ksmp(k) .ne. 1) then
c s-p data
        call formf(x(4,k),isr,5,2)
      endif
  300 continue
      call formf(dly(kno,kji),ipdly,3,1)
      call formf(sdly(kno,kji),isdly,3,1)
      call formf(elvdly(k),ieldly,3,1)
      call formf(delta(k),idelt,4,1)
      call formf(az(k),iaz,3,0)
c
      write(pline(1:50), 301) phcard(i)(1:24), idelt, iaz,
     * phcard(i)(32:40), iain, phcard(i)(44:50)
  301 format(a24, i4, i3, a9, i3, a7)
c
      call formal(t(k), iptt, 4, 2, fmit, xiptt)
      if(fmit .eq. '      ') then
        write(pline(51:54), '(i4)') iptt
      else
        write(pline(51:54), fmit) xiptt
      endif
      if(wt(k) .ne. 0.0) then
        setmp = yse/sqrt(wt(k))
        call formal(setmp, ise, 3, 2, fmit, xse)
        if(fmit .eq. '      ') then
          write(pline(55:57), '(i3)') ise
        else
          write(pline(55:57), fmit) xse
        endif
      else
        write(pline(55:57), '(a)') '99.'
      endif
c
      write(pline(58:58), '(a1)') kwr(k)
c save the instrument period and gain characters
      write(pline(59:60), '(a)') phcard(i)(59:60)
c
c replace calr with siemens gain state and a1vco gain range state
      write(pline(61:62), '(a)') phcard(i)(61:62)
c
      write(pline(63:80), 303) phcard(i)(63:75), ipr
  303 format(a13, i5)
c
      write(pline(84:98), 3031) iwrk, isr, ipdly,
     * isdly, ieldly
3031  format(a1, i5, 3i3)
c
      if (xmag(k) .ne. blank) then
        write(pline(99:100), '(i2)') kluse(k)
        call riorbk(xmag(k), ixmag, fmit, 2, 1)
        write(pline(101:102), fmit) ixmag
      endif
c
      if (fmag(k) .ne. blank) then
        call riorbk(fmag(k), ifmag, fmit, 2, 1)
        write(pline(103:104), fmit) ifmag
      endif
c
c satellite hops
c value of nhop is no longer preserved, because the station history can
c accomodate two different telemetry delays
c      if(phcard(i)(110:110) .ne. ' ') then
c preserve any non blank value
c        pline(105:110) = phcard(i)(105:110)
c      else
c note that itpused is based soley on the p-phase
      if (itpused(k) .ne. 0) then
        nhop = -tpdly(itpused(k), kji)/.27 + .1
      else
        nhop = 0
      endif
      write(pline(105:110), '(a, i1)') phcard(i)(105:109), nhop

c     for now (12/21/91) keep upper case 
c     if(pline(1:2) .ne. 'C*') then
      if(upstrg(pline(1:2)) .ne. 'C*') then
        pline(58:58) = upstrg(pline(58:58))
        pline(65:65) = upstrg(pline(65:65))
        pline(84:84) = upstrg(pline(84:84))
      endif
c
      write(arcout, '(a)') pline(1:lentru(pline))
      if(style .ne. 'final') n15 = n15 + 1
      goto 310
c
c***********************genterate perfect data*********************
  302 k = keyph(i)
      read(phcard(i), 3021) ldate, lhr, lmin
3021  format(9x, i6.6, 2i2)
      psec = p(k) - x(4,k)
      if(ldx(k) .ne. 0) ssec = s(k) - x(4,ldx(k))
      call tshift(ldate,lhr,lmin,psec,ssec)
      call formf(psec, ipsec, 5, 2)
      if(ldx(k) .ne. 0) then
c       take care of s phase
        call formf(ssec, issec, 5, 2)
        write(arcout, 304) phcard(i)(1:9), ldate, lhr, lmin, ipsec,
     *  issec, phcard(i)(37:40)
        if(style .ne. 'final') n15 = n15 + 1
  304   format(a9, i6.6, 2i2, i5, 7x, i5, a4)
      else
c       only have p to write out
        write(arcout, 305) phcard(i)(1:9), ldate, lhr, lmin, ipsec
  305   format(a9, i6.6, 2i2, i5)
        if(style .ne. 'final') n15 = n15 + 1
      endif
  310 continue
      if(ipun .eq. 4) then
        write(arcout,320)
  320   format('                      ')
        if(style .ne. 'final') n15 = n15 + 1
      endif
      return
      end
c end npunch
