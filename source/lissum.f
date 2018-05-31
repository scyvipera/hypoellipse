c lissum.for    []
      subroutine lissum(lisco)
c output summary of time and magnitude residuals
      save
      include 'params.inc' 
      parameter (ndly = 11)
      real lat,lon
      character fixsw*1, fixp*1, fixs*1, jfmt*4, line*80
      character*1 ins, iew
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /dhil/ iq,ilat,kms,icat
      common /dmost/ ipun,ivlr,blank
      common /hl/ nwad, tslope, tsqsl
      character*1 iqcls
      common /il1/ iqcls
      common /ilpu/ sw(nsn),ndate(nsn),nhr(nsn),mno(nsn),ielv(nsn)
      common /ilmpu/ ns
      common /iclmpq/ lat(nsn),lon(nsn)
      common /ilotv/ elvdly(npa)
      common /ilpx/ calr(5, nsn),fmgc(nsn),xmgc(nsn)
      common /ilv/ c(nsn),e(nsn)
      common /ilt/ vthk(2,nsn),ipdly(nsn),mod(nsn),ipthk(nsn)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /int/ thk(lmax+1),lbeg(mmax),lend(mmax),vi(lmax),vsq(lmax),
     *  vsqd(lmmax,lmax),f(lmax,lmmax),kl,ivway,sthk(mmax),sthk1(mmax),
     *  sdly(ndly,nsn)
      logical fsteq
      character*1 revp, exdly
      common /ip/ latr,lonr,tpdly(2,nsn),infil,iofil,indexs,
     *  iscod(nsn),ipcod(nsn), fsteq, exdly(4, nsn), revp(6, nsn)
      common /lc/ nres(nsn)
      common /ohbl/ jav
      common /olnx/ xmag(npa),fmag(npa),avxm,avfm,xmmed,fmmed
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /pnl/ ww(npa)
      common /pnoqtx/ ldx(npa)
      common /qmost/ wt(npa),z
      common /reloc/ irelo, nreloc
      common /rq/ xmean(4),avwt,aveaj
      common /tmost/ x(4,npa)
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      dimension nxm(nsn), nfm(nsn), sr(nsn), srsq(nsn),
     * srwt(nsn),sxm(nsn),sxmsq(nsn),sfm(nsn),sfmsq(nsn),qno(4)
      dimension nresp(nsn), srp(nsn), srpsq(nsn), srpwt(nsn)
      dimension nsresp(nsn), ssrp(nsn), ssrpsq(nsn), ssrpwt(nsn)
      dimension nsres(nsn),ssr(nsn),ssrsq(nsn),ssrwt(nsn),nsmpr(nsn),
     * ssmpr(nsn),ssmpq(nsn),ssmpw(nsn),qnoper(4)
      dimension avres(7),sdres(7)
      data avres/7*0./
      if (iprn .ge. 5) write(punt, '(a, i5)') ' begin lissum ', lisco
      logu = 6
c initialize arrays
      if(lisco .eq. 1) then
        nwad = 0
        tslope = 0.
        tsqsl = 0.
        sumrms = 0.0
        do 48 l=1,ns
          nres(l)=0
          nresp(l)=0
          nres(l)=0
          nxm(l)=0
          nfm(l)=0
          sr(l)=0.
          srp(l)=0.
          srsq(l)=0.
          srpsq(l)=0.
          srwt(l)=0.
          srpwt(l)=0.
          nsres(l) = 0
          nsresp(l) = 0
          ssr(l) = 0.0
          ssrp(l) = 0.0
          ssrsq(l) = 0.0
          ssrpsq(l) = 0.0
          ssrwt(l) = 0.0
          ssrpwt(l) = 0.0
          nsmpr(l) = 0
          ssmpr(l) = 0.0
          ssmpq(l) = 0.0
          ssmpw(l) = 0.0
          sxm(l)=0.
          sxmsq(l)=0.
          sfm(l)=0.
          sfmsq(l)=0.
   48   continue
        do 49 i=1,4
          qno(i)=0.
   49   continue
        return
      endif
c
    4 if(lisco .eq. 0) then
        call lisinc(qno, sumrms, rms, jav, iq, nrwt, avwt, nr,
     *  kdx, wt, nrp, xmag, fmag, blank, avxm, avfm, nxm, nfm, sxm,
     *  sxmsq, sfm, sfmsq, sw, ipcod, ww, ksmp, nsmpr, ssmpr, x,
     *  ssmpq, ssmpw, nres, sr, srsq, srwt, nresp, srp, srpsq, srpwt,
     *  iscod, nsres, ssr, ssrsq, ssrwt, nsresp, ssrp, ssrpsq, ssrpwt)
        return
      endif
c
c write summary of time and mag residuals (statements 0 to 70)
      rewind 2
      qsum=qno(1)+qno(2)+qno(3)+qno(4)
      if(qsum .eq. 0.) then
        write(logu, 3)
        if(punt .ne. logu) write(punt, 3)
    3   format(' there were no events located in this run.')
        return
      endif
      averms = 0.0
      if(qsum .ne. 0.0) averms = sumrms/qsum
      avpvs = 0.
      if(nwad .gt. 0) avpvs = tslope/nwad
      sdpvs = 0.
      if(nwad .gt. 1) then
        arg = tsqsl/(nwad-1) - tslope*tslope/(nwad*(nwad-1))
        if(arg .lt. 0.0) arg = 0.0
        sdpvs = sqrt(arg)
      endif
      write(logu,5) averms, avpvs, nwad, sdpvs, (qno(i),i=1,4), qsum
      if(punt .ne. logu)
     *write(punt,5) averms, avpvs, nwad, sdpvs, (qno(i),i=1,4), qsum
    5 format(' average rms of all events = ', f10.5, /,
     *       ' average vp/vs ratio = ', f10.2, ' for ', i4, ' events.',
     *       '  standard deviation of ratio = ', f10.2,
     * //, ' ***** class/     a     b     c     d total *****',
     * //, '      number/', 5f6.1)
      do 10 i=1,4
   10 qnoper(i)=100.*qno(i)/qsum
      write(logu,15)(qnoper(i),i=1,4)
      if(punt .ne. logu) write(punt,15)(qnoper(i),i=1,4)
   15 format(/2x,'percentage/',4f6.1)
      if(iqcls .eq. '0') return
      write(logu,20) iqcls
      if(punt .ne. logu) write(punt,20) iqcls
   20 format(/,'   include only class ',a1,' and better in the',
     *  ' following statistics.' /)
   71 write(logu,1020)
      if(punt .ne. logu) write(punt,1020)
 1020 format(
     *'          ----------- p residuals --------------',
     *'    ----------- s residuals -------------', /,

     *'           no event wting        event wting',
     *'         no event wting        event wting', /,

     *' station  n  wt  ave   sd     n   wt  ave   sd',
     *'      n  wt   ave  sd     n   wt   ave  sd    station')

      do 70 i=1,ns

        do 30 j=1,7
          avres(j)=0.
          sdres(j)=0.
   30   continue

        avwt1 = 0.
        avwt6 = 0.
        if(nres(i) .gt. 0) then
c compute average p residual
          avres(1)=sr(i)/srwt(i)
c compute average weight
          avwt1 = srwt(i)/nres(i)
          if(nres(i) .gt. 1) then
	    temp = srsq(i)/srwt(i) - avres(1)**2
	    if(temp .gt. 0.0) then
              sdres(1)=sqrt(temp)
	    else
	      sdres(1)=0.0
	    endif
          endif
          if(nresp(i) .gt. 0) then
            if(srpwt(i) .gt. 0.0) then
c compute average residual, weight, and std. deviation a la pavlis
c see pavlis and hokanson (1985) - jgr, v 90, p 12777-12789
              avres(6)=srp(i)/srpwt(i)
              avwt6 = srpwt(i)/nresp(i)
	      temp = srpsq(i)/srpwt(i) - avres(6)**2
	      if(temp .gt. 0.0) then
                sdres(6)=sqrt(temp)
	      else
	        sdres(6)=0.0
	      endif
            endif
          endif
        endif

        avwt2 = 0.
        avwt7 = 0.
        if(nsres(i) .gt. 0) then
          avwt2 = ssrwt(i) / nsres(i)
          avres(2) = ssr(i)/ssrwt(i)

          if(nsres(i) .gt. 1) then
	    temp = ssrsq(i)/ssrwt(i) - avres(2)**2
	    if(temp .gt. 0.0) then
              sdres(2) = sqrt(temp)
	    else
	      sdres(2) = 0.0
	    endif
          endif

          if(nsresp(i) .gt. 0) then
            if(ssrpwt(i) .gt. 0.0) then
c compute average residual, weight, and std. deviation a la pavlis
              avres(7) = ssrp(i)/ssrpwt(i)
              avwt7 = ssrpwt(i) / nsresp(i)
	      temp = ssrpsq(i)/ssrpwt(i) - avres(7)**2
	      if(temp .gt. 0.0) then
                sdres(7) = sqrt(temp)
	      else
	        sdres(7) = 0.0
	      endif
            endif
          endif
        endif

        ipt = nres(i) + nsres(i)
        if(ipt .eq. 0) goto 70

        fixsw = ' '
        if (sw(i) .eq. 0.) fixsw = 'w'
        fixp = ' '
        if ( (ipcod(i) .gt. 3) .and.
     *  (ipcod(i) .lt. 9) ) fixp = 'p'
        fixs = ' '
        if ( (iscod(i) .gt. 3) .and.
     *  (iscod(i) .lt. 9) ) fixs = 's'

        write(logu, 65) fixsw, nsta(i),
     *  nres(i), avwt1, avres(1), sdres(1),
     *  nresp(i), avwt6, avres(6), sdres(6), fixp,
     *  nsres(i), avwt2, avres(2), sdres(2),
     *  nsresp(i), avwt7, avres(7), sdres(7), fixs, nsta(i)

        if(punt .ne. logu) write(punt, 65) fixsw, nsta(i),
     *  nres(i), avwt1, avres(1), sdres(1),
     *  nresp(i), avwt6, avres(6), sdres(6), fixp,
     *  nsres(i), avwt2, avres(2), sdres(2),
     *  nsresp(i), avwt7, avres(7), sdres(7), fixs, nsta(i)

   65   format(1x, a1, 1x, a5,
     *  2(i4, f4.1, 2f6.3, i4, f5.1, 2f6.3, a1), 2x, a4)

      if(irelo .gt. 0) then
        if(nreloc .eq. irelo) then
c write out revised primary station record
          call unfold2(lat(i),lon(i),la,ins,ala,lo,iew,alo)
          write(line, 67) nsta(i), la, ins,  ala, lo, iew,  alo,
     *    ielv(i), mod(i), ipthk(i)
67        format(              a5, i2,  a1, f5.2, i4,  a1, f5.2,
     *         i5,     i2,       i1)
c is a5 correct?  Shouldn't it be a4????????????

          call riorbk(vthk(1, i), ivt, jfmt, 4, 2)
          write(line(31:34), jfmt) ivt
          call riorbk(vthk(2, i), ivt, jfmt, 4, 2)
          write(line(35:38), jfmt) ivt
          write(line(39:39), '(i1)') ipdly(i)
          call riorbk(dly(1, i), idly, jfmt, 4, 2)
          write(line(40:43), jfmt) idly
          call riorbk(sdly(1, i), idly, jfmt, 4, 2)
          write(line(44:47), jfmt) idly
          write(13, '(a)') line
        else
          dly(1, i) = dly(1, i) + avres(6)
          sdly(1, i) = sdly(1, i) + avres(7)
        endif
      endif
   70 continue

      write(logu,1021)
      if(punt .ne. logu) write(punt,1021)
 1021 format(/,
     *'           s-p residuals        x-mag res       f-mag res', /
     *' station  n  wt  ave   sd     n  ave   sd     n  ave   sd')

      do 75 i=1,ns

        do 73 j=1,7
          avres(j)=0.
          sdres(j)=0.
   73   continue

        if(nxm(i) .gt. 0) then
          avres(3)=sxm(i)/nxm(i)
          if(nxm(i) .gt. 1) then
	    temp = sxmsq(i)/nxm(i) - avres(3)**2
	    if(temp .gt. 0.0) then
              sdres(3)=sqrt(temp)
	    else
	      sdres(3) = 0.0
 	    endif
          endif
        endif

        if(nfm(i) .gt. 0) then
          avres(4)=sfm(i)/nfm(i)
          if(nfm(i) .gt. 1) then
	    temp = sfmsq(i)/nfm(i) - avres(4)**2
	    if(temp .gt. 0.0) then
              sdres(4)=sqrt(temp)
	    else
	      sdres(4) = 0.0
	    endif
          endif
        endif

        asmpw = 0.
        if(nsmpr(i) .gt. 0) then
          asmpw = ssmpw(i) / nsmpr(i)
          avres(5) = ssmpr(i)/ssmpw(i)
          if(nsmpr(i) .gt. 1) then
	    temp = ssmpq(i)/ssmpw(i) - avres(5)**2
	    if(temp .gt. 0.0) then
              sdres(5) = sqrt(temp)
	    else
	      sdres(5) = 0.0
	    endif
          endif
        endif

        ipt = nsmpr(i) + nxm(i) + nfm(i)
        if(ipt .eq. 0) goto 75

        fixsw = ' '
        if (sw(i) .eq. 0.) fixsw = 'w'

        write(logu, 74) fixsw, nsta(i),
     *  nsmpr(i), asmpw, avres(5), sdres(5),
     *  nxm(i), avres(3), sdres(3), nfm(i), avres(4), sdres(4)

        if(punt .ne. logu) write(punt, 74) fixsw, nsta(i),
     *  nsmpr(i), asmpw, avres(5), sdres(5),
     *  nxm(i), avres(3), sdres(3), nfm(i), avres(4), sdres(4)

   74   format(1x, a1, 1x, a5, 
     *  (i4, f4.1, 2f6.3), 2(i4, 2f6.3), 4x, a4)

   75 continue
      return
      end
c end lissum
