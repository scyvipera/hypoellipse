c weight.for    []
      subroutine weight
      include 'params.inc' 
      parameter (ndly = 11)
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /anox/ keyd(npa)
      common /bqz/ avrps,avuse
      common /dhip/ inpt,isa,ilis,inmain,injump
      common /dmost/ ipun,ivlr,blank
      common /gmost/ az(npa)
      common /imost/ test(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /ihfgpq/ itest(100)
      common /iiq/ wf(51)
      common /pgqv/ w(npa)
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /phoqn/ inst,knst
      character*1 kwr, iqdo
      common /pbnoq/ kwr(npa),iqdo(npa)
      common /pnoqtx/ ldx(npa)
      common /qmost/ wt(npa),z
      common /qmost1/ lonep,ni,latep
      common /qgnotx/ delta(npa)
      common /qo/ dmax,ndec,adjsq,iph
      common /qw/ azmv(5), iuse(npa), duse(npa), ndwt, iboxc
      logical ddone, adone, tdone, bdone, jdone
      common /qw1/ ddone, adone, tdone, bdone, jdone
      common /rq/ xmean(4),avwt,aveaj
      common /tmost/ x(4,npa)
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      data pi/3.1415927/
c----
c weight arrival times 5 different ways:
c------- nrwt is the total number of readings with weight not zero
c------- fno is the sum of p,s and s-p weights.
c------- nsmp is the total number of s-p data with weight not zero
c------- onf is the sum of p and s weights
c----
c distance weighting
      ndout = 0
      if((ni .ge. itest(10)) .and. (.not. ddone)) then
c compute distance weights this iteration
        if((iprn .ge. 1) .and. (ilis .gt. 0)) write(punt, 610)
610     format(' apply distance weighting')
        ddone = .true.
        dmax = test(12)
        if(itest(46) .ne. 0) then
c set xfar to 10 km beyond the distance of the test(46)th station
          n10 = iabs(itest(46) )
          if(ndwt .lt. n10) n10 = ndwt
          if(duse(n10) .gt. dmax) dmax = duse(n10) + 10.
        endif
620     xfn = dmax - test(11)
        do 631 i = 1,nr
          if(wt(i) .gt. 0.0) then
c since station has weight, check distance weight
            if(delta(i) .le. test(11)) then
c full weight
            else if(delta(i) .lt. dmax) then
c partial weight
c             wt(i) = wt(i)*((dmax - delta(i))/xfn)**2
              thet = pi*(delta(i) - test(11))/xfn
              distwt = .5*(cos(thet) + 1.0)
              wt(i) = wt(i)*distwt
            else
c no weight
              ndout = ndout + 1
              wt(i) = 0.0
              kwr(i) = 'd'
            endif
          endif
631     continue
c
c reweight stations that are distant but reduce a gap .gt. 100
c       by 50 degrees or more.
        if((ndout .gt. 0) .and. (itest(46) .lt. 0))
     *  call redgap(iuse, w, wt, kwr, az, keyd, ldx, nrp)
cd      print *, 'iuse w     wt    kwr  az    keyd i'
cd      print '(i5, f6.2, a1, i5, f6.2, 3i5)',
cd   *  (iuse(i), w(i), wt(i), kwr(i), az(i), keyd(i), ldx(i),
cd   *  i, i = 1, nrp)
        call zstats
      endif
c
c azimuthal weighting
      iazch = 0
      if((ni .ge. itest(13)) .and. (.not. adone)) then
        if((iprn .ge. 1) .and. (ilis .gt. 0)) write(punt, 634)
634     format(' apply azimuthal weighting')
        adone = .true.
        call azwtos
        call zstats
      endif
c
c truncation weighting
      if((ni .ge. itest(14)) .and. (.not. tdone)) then
        if((iprn .ge. 1) .and. (ilis .gt. 0)) write(punt, 635)
635     format(' apply truncation weighting')
        tdone = .true.
636     continue
        if(nrwt .ge. 6) then
          resmx = 0.0
          do 640 i = 1,nr
            if(wt(i) .eq. 0.0) goto 640
            dif = abs(x(4,i)-ksmp(i)*avuse)
            if(dif .le. resmx) goto 640
            resmx = dif
            iresx = i
640       continue
          if(resmx .le. test(15)) goto 650
          wt(iresx) = 0.0
          kwr(iresx) = 'm'
          call zstats
          goto 636
        endif
      endif
c
c boxcar weighting
650   nobox=0
      if((ni .eq. itest(16)) .and. (.not. bdone)) then
        if((iprn .ge. 1) .and. (ilis .gt. 0)) write(punt, 652)
652     format(' apply boxcar weighting')
        bdone = .true.
        if (nrwt .ge. 6) then
          t34 = rms
          if(rms .lt. abs(test(29))) t34 = abs(test(29))
          do 660 i = 1,nr
            if(wt(i) .eq. 0.0) goto 660
            dif = abs(x(4,i)-ksmp(i)*avuse)
            if(dif .gt. test(17)*t34) then
              kwr(i) = 'b'
              wt(i) = 0.0
              nobox=nobox+1
            endif
660       continue
          if(nobox .gt. 0) then
            call zstats
            goto 650
          endif
        endif
      endif
c
c jeffreys weighting
674   if( (ni .eq. itest(18)) .and. (rms .gt. test(19))
     * .and. (.not. jdone) ) then
        if((iprn .ge. 1) .and. (ilis .gt. 0)) write(punt, 675)
675     format(' apply jeffreys weighting')
        jdone = .true.
        t34 = rms
        if(t34 .lt. abs(test(29))) t34 = abs(test(29))
        do 690 i=1,nr
          if(wt(i) .eq. 0.0) goto 690
          k = 10.*abs((x(4,i)-ksmp(i)*avuse)/t34) + 1.5
          if(k .gt. 51) k = 51
          wt(i) = wt(i)*wf(k)
          if(k .gt. 30) kwr(i) = 'j'
690     continue
        call zstats
      endif
      call zstats
      return
      end
c end weight
