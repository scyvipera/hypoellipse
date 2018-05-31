c boxau.for    []
      subroutine boxau
c compute rms at auxiliary points and estimate quality
      include 'params.inc' 
      parameter (ndly = 11)
      real latep,lonep,latsv,lonsv
      save latsv, lonsv, rmssv, zsv
      save dez, t6, drsv, kdrsv, diag, kdiag, sum, aalz, aala
      save aalo, idirec, idrc
      character*1 iqa, ins, iew
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /bz/ na
      common /bqz/ avrps,avuse
      common /dmost/ ipun,ivlr,blank
      character*1 iclass
      common /dbio/ iclass(0:4)
      common /gmost/ az(npa)
      logical freor
      common /hopq/ savla,savlo,savez,savor,freor
      common /imost/ test(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /obcfn/ ain(npa)
      character*1 kwr, iqdo
      common /pbnoq/ kwr(npa),iqdo(npa)
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /phoqn/ inst,knst
      common /qmost/ wt(npa),z
      common /qmost1/ lonep,ni,latep
      common /qgnotx/ delta(npa)
      common /qgo/ org
      common /rob/ v(4,4),noaxp
      common /rbno/ idip(3),iaaz(3),a(3,3)
      common /rfgnoq/ se(4)
      common /tmost/ x(4,npa)
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      common /zqr/ fno
      dimension drsv(14),kdrsv(14),diag(7),kdiag(7)
      dimension sum(5)
      dimension aalz(14),aala(14),aalo(14)
      dimension vec(3)
      character*2 idirec(7), idrc(7)
      data idirec/' n','se','sw','nw','ne',' z',' e'/
      data aala/1.732,1.,1.,1.,1.,0.,0.,0.,0.,-1.,-1.,-1.,-1.,-1.732/,
     *     aalo/0.,1.,-1.,1.,-1.,0.,1.732,-1.732,0.,1.,-1.,1.,-1.,0./,
     *     aalz/0.,-1.,-1.,1.,1.,-1.732,0.,0.,1.732,-1.,-1.,1.,1.,0./
      kgoon = 0
c
c write heading
   17 if( (iprn .gt. 2)  .or. (noaxp .eq. 1) ) write(punt,18)
   18 format(/50h       lat       lon         z     avrps        no ,
     *  7x,3hrms,25x,4hdrms/)
c
c save current values
      rmssv = rms
      latsv = latep
      lonsv = lonep
      zsv = z
      orgsv = org
      instsv = inst
c set inst = -9 so quakes will compute fixed location rms but will
c not call regres.
      inst = -9
      freor = .true.
      t6 = abs(test(6))/1.732
      dez = t6
      do 70 na = 1, 14
        call back(t6*aala(na),t6*aalo(na),savla,savlo,latsv,lonsv)
        savez = zsv +aalz(na)*dez
c       if(z .lt. 0.) z = 0.0
        if(savez .lt. 0.) savez = 0.0
        call quakes
c output rms error af auxiliary points
        rmsx = rms
        drms = rmsx - rmssv
        if(iprn .eq. 4) then
c calcute predicted rms at aux points
          tot = 0.0
          vec(1) = aalo(na)/1.732
          vec(2) = aala(na)/1.732
          vec(3) = aalz(na)/1.732
          do 48 i = 1,3
            do 46 j = 1,3
              tot = tot + vec(i)*vec(j)*v(i,j)
   46       continue
   48     continue
          pdrms = sqrt(rmssv**2 + tot*test(6)*test(6)/fno)
          drms = rmsx - pdrms
        endif
        call unfold2(savla,savlo,la,ins,ala,lo,iew,alo)
        drsv(na) = drms
        if( (iprn .ge. 2) .and. (noaxp .ne. 1) ) then
          goto (1,2,3,4,5,6,7,8,9,10,11,4,13,1), na
    2     write(punt,801) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  801     format(4f10.2,i10,f10.2,10x,f5.2)
          goto  50
    3     write(punt,802) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  802     format(4f10.2,i10,f10.2,24x,1h-,8x,f5.2/96x,1h.)
          goto  50
    1     write(punt,803) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  803     format(/4f10.2,i10,f10.2,23x,f5.2/)
          goto  50
    4     write(punt,804) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  804     format(4f10.2,i10,f10.2,13x,f5.2,19x,1h.)
          goto  50
    5     write(punt,805) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  805     format(4f10.2,i10,f10.2,12x,'|',23x,f5.2)
          goto  50
    6     write(punt,806) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  806     format(4f10.2,i10,f10.2,20x,f5.2,10x,'|')
          goto  50
    7     write(punt,807) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  807     format(4f10.2,i10,f10.2,3x,f5.2/)
          write(punt,815) rmssv
  815     format(50x,f10.2,12x,'|',10x,5h 0.00,10x,'|'/95x,'|')
          goto  50
    8     write(punt,808) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  808     format(4f10.2,i10,f10.2,43x,f5.2)
          goto  50
    9     write(punt,809) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  809     format(4f10.2,i10,f10.2,26x,f5.2)
          goto 50
   10     write(punt,810) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  810     format(4f10.2,i10,f10.2,10x,f5.2,23x,'|')
          goto 50
   11     write(punt,811) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  811     format(4f10.2,i10,f10.2,13x,1h.,10x,1h-,8x,f5.2/
     *    74x,1h.,21x,1h.)
          goto 50
   13     write(punt,813) ala,alo,z-test(8),avrps,nrwt,rmsx,drms
  813     format(4f10.2,i10,f10.2,27x,1h-,8x,f5.2)
        endif
   50   continue
   70 continue
c ouality output
      i = 0
      ii = 15
      do  80 iii = 1,7
      i = i + 1
      ii = ii - 1
   80 diag(iii) = (drsv(i) + drsv(ii))/2.0
      call sort(diag, kdiag, 7)
      call sort(drsv, kdrsv, 14)
      do  90 i = 1,7
        j = kdiag(i)
        idrc(i) = idirec(j)
   90 continue
      if((noaxp .eq. 0) .and. (iprn .ge. 0)) write(punt,100) idrc, diag
  100 format(/25x,18hquality evaluation//,
     * 34h diagonals in order of strength   ,7(4x,a2)/,
     *  19h ave. of end points,15x,7f6.2//)
      sum(1) = 0.0
      do 110 i=1,14
c do not use upper point in average if limited by surface
        if((z.lt.abs(test(6))*1.732).and.(kdrsv(i).eq. 6)) goto 110
        sum(1) = sum(1) + drsv(i)
  110 continue
      avdrms = sum(1)/14.
      if(z .lt. abs(test(6))*1.732) avdrms = sum(1)/13.
      drmin = drsv(1)
      icmin = kdrsv(1)
      if((z .ge. abs(test(6))*1.732).or.(kdrsv(1) .ne. 6)) goto 115
      drmin = drsv(2)
      icmin = kdrsv(2)
  115 jab = 4
      if((nrwt.ge. 4.).and.(rmssv.le. 0.4).and.(avdrms.ge. 0.5)) jab = 3
      if((nrwt.ge.5.).and.(rmssv.le. 0.4).and.(drmin  .ge. 0.15)) jab=2
      if((nrwt.ge.6.).and.(rmssv.le. 0.2).and.(drmin  .ge. 0.30)) jab=1
      iqa = iclass(jab)
      if((noaxp.eq.0) .and. (iprn .ge. 0))
     * write(punt,120)nrwt,rmssv,drmin,avdrms,iqa
  120 format(10x,50h    number       rms  min drms  ave drms   quality/
     * 10x,i10,3f10.2,9x,a1/)
c
c restore variable values
      latep = latsv
      lonep = lonsv
      z = zsv
      org = orgsv
      inst = instsv
      if (drmin .gt. -.03 .or. test(6) .gt. 0.) then
c restore all values, such as azimuth, angle of incidence, and
c standard error to those of final solution for npunch.
130     iprnsv = iprn
        iprn = -2
        inst = 9
        freor = .false.
        savla = latsv
        savlo = lonsv
        savez = zsv
        savor = orgsv
        call quakes
c       call output(0)
c       call output(1)
        iprn = iprnsv
        inst = instsv
        return
      else
        kgoon = kgoon + 1
c the first time an event reaches here, kgoon will equal 1.
c the 2nd time kgoon will equal 2, so the event will not be rurun again.
        if((inst .ne. 9) .and. (inst .ne. 8)) return
        if(kgoon .ge. 2) then
c do the same as above, starting with statement 130
c         goto 130
          iprnsv = iprn
          iprn = -2
          inst = 9
          freor = .false.
          savla = latsv
          savlo = lonsv
          savez = zsv
          savor = orgsv
          call quakes
c         call output(0)
c         call output(1)
          iprn = iprnsv
          inst = instsv
          return
        endif
        call back(t6*aala(icmin),t6*aalo(icmin),savla,savlo,latsv,lonsv)
        savez = zsv + aalz(icmin)*dez
        if(z .lt. 0.0) z = 0.0
        write(punt,1000)
 1000   format(///' *** run again starting at best nearby point ***',/)
        call quakes
        goto 17
      endif
      end
c end boxau
