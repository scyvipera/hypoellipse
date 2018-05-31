c mising.for    []
      subroutine mising
c check to see if data from stations that were not used in
c------- this solution would significantly improve hypocenter
      include 'params.inc' 
      parameter (ndly = 11)
      real lat,lon,latep,lonep,mag
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /dhip/ inpt,isa,ilis,inmain,injump
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /amo/ tempg(npa), ntgap
      common /dmost/ ipun,ivlr,blank
      common /iclmpq/ lat(nsn),lon(nsn)
      common /ilmpu/ ns
      common /ilpu/ sw(nsn),ndate(nsn),nhr(nsn),mno(nsn),ielv(nsn)
      common /ilpx/ calr(5, nsn),fmgc(nsn),xmgc(nsn)
      common /ilt/ vthk(2,nsn),ipdly(nsn),mod(nsn),ipthk(nsn)
      common /ilv/ c(nsn),e(nsn)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /int/ thk(lmax+1),lbeg(mmax),lend(mmax),vi(lmax),vsq(lmax),
     *  vsqd(lmmax,lmax),f(lmax,lmmax),kl,ivway,sthk(mmax),sthk1(mmax),
     *  sdly(ndly,nsn)
      common /iox/ prr(nsn),iuses
      integer fmwt, xmwt
      common /ioxl/ fmwt(nsn), xmwt(nsn)
      logical fsteq
      character*1 revp, exdly
      common /ip/ latr,lonr,tpdly(2,nsn),infil,iofil,indexs,
     *  iscod(nsn),ipcod(nsn), fsteq, exdly(4, nsn), revp(6, nsn)
      common /ip1/ rsew(4)
      common /omnfh/ dmin,dmin3,sminp
      common /pfnoqv/ kdate,khrmn,khr
      common /pm/ jdx(nsn)
      common /pox/ fmp(npa),prx(npa),icent1,icent2
      common /qmost1/ lonep,ni,latep
      common /reloc/ irelo, nreloc
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      common /xfmno/ mag
      ihd=0
      nj = ntgap + 1
      tempg(nj)=tempg(1)+360.
      if (mag .eq. blank) then
        tdel=100.
      else
        tdel = 10.**(1.48 + mag/2.)
      endif
      do 130 i=1,ns
    5 if (ndate(i) .eq. 99999998) goto 130
      if (jdx(i) .eq. 1) goto 130
      call delaz(latep,lonep,deli,deldeg,azi,lat(i),lon(i))
      if (deli .gt. tdel) goto 130
      if (azi .le. tempg(1)) azi=azi+360.
      do 10 j=2,nj
      if (azi .lt. tempg(j)) goto 20
   10 continue
      j=nj
   20 exgap=tempg(j)-tempg(j-1)
      rdgap=tempg(j)-azi
      tgap=azi-tempg(j-1)
      if (tgap .lt. rdgap) rdgap=tgap
      if (deli.gt.dmin3 .and. rdgap.lt.30.) goto 130
      if (azi .ge. 360.) azi=azi-360.
      if (ihd .eq. 1) goto 22
      write(punt,21)
   21 format(/10x,45hmissing station  delta   azim  ex-gap  rd-gap)
      ihd=1
c     make sure station record has not expired.
   22 if (ndate(i) .gt. icent2*1000000+kdate) goto 100
      if ((ndate(i) .eq. icent2*1000000+kdate) .and.
     *  (nhr(i) .ge. khrmn/100)) goto 100
      call update(indexs, nsta, ielv,
     *         lat, lon, c, e, sw,
     *         klas, calr, xmgc, fmwt, xmwt, fmgc, ipcod, iscod, 
     *         tpdly, revp, ndate, nhr, ns,
     *         exdly, icent2, kdate, khrmn,
     *         infil, iofil)
      goto 5
  100 write(punt,125) nsta(i),deli,azi,exgap,rdgap
  125 format(21x,a5,2f7.1,2f8.1)
  130 continue
      return
      end
c end mising
