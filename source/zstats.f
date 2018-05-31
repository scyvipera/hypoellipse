c zstats.for    []
      subroutine zstats
c calculate the statistics of the residuals.
      include 'params.inc' 
      parameter (ndly = 11)
      integer punt
      common /punt/ punt
      common /bz/ na
      common /bqz/ avrps,avuse
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /orz/ onf
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /phoqn/ inst,knst
      common /zqr/ fno
      common /qmost/ wt(npa),z
      common /tmost/ x(4,npa)
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      double precision sum(5),xwt
c nrwt is the total number of readings with weight .gt. zero.
c fno is the sum of p, s and s-p weights.
c onf is the sum of p and s weights.
      do 50 i = 1,5
        sum(i) = 0.0d0
50    continue
      nrwt = 0
      nswt = 0
      fno = 0.0
      onf = 0.0
      avr = 0.0
      aar = 0.0
      avrps = 0.0
      rms = 0.0
      if((iprn .ge. 5) .and. (na .eq. 0)) then
        write(punt, 60)
   60   format(//'          i    weight      ksmp             residual',
     *   '  zstats formats 80 and 1000')
      endif
      do 100 i = 1,nr
        if (wt(i) .eq. 0.0) goto 80
        nrwt = nrwt + 1
        fno = fno + wt(i)
        if (i .gt. nrp) nswt = nswt + 1
        onf = onf + wt(i)*ksmp(i)
        xwt = x(4,i)*wt(i)
        sum(1) = sum(1) + xwt
        sum(2) = sum(2) + abs(xwt)
        sum(5) = sum(5) + xwt*ksmp(i)
   80   if((iprn .lt. 5) .or. (na .ne. 0)) goto 100
        write(punt,1000) i,wt(i),ksmp(i),x(4,i)
 1000   format(i10,f12.4,i10,f20.2)
  100 continue
      if(nrwt .eq. 0) goto 500
      avr = sum(1)/fno
      aar = sum(2)/fno
      if(onf .eq. 0.0) goto 125
      avrps = sum(5)/onf
      fixor = 1.0
      if(inst .eq. 8) fixor = 0.0
  125 do 150 i = 1,nr
  150 sum(3) = sum(3) + wt(i)*((x(4,i)-avrps*fixor*ksmp(i))**2)
      rms = sqrt(sum(3)/fno)
      if((iprn .lt. 4) .or. (na .ne. 0)) goto 500
      write(punt,1010) avr,aar,avrps,rms,nrwt,fno,onf
 1010 format(/, '       avr       aar     avrps         rms      nrwt',
     * '       fno        onf   zstats format 1010', /,
     * 1x, 3f10.3, e15.8, i10, 2f10.2)
  500 avuse = avrps
      if(inst .eq. 8) avuse = 0.0
      return
      end
c end zstats
