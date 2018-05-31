c fmplot.for    []
      subroutine fmplot
c plot first-motion directions on the lower focal hemisphere
c------- in equal area projection
      include 'params.inc' 
      logical repeat
      real bat2,bon2,mag
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /dmost/ ipun,ivlr,blank
      common /gmost/ az(npa)
      common /hf/ cp(72),sp(72)
      common /imost/ test(100)
      common /obcfn/ ain(npa)
      common /ocfn/ adj,seh,lat1,bat2,lon1,bon2,igap
      character*1 isnla, isnlo, krm1, krm2
      common /ocfn1/ isnla, isnlo, krm1, krm2
      common /ofln/ sec
      common /ofnv/ kmin
      common /omnfh/ dmin,dmin3,sminp
      character msym*1
      common /pfo/ msym(npa)
      common /pfnoqv/ kdate,khrmn,khr
      common /pno/ lph,keyph(npa),nsum
      character phcard*117, krm*2, seq*5
      common /pno1/ phcard(npa), krm(npa), seq
      common /qmost/ wt(npa),z
      common /xfmno/ mag
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
c
c     for multics printer use xscale = 0.101064
c     for slac printer use xscale = 0.1379
      xscale = test(45)
      repeat = .false.
      if (test(7) .lt. 0.) repeat = .true.
      write(punt,2)
    2 format(52h1  date    origin    lat n    long w    depth    mag,
     * 17h no gap dmin  rms)
      rmag = mag
      if (mag .eq. blank) rmag = 0.0
      write(punt,5) kdate,khr,kmin,sec,lat1,bat2,lon1,bon2,krm1,
     * z-test(8),krm2,rmag,nrwt,igap,dmin,rms
    5 format(2x,i6.6,1x,2i2,f6.2,i3,1h-,f5.2,i4,1h-,f5.2,a1,f6.2,a1
     *,f6.2,i3,i4,f5.1,f5.2)
      call fmplt(msta, az, ain, msym, lph, xscale, repeat, punt,
     *                 cp, sp)
      return
      end
c end fmplot
