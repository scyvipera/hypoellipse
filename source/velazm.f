c velazm.for    []
      subroutine velazm
c find azimuth, apparent velocity, and residuals for plane wave
c    traveling across network form a distant event.
      include 'params.inc' 
      parameter (ndly = 11)
      double precision pp,q,r,s,t,u,v,y
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /gmost/ az(npa)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /ilotv/ elvdly(npa)
      common /ilv/ c(nsn),e(nsn)
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /pfnoqv/ kdate,khrmn,khr
      common /pgnoqv/ p(npa),jmin(npa)
      common /pgnov/ dt(npa)
      common /pgqv/ w(npa)
      character*4 krmp
      common /pnoqv/ krmp(npa)
      common /povx/ amx(npa)
      common /qmost/ wt(npa),z
      common /tmost/ x(4,npa)
      dimension tim(npa),tx(npa)
      nrpp=0
      do 8 i=1,nrp
        if (w(i).ne.0.0) nrpp=nrpp+1
    8 continue
      if (nrpp.gt.3) goto 10
      write(punt,9) nrpp
    9 format(/, ' insufficient data to calculate azimuth and apparent',
     * ' velocity.', /,
     * ' only ', i3, ' p-wave readings with weights not equal to zero.')
      return
   10 kno=1
      pp=0.0d00
      q=0.0d00
      r=0.0d00
      s=0.0d00
      t=0.0d00
      d=0.0
      u=0.0d00
      v=0.0d00
      y=0.0d00
c normalize reading weights.  use no other weights.
      sumw=0.0
      do 15 i=1,nrp
        wt(i)=w(i)
        sumw=sumw+w(i)
   15 continue
      sumw=sumw/nrp
c set up 3x3 matrix by multiplying through by transpose.
      do 20 i=1,nrp
        wt(i)=wt(i)/sumw
        ji=kdx(i)
        tim(i)=tp(i)-dly(kno,ji)-elvdly(i)
        pp=pp+c(ji)*c(ji)*wt(i)
        q=q+e(ji)*e(ji)*wt(i)
        r=r+c(ji)*e(ji)*wt(i)
        s=s+tim(i)*c(ji)*wt(i)
        t=t+tim(i)*e(ji)*wt(i)
        d=d+tim(i)*wt(i)
        u=u+c(ji)*wt(i)
        v=v+e(ji)*wt(i)
        y=y+wt(i)
   20 continue
c solve 3x3 by matrix equation
      alp1=pp*v-r*u
      bet1=r*v-q*u
      gam1=t*u-s*v
      alp2=r*y-u*v
      bet2=q*y-v*v
      gam2=d*v-t*y
      a=(gam1*bet2-gam2*bet1)/(alp2*bet1-alp1*bet2)
      b=-(gam1+alp1*a)/bet1
      cc=( s-a*pp-b*r)/u
      sum=0.0
      sumw=0.0
      btt=0.0
      if ( iprn.lt. 3) goto 25
      write(punt,21)
   21 format(51h velazm format 21. matrix equation a(3 by 3) x b(3),
     * 8h = c(3)./)
      write(punt,22) pp,r,u,a,s,r,q,v,b,t,u,v,y,cc,d
   22 format(3(5x,3f10.3,2(9x,f10.3),/))
   25 do 30 i=1,nrp
        ji=kdx(i)
        tx(i)=a*c(ji)+b*e(ji)+cc
        x(4,i)=tim(i)-tx(i)
        tx(i)=tx(i)-dt(i)
        if ( kdx(i).eq.1) btt=x(4,i)
        sumw=sumw+abs(x(4,i))*wt(i)
        sum=sum+(x(4,i)**2)*wt(i)
   30 continue
c calculate apparent velocity, standard deviation, and rms.
      vapp=1./sqrt(a**2+b**2)
      yy=y
      xmean=sumw/yy
      rms=sqrt(sum/yy)
      sd =sqrt(sum/(yy-3.0))
c determine azimuth being careful to pick the right quadrant
      if ((a.ne.0.0).or.(b.ne.0.0)) goto 300
      angn=999.
      goto 310
  300 angn=atan2( a,b)*57.29578
      if (angn.lt.0.0) angn=360.0+angn
  310 iang=angn+0.5
      jang=iang+180
      if (jang.ge.360) jang=jang-360
      kmin=jmin(1)
c write the output on the printer
      write(punt,500) kdate,khr,kmin,iang,jang,vapp,xmean,rms,sd
  500 format(/,
     * ' plane wave solution.  yrmody hr:min = ', i8, 1x, i2,':',i2, /,
     * ' azimuth of approach    = ', i5,
     * ' degrees clockwise from north.', /,
     * ' azimuth to source      = ', i5, ' degrees', /,
     * ' apparent velocity      = ', f6.2, ' km/sec', /,
     * ' mean of abs residuals  = ', f6.2, ' sec', /,
     * ' rms of the residuals   = ', f6.2, ' sec', /,
     * ' standard deviation     = ', f6.2, ' sec')
      write(punt,499) nsta(1)
  499 format(/, ' the residuals are relative to station ',a5,//,
     * '  sta   prmk  hrmn  p-sec  p-cal  dly/h1 elvdly p-res rel-',
     * 'res weight amax   dt',/)
      do 505 i=1,nrp
        ji=kdx(i)
        iamx=amx(i)+0.5
        rres=x(4,i)-btt
        write(punt,502)msta(i),krmp(i),khr,jmin(i),p(i),tx(i),
     *  dly(kno,ji),elvdly(i),x(4,i),rres,wt(i),iamx,dt(i)
  502   format(1x,a5,2x,a4,2x,2i2,7f7.2,i5,f7.2,f10.2)
  505 continue
      return
      end
c end velazm
