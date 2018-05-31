c geterr.for    []
      subroutine geterr(zup, zdn, rmslim)
c this subroutine will find the error limits for depth.
c j. c. lahr 4/22/87
      include 'params.inc' 
      parameter (ndly = 11)
      real bat2,bon2,lat,lon
      logical good, eoff, supout
      common /gg/ altla(3), altlo(3), altz(3), altrms(3), nsolut,
     * fla(20), flo(20), fz(20), frms(20), forg(20), maxf, altorg(3)
      character*4 iahead*60, msta*5, nsta*5, icard*110
      common /char/ iahead, msta(npa), nsta(nsn), icard
      character*4 ipro, ichec, evtype*1, evstat*1, root*256
      common /dbhip/ ipro, ichec, evtype, evstat, root
      common /dmost/ ipun,ivlr,blank
      common /dhil/ iq,ilat,kms,icat
      common /dhip/ inpt,isa,ilis,inmain,injump
      common /dhio/ nedit,good
      common /dio/ rmsmx,presmx,sresmx,noutmx,nimx,iprun,semx
      common /dph/ noswt, eoff
      common /ghnq/ iexit
      common /gmost/ az(npa)
      common /hf/ cp(72),sp(72)
      common /hl/ nwad, tslope, tsqsl
      common /hpn/ wslope
      logical freor
      common /hopq/ savla,savlo,savez,savor,freor
      common /iclmpq/ lat(nsn),lon(nsn)
      common /logfil/ logfil
      common /idno/ ksel,ksort
      common /ilv/ c(nsn), e(nsn)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /lm/ mapend
      common /ocfn/ adj,seh,lat1,bat2,lon1,bon2,igap
      character*1 isnla, isnlo, krm1, krm2
      common /ocfn1/ isnla, isnlo, krm1, krm2
      common /ohq/ gap, supout
      common /omnfh/ dmin,dmin3,sminp
      common /on/ ix,iy,iz,ax1,axz
      common /ph/ nfirst
      common /pno/ lph,keyph(npa),nsum
      character*4 krms
      common /po/ krms(npa)
      common /pfnoqv/ kdate,khrmn,khr
      common /pgqv/ w(npa)
      common /phoqn/ inst,knst
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /pnoqtx/ ldx(npa)
      character*4 krmp
      common /pnoqv/ krmp(npa)
      common /pot/ ts(npa),ihr,model(npa), keyphi(npa)
      common /qmost/ wt(npa),z
      real latep,lonep
      common /qmost1/ lonep,ni,latep
      common /ro/ yse,seorg,phi
      common /reloc/ irelo, nreloc
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      common /zqr/ fno
c
c given a local minimum, find the depth limits within which the
c rms will not increase by more than expected, given yse, the
c reading standard error.
c
c set iprn = -2 to supress printing
      isvprn = iprn
      iprn = -2
      rmslim = sqrt(altrms(1)**2 + (yse**2)/fno)
c
c first consider the case of only one solution
      if(nsolut .eq. 1) then
c
c
        call upward(altrms(1), altla(1), altlo(1), altz(1),
     *  frms(1), rmslim, zup, axz)
c
        call dwnwrd(altrms(1), altla(1), altlo(1), altz(1),
     *  rmslim, zdn, axz)
c
      else
c next consider the case of two solutions
        if(altz(1) .lt. altz(2)) then
c in this case, the primary solution is more shallow.
c get the upper limit above the more shallow solution.
          call upward(altrms(1), altla(1), altlo(1), altz(1),
     *    frms(1), rmslim, zup, axz)
c
c get the lower limit below the deeper solution.
          call dwnwrd(altrms(2), altla(2), altlo(2), altz(2),
     *    rmslim, zdn, axz)
          zdn = zdn + (altz(2) - altz(1))
        else
c in this case, the secondary solution is more shallow.
c get the upper limit above the more shallow solution.
          call upward(altrms(2), altla(2), altlo(2), altz(2),
     *    frms(1), rmslim, zup, axz)
          zup = zup + (altz(1) - altz(2))
c
c get the lower limit below the deeper solution.
          call dwnwrd(altrms(1), altla(1), altlo(1), altz(1),
     *    rmslim, zdn, axz)
        endif
c
      endif
      iprn = isvprn
      return
      end
c end geterr
