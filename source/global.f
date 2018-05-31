c global.for    []
      subroutine global
c this subroutine attempt to find the global minimum when there are
c more than one local minimum at different depths.  j. c. lahr 4/22/87
c
      include 'params.inc' 
      parameter (ndly = 11)
      real bat2,bon2,lat,lon
      logical good, eoff, supout
      integer punt
      common /punt/ punt
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
      logical ingulf
      common /gu/ ingulf
      common /hf/ cp(72),sp(72)
      common /hl/ nwad, tslope, tsqsl
      common /hpn/ wslope
      logical freor
      common /hopq/ savla,savlo,savez,savor,freor
      common /iclmpq/ lat(nsn),lon(nsn)
      common /logfil/ logfil
      common /idno/ ksel,ksort
      common /ilv/ c(nsn), e(nsn)
      common /imost/ test(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /ihfgpq/ itest(100)
      common /lm/ mapend
      common /ocfn/ adj,seh,lat1,bat2,lon1,bon2,igap
      character*1 isnla, isnlo, krm1, krm2
      common /ocfn1/ isnla, isnlo, krm1, krm2
      common /ohq/ gap, supout
      common /omnfh/ dmin,dmin3,sminp
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
      common /qgo/ org
      common /qmost/ wt(npa),z
      real latep,lonep
      common /qmost1/ lonep,ni,latep
      common /ro/ yse,seorg,phi
      common /reloc/ irelo, nreloc
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      common /zqr/ fno
c
      ingulf = .false.
c
c deep depth should be deep enough to catch deepest local mimima.
c we use 75. km in alaska.  add test(8) so that deepz is in km wrt
c the top of the model, not wrt sea level.
c     test(42) = 75. is the default.
      deepz = test(42) + test(8)
c
c a solution is first computed for z = 0 and for z free, starting at deepz.
c
c if the deep solution converges to a depth less than some cutoff,
c cutz, then the solution with smaller rms is taken as the best.
c we use 20 in alaska.  add test(8) so that deepz is in km wrt
c the top of the model, not wrt sea level.
c     test(27) = 20. is the default value
      cutz = test(27) + test(8)
c
c if the deep solution converges to a depth greater than cutz, then
c another solution is found starting at shalz = cutz/2.0.
c     shalz = 9. (old value)   new default value is 10.
      shalz = cutz/2.0
c
c emperical tests indicate that for alaska data this algorithm with
c the default values of test(27) and test(42) usually
c finds the global minimum.  for other networks, different values of
c deepz, cutz, and shalz may need to be used.
c
      if(iprn .ge. 5) then
        write(punt,91)
   91   format(' call global')
c show elapsed time
        call timit(1)
      endif
      instsv = inst
      if(instsv .eq. 8) freor = .false.
c set inst = 1 to fix depth
      inst = 1
c set savez to surface
      savez = 0.
      call quakes
c save this location and rms
      fla(1) = latep
      flo(1) = lonep
      fz(1) = z
      frms(1) = rms
      forg(1) = org
      rmslm1 = frms(1)
      if(fno .gt. 0.) rmslm1 = sqrt(frms(1)**2 + (yse**2)/fno)
c set savez to deep depth
      savez = deepz
      call quakes
c save this location and rms
      fla(2) = latep
      flo(2) = lonep
      fz(2) = z
      frms(2) = rms
      forg(2) = org
c now free up the depth and begin at deep depth again
      inst = 0
      savla = latep
      savlo = lonep
      call quakes
c if solution is in the gulf of alaska, then save it and return
      if(ingulf) then
	if(iprn .ge. 5) then
          write(punt, '(a)') 
     *    'Global deep-starting free solution ends up in gulf.'
        endif
        altla(1) = latep
        altlo(1) = lonep
        altz(1) = z
        altrms(1) = rms
        altorg(1) = org
        nsolut = 1
        return
      endif
c save this location and rms
      fla(3) = latep
      flo(3) = lonep
      fz(3) = z
      frms(3) = rms
      forg(3) = org
      rmslm3 = frms(3)
      if(fno .gt. 0.) rmslm3 = sqrt(frms(3)**2 + (yse**2)/fno)
      if(fz(3) .lt. cutz) then
c the solution must be shallow.
        maxf = 3
        if(fz(3) .le. 0.1) then
c in this case there is only one minimum, and it is at the surface
          altla(1) = fla(3)
          altlo(1) = flo(3)
          altz(1) = fz(3)
          altrms(1) = frms(3)
          altorg(1) = forg(3)
          nsolut = 1
          return
        else if(rmslm1 .lt. frms(3)) then
c in this case near the surface is significantly better,
c so run free location to derive information for output
          savez = fz(1)
          savla = fla(1)
          savlo = flo(1)
          inst = 0
          call quakes
c save this location and rms
          altla(1) = latep
          altlo(1) = lonep
          altz(1) = z
          altrms(1) = rms
          altorg(1) = org
          nsolut = 1
          return
        else if(frms(1) .gt. rmslm3) then
c the deeper solution is significantly better, so use it
          altla(1) = latep
          altlo(1) = lonep
          altz(1) = z
          altrms(1) = rms
          altorg(1) = org
          nsolut = 1
          return
        else
c in this case the difference in rms is less than limit
c these will be joined if there is not a boundary in between them.
          nsolut = 2
c the prefered solution will have the lesser rms
          if(frms(1) .le. rms) then
c the surface is at least slightly better or equal to deeper z.
c save current solution as secondary minimum.
            altla(2) = latep
            altlo(2) = lonep
            altz(2) = z
            altrms(2) = rms
            altorg(2) = org
c recompute information for output.
            savez = fz(1)
            savla = fla(1)
            savlo = flo(1)
            if (instsv .ne. 8) freor = .true.
            inst = 9
            call quakes
c save this location and rms
            altla(1) = latep
            altlo(1) = lonep
            altz(1) = z
            altrms(1) = rms
            altorg(1) = org
            return
          else
c the deeper solution is at least slightly better.
            altla(1) = latep
            altlo(1) = lonep
            altz(1) = z
            altrms(1) = rms
            altorg(1) = org
            altla(2) = fla(1)
            altlo(2) = flo(1)
            altz(2) = fz(1)
            altrms(2) = frms(1)
            altorg(2) = forg(1)
            return
          endif
        endif
      else
c the final depth is greater than cutz km when starting at deep depth
        altla(1) = latep
        altlo(1) = lonep
        altz(1) = z
        altrms(1) = rms
        altorg(1) = org
c compute a fixed depth solution at shallow depth
        savez = shalz
        savla = fla(1)
        savlo = flo(1)
        inst = 1
        call quakes
c save this location and rms
        fla(4) = latep
        flo(4) = lonep
        fz(4) = z
        frms(4) = rms
        forg(4) = org
        maxf = 4
c compare rms at surface and shallow depth and start free solution at better.
        if(frms(4) .le. frms(1)) then
c shallow depth is better than surface so start free solution there.
          inst = 0
          savla = latep
          savlo = lonep
          call quakes
c save this location and rms
          fla(5) = latep
          flo(5) = lonep
          fz(5) = z
          frms(5) = rms
          forg(5) = org
          rmslm5 = frms(5)
          if(fno .gt. 0.) rmslm5 = sqrt(frms(5)**2 + (yse**2)/fno)
          maxf = 5
        else
c surface is better than shallow depth so start free soltuion there.
          savez = fz(1)
          savla = fla(1)
          savlo = flo(1)
          inst = 0
          call quakes
c save this location and rms
          fla(5) = latep
          flo(5) = lonep
          fz(5) = z
          frms(5) = rms
          forg(5) = org
          rmslm5 = frms(5)
          if(fno .gt. 0.) rmslm5 = sqrt(frms(5)**2 + (yse**2)/fno)
          maxf = 5
        endif
c       if(abs(fz(5) - fz(3)) .lt. 5.) then
        if(abs(fz(5) - fz(3)) .lt. deepz/10.) then
c the depths are so close together that these solutions are equivalent.
          nsolut = 1
          if(frms(5) .le. frms(3)) then
            altla(1) = fla(5)
            altlo(1) = flo(5)
            altz(1) = fz(5)
            altrms(1) = frms(5)
            altorg(1) = forg(5)
            return
          else
c compute fixed depth solution for output values
            savla = fla(3)
            savlo = flo(3)
            savez = fz(3)
            if (instsv .ne. 8) freor = .true.
            inst = 9
            call quakes
c save this location and rms
            altla(1) = latep
            altlo(1) = lonep
            altz(1) = z
            altrms(1) = rms
            altorg(1) = org
            return
          endif
        else
c the depths are far enough appart that there may be two solutions.
          if(frms(5) .gt. frms(3)) then
c the deep solution is at least slightly better
            altla(1) = fla(3)
            altlo(1) = flo(3)
            altz(1) = fz(3)
            altrms(1) = frms(3)
            altorg(1) = forg(3)
            altla(2) = fla(5)
            altlo(2) = flo(5)
            altz(2) = fz(5)
            altrms(2) = frms(5)
            altorg(2) = forg(5)
            nsolut = 1
            savla = fla(3)
            savlo = flo(3)
            savez = fz(3)
            if (instsv .ne. 8) freor = .true.
            inst = 9
            call quakes
          else
c the shallow solution is at least slightly better
            altla(2) = fla(3)
            altlo(2) = flo(3)
            altz(2) = fz(3)
            altrms(2) = frms(3)
            altorg(2) = forg(3)
            altla(1) = fla(5)
            altlo(1) = flo(5)
            altz(1) = fz(5)
            altrms(1) = frms(5)
            altorg(1) = forg(5)
          endif
          if(frms(5) .gt. rmslm3) then
c the deep solution is significantly better
            nsolut = 1
            return
          else if(frms(3) .gt. rmslm5) then
c the shallow solution is significantly better
            nsolut = 1
            return
          else
c report two solutions
            nsolut = 2
            return
          endif
        endif
      endif
      end
c end global
