c usedly.for    []
      subroutine usedly(k, zuse, modset, alat, alon)
c
c this sub is set up by each user to make such parameters as
c    delay model (kno), crustal model (modset), starting
c    depth maximum (test36)), and (or) starting depth (zuse)
c    a function of eq location.
c
c     integer   kno
c               kno     output - delay model to use on next iteration
c
      integer   k
c               k       input - index of station for which travel time
c                               will be computed next, unless called
c                               from hymain, in which case k = 0.
      real      zuse
c               zuse    output - set to 99999. of starting depth is not
c                                being set by this sub.
      integer   modset
c               modset  output - next crustal model to be used.  if zero,
c                                then use model preferred by each station.
      real      px(6), py(6)
c     real      px(6), py(6)     x and y values defining the gulf of alaska
c
      include 'params.inc' 
      parameter (ndly = 11)
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      logical ingulf
      real lat, lon
      character*1 bksrc
      integer punt
      common /punt/ punt
      common /dipu/ ipkdly, igsum, bksrc
      common /gu/ ingulf
      common /hopq/ savla,savlo,savez,savor,freor
      common /iclmpq/ lat(nsn),lon(nsn)
      common /ihfgpq/ itest(100)
      common /ilt/ vthk(2,nsn),ipdly(nsn),mod(nsn),ipthk(nsn)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /int/ thk(lmax+1),lbeg(mmax),lend(mmax),vi(lmax),vsq(lmax),
     *  vsqd(lmmax,lmax),f(lmax,lmmax),kl,ivway,sthk(mmax),sthk1(mmax),
     *  sdly(ndly,nsn)
      common /imost/ test(100)
      common /logfil/ logfil
      character mtyp*2
      common /iu/ mtyp(mmax)
c if instset .ne. ' ', then put this new value of inst on summary record
      character*1 instset
      common /hnu/ instset
      common /phoqn/ inst,knst
      common /qmost/ wt(npa),z
      common /qmost1/ lonep,ni,latep
      character*1 ins, iew
      data px/-436.41858,  56.64243, 225.09338, 469.42575,
     * 573.36530, 745.52167/
      data py/-533.93683,-166.69151,-138.00677,-283.51489,
     *-303.27237,-489.55786/
cd    if(k .eq. 0) then
cd      print *, 'usedly called from hymain with k = 0'
cd    else
cd      print *, 'index of station for which travel time delay'
cd      print *, 'needs to be computed is ', k
cd    endif
c zuse of 99999. will be ignored in setting the starting depth.
      zuse = 99999.
      if (ipkdly .gt. 0) then
c select delay model basesd on closest station
        if(k .eq. 0) then
          kno = ipdly(1)
        else
          kno = ipdly(k)
        endif
c       crustal model will be set to modset, unless modset = zero.
c       in which case the model preferred by each station will be used.
        modset = 0
        return
      else
c convert eq location to geographic coordinates> bla, blo
        call unfold2(alat, alon, la, ins, ala, lo, iew, alo)
        bla = la + ala/60.
        if(ins .eq. 's') bla = -bla
        blo = lo + alo/60.
        if (iew .eq. 'e') blo = -blo
      endif
cd    print *, 'eq location is (geographic) ', bla, blo
cd    print *, 'ipkdly = ', ipkdly
      if (ipkdly .eq. 0) then
c **************************************************
c this is the old option for selecting delay and velocity models
c **************************************************
        ingulf = .false.
        if ((bla .lt. 59.7) .and. (blo .lt. 154.)
     *  .and. (blo .gt. 135.)) then
cd         print *, 'roughly in gulf of alaska'
          if ((bla .lt. 57.9) .and. (blo .lt. 146.)
     *    .and. (blo .gt. 137.3)) then
cd           print *, 'definately in gulf of alaska'
            ingulf = .true.
          else
            call delaz(lat(1), lon(1), delt, deldeg, azz, alat,alon)
cd           print *, 'reference station (geocentric) ', lat(1), lon(1)
cd           print *, 'location of epicenter wrt reference station is'
cd           print *, 'azimuth (deg) = ', azz
cd           print *, 'distance (km) = ', delt
            x0 = delt*sin(azz*rad)
            y0 = delt*cos(azz*rad)
            ini = inside(x0, y0, px, py, 6)
            if (iabs(ini) .eq. 2) ingulf = .true.
          endif
        endif
        if (ingulf) then
          if((inst .eq. 0) .or. (inst .eq. 8))then
            kno = 5
            modset = 5
            inst = 1
            instset = '1'
            savez = 13.
            zuse = 13.
            z = 13.
            test(36) = 13.
          else
            kno = 5
            modset = 5
            test(36) = 13.
          endif
          return
        else if (blo .gt. 148) then
c western
          kno = 1
          test(36) = 100.
        else if (blo .gt. 144.5) then
c central
          kno = 2
          test(36) = 45.
        else if (((blo .le. 142.25) .and. (blo .ge. 138.)) .and.
     *     (bla .lt. 61.)) then
c    *     ((bla .lt. 61.) .and. (bla .gt. 59.25))) then
c icy bay
          kno = 3
          test(36) = 25.
        else
c eastern, except for icy bay
          kno = 4
          modset = 4
          test(36) = 25.
        endif
      else if (ipkdly .lt. 0) then
c ***********************************************************
c this is the new option for use of cylindrical delay regions
c ***********************************************************
c if iteration number is .gt. test(37) then do not change
c crustal model number or delays.  this is to prevent boundary
c chatter.
        if(ni .gt. itest(37)) return
c ***********************************************************
c
c       cyldly computes delay model ndly, based on the current eq
c       location and the cylindrical delay domains.
        call cyldly
     *  (kno, alat, alon, lat(1), lon(1), x0, y0, z, dly, sdly, iprn,
     *   modset, test(8))
c	   if cyldly returns modset .ne. 0, then it will not be 
c	    reset below, except for gulf of alaska.
c          kno=1 is the default delay model
c	   kno=2,3, and 4 are the volcanic axis delay models
c	   kno=5 is the gulf of alaska delay model
c	   kno is set to 11 if a combined delay model is computed.
cd       print *, 'reference station (geocentric) ', lat(1), lon(1)
cd       print *, 'location of epicenter wrt reference station is'
cd       print *, 'x0, y0 = ', x0, y0
cd       print *, 'next select velocity model based on eq location'
        ingulf = .false.
        if ((bla .lt. 59.7) .and. (blo .lt. 154.)
     *  .and. (blo .gt. 135.)) then
cd         print *, 'roughly in gulf of alaska'
          if ((bla .lt. 57.9) .and. (blo .lt. 146.)
     *    .and. (blo .gt. 137.3)) then
cd           print *, 'definately in gulf of alaska'
            ingulf = .true.
          else
            ini = inside(x0, y0, px, py, 6)
            if (iabs(ini) .eq. 2) then
cd             print *, 'in gulf of alaska, based on sub. inside'
              ingulf = .true.
              if(iprn .ge. 5) write (punt, *) ' in gulf'
            endif
          endif
        endif
        if (ingulf) then
c         this overrides the cylinder specification
	  kno = 5
          if((inst .eq. 0) .or. (inst .eq. 8))then
            modset = 1
            inst = 1
            instset = '1'
            savez = 13.
            zuse = 13.
            z = 13.
            test(36) = 13.
          else
            modset = 1
            test(36) = 13.
          endif
          return
        else if (bla .lt. 62.5) then
cd         print *, 'southern region'
           if(iprn .ge. 5) write (punt, *) ' in southern region'
          if(modset .eq. 0) modset = 2
          test(36) = 100.
        else
cd         print *, 'northern region'
           if(iprn .ge. 5) write (punt, *) ' in northern region'
          if(modset .eq. 0) modset = 3
          test(36) = 50.
        endif
      endif
      if (modset .le. kl) return
cd     print *, 'modset, kl ', modset, kl
      if ( (modset .lt. 11) .or. (modset .gt. 13) ) then
        write(logfil, 80) modset
        write(punt, 80) modset
80      format (' usedly may not set the crustal model to ', i5,
     *          ', because that model has not been defined.', /,
     *          ' xxxx stop xxxx')
        stop
      endif
      return
      end
c end usedly
