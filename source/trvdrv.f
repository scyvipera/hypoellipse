c trvdrv.for    []
      subroutine trvdrv
c---- compute travel time and derivatives from crustal model
      save
      include 'params.inc' 
      parameter (ndly = 11)
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      logical tttset, notified
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /dit/ tid(lmax,lmmax),did(lmax,lmmax),lowv,modin(mmax+3)
      common /dmost/ ipun,ivlr,blank
      common /gmost/ az(npa)
      common /imost/ test(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /idt/ v(lmax2)
      common /ilt/ vthk(2,nsn),ipdly(nsn),mod(nsn),ipthk(nsn)
      common /ilpu/ sw(nsn),ndate(nsn),nhr(nsn),mno(nsn),ielv(nsn)
      common /iot/ flt(2,nsn),thks(nsn)
      common /ilotv/ elvdly(npa)
      common /int/ thk(lmax+1),lbeg(mmax),lend(mmax),vi(lmax),vsq(lmax),
     *  vsqd(lmmax,lmax),f(lmax,lmmax),kl,ivway,sthk(mmax),sthk1(mmax),
     *  sdly(ndly,nsn)
      common /it/ vpvs(lmax2),vpvsm(mmax+3),nttab
      common /logfil/ logfil
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /pnoqtx/ ldx(npa)
      common /pot/ ts(npa),ihr,model(npa), keyphi(npa)
      common /pqt/near
      common /pt/ nlay(npa)
      common /qmost/ wt(npa),z
      real latep, lonep
      common /qmost1/ lonep,ni,latep
      common /qgo/ org
      common /qgnotx/ delta(npa)
      common /tmost/ x(4,npa)
      common /obcfn/ ain(npa)
      common /tonxb/ t(npa),fms(npa)
      common /lvz/ jref(lmax)
      data notified/.false./
      imod = 0
      nearsta = kdx(near)
      call usedly(nearsta, zuse, modset, latep, lonep)
      tttset = .false.
      zpre = -1.e22
      do 30 idum = 1,nrp
      i = idum
      elvdly(i) = 0.0
      ista = kdx(i)
      if(modset .ne. 0) then
        imod = modset
      else
        imod = mod(ista)
      endif
cds      if((delta(i) .gt. test(51)) .and. (nttab .ne. 0)) imod = 11
      if((delta(i) .gt. test(51)) .and. (nttab .ne. 0)) imod = mmax + 1
cds   write(punt, *) 'delta=', delta(i), ' test(51)=', test(51),
cds  * ' nttab=', nttab
      model(i) = imod
      ismod = 0
cds      if (imod .gt. 10) then
cds   write(punt, *) 'trvdrv mmax=', mmax, ' imod=', imod
      if (imod .gt. mmax) then
c       first deal with the p phase **********************************
        if(.not. tttset) then
c         initialize all travel time tables for current depth
          tttset = .true.
          if(z .eq. zpre) go to 14
          zpre = z
          if(z .ge. 0.) then
	    call hyset(nttab, z)
	  else
	    call hyset(nttab, 0.0)
	  endif

          if((test(8) .ne. 0.0) .and. (.not. notified)) then
	    notified = .true.
            write(punt, 10)
            write(logfil, 10)
10          format('Warning:  the elevation of the top of the velocity',
     *        ' models (test(8)) ', /,
     *        'is not zero.  ',
     *        'This is OK for travel times to distant stations.')
          endif

        endif
c                  modin = 1,2, or 3
14	continue
cds	  write(punt, *) 'near statement 14'
cds          write(punt, *) 'calling hytab.  imod=', imod,
cds  *      ' modin(imod)=', modin(imod)
        call hytab(modin(imod), delta(i), t(i), ain(i), dtdd,
     *               dtdh, vt, vs)
        ista = kdx(i)
c       set elevation delay for p phase
        elvdly(i) = 0.0
        if (test(2) .ge. 0.0) then
          if (test(2) .gt. 0.0) then
            tes2 = test(2)
          else
            tes2 = vt
          endif
          utop = (sin(rad*ain(i))*tes2/vs)**2.0
          if(utop .gt. 1.) then
            write(punt, 20) ista,utop,i,ain(i),tes2,vs
            elvdly(i) = 0.0
          else
            elvdly(i) = (ielv(ista)/1000.)*(sqrt(1.0001-utop))/tes2
          endif
        endif
c       p phase derivatives
        x(1,i) = +dtdd*sin(az(i)*rad)
        x(2,i) = -dtdd*cos(az(i)*rad)
        x(3,i) = dtdh
        x(4,i) = tp(i) - t(i) - org - dly(kno,ista) - elvdly(i)
c       now for the s phase ******************************************
        if (ldx(i) .eq. 0) goto 30
        k = ldx(i)
        vpvsmk = vpvsm(imod)
        if (vpvsmk .lt. 0.0) then
c         use travel time table modin(imod) + 1 for the s phase
          model(k) = imod + 1
cds       write(punt, *) "calling hytab.  k=", k, ' imod=', imod,
cds  *      ' modin(imod)=', modin(imod)
          call hytab(modin(imod) + 1, delta(i), t(k),
     *               ain(k), dtdd, dtdh, vts, vss)
          x(1,k) = +dtdd*sin(az(i)*rad)
          x(2,k) = -dtdd*cos(az(i)*rad)
          x(3,k) = dtdh
          vpvsmk = vt/vts
        else
c         assume a constant vp/vs ratio for the s phase parameters
          t(k) = t(i)*vpvsmk
          model(k) = model(i)
          x(1,k) = x(1,i)*vpvsmk
          x(2,k) = x(2,i)*vpvsmk
          x(3,k) = x(3,i)*vpvsmk
          ain(k) = ain(i)
          vts = vt/vpvsmk
          vss = vs/vpvsmk
        endif
c       set elevation delay for s phase
        elvdly(k) = 0.0
        if (test(2) .ge. 0.0) then
          if (test(2) .gt. 0.0) then
            tes2 = test(2)/vpvsmk
          else
            tes2 = vts
          endif
          utop = (sin(rad*ain(k))*tes2/vss)**2.0
          if(utop .gt. 1.) then
            write(punt, 20) ista,utop,k,ain(k),tes2,vss
            elvdly(k) = 0.0
          else
            elvdly(k) = (ielv(ista)/1000.)*(sqrt(1.0001-utop))/tes2
          endif
        endif
        x(4,k) = tp(k) - t(k) - org - sdly(kno,ista) - elvdly(k)
        goto 30
      endif
c**********************************************************************
c     layer model calculations begin here *****************************
      vpvsk = vpvsm(imod)
      if(vpvsk .eq. 0.) vpvsk = test(1)
c     test(8) is the elevation in km of the top of the model
      stz = test(8) - ielv(ista)*.001
      if(stz .lt. 0.0) stz = 0.0
15    nlayers = lend(imod) - lbeg(imod) + 1
c     write(punt, *) 'trvdrv, imod = ', imod, ' lend(imod) = ',
c    *  lend(imod), ' v = ', v(lend(imod))
      if(v(lend(imod)) .lt. 100.) then
c----
c----   constant velocity within each layer with topography
c     variables associated with the variable layer thickness option
c     ivl              layer with variable thickness (=0 to skip this option)
c     vthk(1/2, ista)  model 1/2 thickness assigned to station ista (each
c                        station is assigned two thicknesses)
c     ipthk(ista)      preferred thickness model of station ista
c     lowv             1/0  do/don't make compensating change in thichkess
c                        of layer below the variable layer
c     ivway            neg/0/pos  source/average/receiver station variable
c                        thickness is used
        ivl = lbeg(imod) + ivlr - 1
        if(ivlr .gt. 0) then
c----     determine thickness of the variable layer.
c----     ivway :::  negative-source, positive-receiver, zer0-average
          if(ivway .lt. 0) then
            thk(ivl) = vthk(ipthk(nearsta), ista)
          else if(ivway .gt. 0) then
            thk(ivl) = vthk(ipthk(ista), ista)
          else
            thk(ivl) =
     *        (vthk(ipthk(nearsta),nearsta) +
     *        vthk(ipthk(nearsta),ista))*0.5
            sum = 0.0
            do 16 l = lbeg(imod),ivl
              sum = sum+thk(l)
16          continue
            if (z .ge. sum) then
              thk(ivl) = vthk(ipthk(nearsta),ista)
            endif
          endif
          thk(ivl + 1) = sthk1(imod) - lowv*(thk(ivl) - sthk(imod))
          if(thk(ivl+1) .le. 0.0) thk(ivl+1) = 0.01
          thks(i) = thk(ivl)
        endif
        k = ldx(i)
        if (k .ne. 0) wtk = wt(k)
        call trvcon( delta(i), z, t(i), ain(i), dtdd, dtdh,
     *            lbeg(imod), lend(imod), lbeg(imod)-1, nlayers,
     *            ivlr, ivl, thk, nlay(i), ldx(i), wt(i), wtk,
     *            tid, did, jref, vsq, vsqd, v, vi, f,
     *            vs, vt, msta(i), stz)
        if (k .ne. 0) wt(k) = wtk
      else if(v(lend(imod)) .eq. 100.) then
c----
c----   linear increase, no half space
        write(punt, 17)
        write(logfil, 17)
17      format(' a linear increase with no half space is ',
     *   ' no longer allowed, so stop')
        stop 'abort from trvdrv'
      else if(v(lend(imod)) .eq. 200.) then
c----
c----   linear increase over half space with topography
        vzero = v(lbeg(imod))
        grad  = v(lbeg(imod) + 1)
        zhalf = v(lbeg(imod) + 2)
        vhalf = v(lbeg(imod) + 3)
c       linvol incorporates station elevations
c       print *, '1: i, delta(i), z, stz ', i, delta(i), z, stz
        call linvol(delta(i), z, stz, grad, vzero, zhalf,
     *    vhalf, t(i), ain(i), dtdd, dtdh)
        vt = vzero + stz*z
        vs = vzero + grad*z
        if(z .gt. zhalf) vs = vhalf
      else if(v(lend(imod)) .eq. 300.) then
c----
c----   linear increase with topography
        vzero = v(lbeg(imod))
        grad  = v(lbeg(imod) + 1)
        call linv(delta(i),   z, vzero, grad,
     *    t(i), ain(i), dtdd, dtdh, stz, vt, vs)
cd    else if(v(lend(imod)) .eq. 400.) then
c----
c----   lienert velocity routine with embedded stations allowed
c     dimension parm(lmax2)
c   inputs:    delta  distance from eq to station in km
c              stz    depth of station in km below top of model
c              eqz    depth of eq in km below top of model
c              nn     number of parameters ( = 2*nl - 1)
c              parm(i), i = 1, nl   layer velocities
c              parm(i), i = nl + 1, nn  layer thicknesses
c   outputs:
c              tmin   minimum travel time
c              dtdd   partial derivative of tt wrt distance (s/km)
c              dtdh   partial derivative of tt wrt depth (s/km)
c              ian    angle of incidence in degrees
c       the last layer is excluded, so
cd      nlayers = lend(imod) - lbeg(imod)
cd      do 18 itmp = 1, nlayers
cd        parm(itmp) = v(lbeg(imod) + itmp - 1)
cd        if(itmp .lt. nlayers)
cd   *      parm(itmp+nlayers) = thk(lbeg(imod) + itmp - 1)
cd18    continue
cd      call dtdx2(2*nlayers-1, parm, delta(i), z , stz, t(i),
cd   *  dtdd, dtdh, ain(i), vt, vs)
      else
        write(punt, 19) imod, v(lend(imod))
        write(logfil, 19) imod, v(lend(imod))
19      format(' halfspace velocity for model ', i2, ' may not equal ',
     *    f10.2, /, 'so stop!')
        stop 'abort from trvdrv'
      endif
c----
      elvdly(i) = 0.0
c     if test(8) is non zero, then elevations are accounted for by
c       the location of the stations within the model.
      if ((test(2) .ge. 0.0) .and. (test(8) .eq. 0.0)) then
        if(test(2) .gt. 0.0) then
c----     elevation correction using test(2) as surface velocity
          if(ismod .eq. 1) then
            tes2 = tes2/vpvsk
          else
            tes2 = test(2)
          endif
        else
c----     elevation correction using 1'st layer velocity as surface velocity
          tes2 = vt
        endif
        utop = (sin(rad*ain(i))*tes2/vs)**2.
        if(utop .gt. 1.) then
          write(punt, 20) ista,utop,i,ain(i),tes2,vs
20        format(' sub trvdrv: velocity for elev correction',
     *      ' should not be greater than velocity at source.', /,
     *      ' for ista =',i5,' utop =',f12.7,' ain(',i5,') =',f12.7,
     *     /' v for elev cor =',f12.7,' and v at source =',f12.7)
          elvdly(i) = 0.
        else
          elvdly(i) = (ielv(ista)/1000.)*(sqrt(1.0001-utop))/tes2
        endif
      endif
c---- calculate the partial derivatives of t wrt lon, lat, & z
c----    and find the residuals.
c----
c---- p phase derivatives
      x(1,i) = +dtdd*sin(az(i)*rad)
      x(2,i) = -dtdd*cos(az(i)*rad)
      x(3,i) = dtdh
      if(ismod .eq. 0) go to 27
c---- s phase model just used
      x(4,i) = tp(i) - t(i) - org - sdly(kno,ista) - elvdly(i)
      go to 30
27    if(ksmp(i) .eq. 0) go to 29
c---- p phase residuals
      x(4,i) = tp(i) - t(i) - org - dly(kno,ista) - elvdly(i)
      if(ldx(i).eq.0) go to 30
c---- check for s model
      if(vpvsm(imod) .eq. 0.0) then
        ismod = 1
        i = ldx(i)
        imod = imod + 1
        model(i) = imod
        go to 15
      endif
c---- s phase data -- based on constant vpvs
      k = ldx(i)
      model(k) = imod
      elvdly(k) = elvdly(i)*vpvsk
      t(k) = t(i)*vpvsk
      x(1,k) = x(1,i)*vpvsk
      x(2,k) = x(2,i)*vpvsk
      x(3,k) = x(3,i)*vpvsk
      x(4,k) = tp(k) - t(k) - org - sdly(kno,ista) - elvdly(k)
      ain(k) = ain(i)
      go to 30
c---- s minus p data
29    x(1,i) = x(1,i)*(vpvsk-1.)
      x(2,i) = x(2,i)*(vpvsk-1.)
      x(3,i) = x(3,i)*(vpvsk-1.)
      x(4,i) = ts(i)-tp(i)-(vpvsk-1.)*(t(i)+elvdly(i))
     *  + dly(kno,ista) - sdly(kno,ista)
30    continue
      return
      end
c end trvdrv
