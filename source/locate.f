c locate.for    []
      subroutine locate(iterm)
      save sockets
      include 'params.inc' 
      parameter (ndly = 11)
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      real bat2,bon2,lat,lon
      character*120 erout
      character*4 malaz, malla, malor, dnstrg
      logical good, eoff, supout, sockets, needsum
      character*1 kwr, iqdo
c     if instset .ne. ' ', then put this new value of inst on summary record
      character*1 instset
      integer punt
      common /punt/ punt
      common /hnu/ instset
      common /anox/ keyd(npa)
      common /bqz/ avrps,avuse
      character*4 iahead*60, msta*5, nsta*5, icard*110
      common /char/ iahead, msta(npa), nsta(nsn), icard
      character*4 ipro, ichec, evtype*1, evstat*1, root*256
      common /dbhip/ ipro, ichec, evtype, evstat, root
      common /dmost/ ipun,ivlr,blank
      common /dhin/ iglob, zup, zdn
      common /dhil/ iq,ilat,kms,icat
      common /dhip/ inpt,isa,ilis,inmain,injump
      common /dhio/ nedit,good
      common /dio/ rmsmx,presmx,sresmx,noutmx,nimx,iprun,semx
      common /dph/ noswt, eoff
      common /gg/ altla(3), altlo(3), altz(3), altrms(3), nsolut,
     * fla(20), flo(20), fz(20), frms(20), forg(20), maxf, altorg(3)
      common /ghnq/ iexit
      common /gmost/ az(npa)
      common /hf/ cp(72),sp(72)
      common /hl/ nwad, tslope, tsqsl
      real*8 time1, time2
      common /hop/ time1,time2,nopu,notim
      common /hpn/ wslope
      logical freor
      common /hopq/ savla,savlo,savez,savor,freor
      common /iclmpq/ lat(nsn),lon(nsn)
      common /logfil/ logfil
      common /idno/ ksel,ksort
      common /igl/ nglobalzs, globalzs(20)
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
      common /pbnoq/ kwr(npa),iqdo(npa)
      common /ph/ nfirst
      common /pno/ lph,keyph(npa),nsum
      character phcard*117, krm*2, seq*5
      common /pno1/ phcard(npa), krm(npa), seq
      character*4 krms
      common /po/ krms(npa)
      common /pqt/ near
      common /pfnoqv/ kdate,khrmn,khr
      common /pgqv/ w(npa)
      common /phoqn/ inst,knst
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /pnoqtx/ ldx(npa)
      character*4 krmp
      common /pnoqv/ krmp(npa)
      common /pot/ ts(npa),ihr,model(npa), keyphi(npa)
      common /qgnotx/ delta(npa)
      common /qw/ azmv(5), iuse(npa), duse(npa), ndwt, iboxc
      common /qmost/ wt(npa),z
      common /reloc/ irelo, nreloc
      common /ro/ yse,seorg,phi
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      data sockets/.false./
      if (iterm .gt. 0) then
        sockets = .true.
      endif
      if(iprn .ge. 5) write(punt, '(a)') 'begin subroutine locate'
20    if(itest(38) .eq. 11) itest(38) = 1
c read in data for one earthquake
      call phasin
      needsum = .true.
      instset = ' '
      if((lph .gt. 0) .and. (nr .eq. 0)) then
c case of no phase data
        iexit = 1
        write(logfil, 22) kdate, ihr, kmin
        write(punt, 22) kdate, khrmn/100, khrmn - 100*(khrmn/100)
22      format(' could not locate event of ', i6, ' ', i2, ':', i2)
c---- write out warning messages
        rewind(16)
        read(16, '(a)') erout
2296    read(16, '(a)', end=2298) erout
        if(erout(1:3) .eq. 'end') goto 2298
        write(punt, '(a)') erout
        write(logfil, '(a)') erout
        goto 2296
2298    continue

        if((ipun .eq. 2) .or. (ipun .eq. 3)) call npunch('final')
        if(eoff) goto 180
        goto 20
      endif
c at end of file
      if(eoff) goto 180
c no data here, but try main input file again
      if(nr .eq. 0) goto 20
      if ((nr .eq. 1) .and. (ipro .eq. 'stop')) then
c print summary
        call lissum(2)
c terminate, even if using sockets
        stop
      endif
      if ((nr .eq. 1) .and. (ipro .eq. 'rese')) then
c "reset" record found
        if (ichec .eq. 't s ') then
c "reset s" record found
c print summary
          call lissum(2)
c reset summary
          call lissum(1)
c show elapsed time
          call timit(1)
          time1 = 0.0d0
        endif
c get new parameters
        call input1
        goto 20
      endif
c normal earthquake case
      kkdate = kdate
      khr = ihr
      kmin = lbastm
      sec = 0.0
      dum = 0.0
      call tshift(kkdate,khr,kmin,sec,dum)
      kkyr=kkdate/10000
      kkmo=(kkdate-kkyr*10000)/100
      kkday=(kkdate-kkyr*10000-kkmo*100)
      if(iprn .lt. -1) then
c no print in this case
      else if(iprn .eq. -1) then
c one line per event in output and log file
        write(punt, 24) kkyr,kkmo,kkday,khr,kmin
        write(logfil, 24) kkyr,kkmo,kkday,khr,kmin
24      format(1x, i2.2, '/', i2.2, '/', i2.2 , 4x, i2.2, ':', i2.2,
     *  5x, a)
      else
        write(punt,26)
26      format('                                              ---------',
     * '-------------------------------------------------------',
     *      /'                                              ---------',
     * '-------------------------------------------------------',
     *      /'                                              ---------',
     * '-------------------------------------------------------')
        write(punt,24)kkyr,kkmo,kkday,khr,kmin,iahead
      endif
c if desired, call wadati
      wslope = 0.
      if(test(49) .ne. 0.) then
        call wadati(nr, nrp, w, ldx, tp, kdate, khrmn, iprn,
     *  ilis, krmp, krms, msta, test, wslope, worig, se3)
        if(wslope .ge. 1.2) then
          tslope = tslope + wslope
          tsqsl = tsqsl + wslope*wslope
          nwad = nwad + 1
          if(freor) savor = worig
          if(test(49) .lt. 0.) then
            savor = worig
            inst = 8    
          endif    
        else  
          if((wslope .ne. 0.) .and. (iprn .ge. 1)) then
            write(punt, '(a,/,a)') 
     *      ' VP/VS slope was less than 1.2, so it will not be used to', 
     *      ' set initial origin time nor to compute average VP/VS.' 
	  endif
        endif 
      endif
c find improved first trial location using half space velocity = test(48)
      vhalf = test(48)
      nmax = 10
c if any initial values = 99999. at this time, then compute them from p times!
      if(iprn .ge. 5) write(punt, *) ' savla, savlo = ', savla, savlo
      if( (savla .eq. 99999.) .or. (savez .eq. 99999.) ) then
        if(iprn .ge. 5) then 
          write(punt, *) ' number of p times ', nrp
          write(punt, *) ' p times ', (tp(i), i = 1, nrp)
          write(punt, *) ' weights ', (w(i), i = 1, nrp)
          write(punt, *) ' savla, savlo, savez, savor ',
     *          savla, savlo, savez, savor
          write(punt, *) ' call strtep'
        endif
        npinv = nrp
        if(npinv .gt. 9) npinv = 9
        if((iprn .ge. 1) .and. (ilis .gt. 0)) write(punt, 28) npinv
28      format(' compute starting parameters from the first ',
     *  i3, ' p-arrival times.')
        call strtep(c, e, kdx, lat(1), lon(1), nrp, nmax, vez,
     *                   vla, vlo, savor, tp, vhalf, w, test(54))
        if(iprn .ge. 5) then 
          write(punt, *) ' strtep input and output:'
          write(punt, *) ' lat(1), lon(1) = ', lat(1)*deg, lon(1)*deg
          write(punt, *) ' nrp, nmax = ', nrp, nmax
          write(punt, *) ' vhalf = ', vhalf
          write(punt, *) ' savor ', savor
          write(punt, *) ' vla, vlo, vez ', vla*deg, vlo*deg, vez
        endif
c
c define savla and savlo
        if(savla .eq. 99999.) then
c in this case, redefine savla and savlo from strtep results
	  if(iprn .ge. 5) 
     *     write(punt, *) 'savla = 99999. so use vla and vlo'
          savla = vla
          savlo = vlo
        endif
c
c set starting depth
        if(savez .eq. 99999.) then
c usedly uses the current values of savla and savlo.
c zuse is set to 99999. by usedly if usedly does not set starting depth.
c if zuse is set to 99999. then test(36) is the maximum starting depth.
cd        write(punt, *) 'call usedly to get starting depth'
          call usedly(0, zuse, modset, savla, savlo)
          if(zuse .ne. 99999.) then
            savez = zuse
          else if(test(5) .ne. -99.) then
            savez = test(5) + test(8)
          else
c use vez determined by strtep
c limit vez to the range 15 to test(36) km and round to the nearest 5 km
            if(vez .gt. test(36) + test(8)) vez = test(36) + test(8)
            if(vez .lt. 15. + test(8)) vez = 15. + test(8)
            nz = vez + .5
            savez = nz
          endif
        endif
      endif
      if(savor .eq. 99999.) savor = 0.
c
c find location for one earthquake
      if(itest(50) .gt. 22) itest(50) = 22
      irep = itest(50)
30    continue
cd    write(punt, *) 'call usedly from statement 91'
      call usedly(0, zuse, modset, savla, savlo)
      if((dnstrg(evtype) .eq. 't') .or. 
     *   (dnstrg(evtype) .eq. 'n') .or. 
     *   (dnstrg(evtype) .eq. 'r')) then
c       teleseism, nuclear or regional
        call velazm
        iexit = 1
        if((ipun .eq. 2) .or. (ipun .eq. 3)) call npunch('final')
        if(eoff) goto 180
        if(ipro .eq. 'more') goto 96
        goto 20
      endif

      if(nglobalzs .ne. 0) then
	call glob_new
	goto 40
      endif

      if((iglob .eq. 0) .and. 
     *   ((inst .eq. 0) .or. (inst .eq. 8))) then
        call global
        if ((nsolut .gt. 1) .and. (iprn .ge. 0) .and. (ilis .gt. 0))
     *  write(punt, 32) nsolut, (altz(i), altrms(i), i = 1, nsolut)
32      format(' the number of solutions = ', i2, /,
     *         '     depth      rms', /,
     *         ( 2f10.2))
        call output(1)
        if(iexit .eq. 0) then
          if((kms.eq.0) .and. (needsum)) call mising
c keep running sum of magnitude residuals for sumout
c but do not include multiple solutions of same event in summary
          if(needsum) call lissum(0)
          if(nfirst .ge. iabs(itest(7)) )  call fmplot
c write summary data to temporary files 14 and 15, call geterr and
c then add the the data to files 4 and 11
          if(ipun .ne. 0) call npunch('temp ')
          call geterr(zup, zdn, rmslim)
c add zup and zdn to summary record and write output to .sum and .arc
cd        write(punt, *) 'call adderr with zup, zdn = ', zup, zdn
          if(ipun .ne. 0) call adderr(zup, zdn)
          if (iprn .ge. 0) write(punt, 34) zup, zdn, yse, rmslim
34        format(' depth may decrease by ', f10.2,
     *    ' or increase by ', f10.2, ' given a reading ', /,
     *    ' standard error of ', f8.3, ' and rms limit of', f8.3)
        else
c iexit = 1, so there is insufficient data for a solution
          if((ipun .eq. 2) .or. (ipun .eq. 3)) then
            call npunch('final')
          endif
        endif
        if((itest(44) .eq. 1) .and. (inst .ne. 9)) then
c since (44) = 1, check if this is a debug event to rerun
          if((nedit .eq. 3) .and. (.not. good)) then
            itest(44) = 100
            inst = 0
            goto 40
          endif
        endif
c check for critical solution based on itest(44) = 2
        if ((itest(44) .eq. 2) .and. (inst .ne. 9)) then
c  continue iteration with only critical stations
          if (iprn .ge. 5) write(punt, '(a)')
     *    ' continue iteration with only critical stations'
          itest(44) = 200
          inst = 0
          goto 40
        endif
        goto 50
      endif
c
c normal solution (not global)
40    call quakes
c if iexit is set = 2 in quakes there are too few phases to
c rerun event with only critical stations.
      if(iexit .eq. 2) then
        if(itest(44) .eq. 100) itest(44) = 1
        if(itest(44) .eq. 200) itest(44) = 2
        goto 50
      endif
      call output(1)
c if iexit is set = 1 in quakes there is insufficient data
      if(iexit .eq. 1) then
        if((ipun .eq. 2) .or. (ipun .eq. 3)) then
          call npunch('final')
        endif
        goto 20
      endif
      if (abs(test(6)) .gt. 0.01) then
        call boxau
      endif
      if(itest(44) .eq. 100) then
        itest(44) = 1
        goto 50
      endif
      if (itest(44) .eq. 200) then
        itest(44) = 2
        goto 50
      endif
      if((kms.eq.0) .and. (needsum) .and.
     * (.not. supout)) call mising
      if( (nfirst .ge. iabs(itest(7))) .and.
     * (.not. supout)) call fmplot
c keep running sum of magnitude residuals for sumout
c do not include multiple solutions of same event in summary
      if((needsum) .and. (.not. supout)) call lissum(0)
      if(ipun .ne. 0) call npunch('final')
      if(itest(50) .ne. 0) then
        if(irep .eq. 0) then
c place a blank record between sets of summary records
          write(4, '(1h1)')
c go on to the next set of phase data
          supout = .false.
          goto 50
        endif
        if(irep .eq. itest(50)) then
c this is the first extra run of this event
c set sepout to true to supress calling output and
c computing magnitude
          supout = .true.
c set inst = 1 to fix z
          inst = 1
c set starting depth to surface of model
          savez = 0.0
          zdif = 1.
        else
c on later extra runs
          savez = savez + zdif
          zdif = 1. + savez/5.
        endif
        irep = irep - 1
        goto 30
      endif
      if((itest(44) .eq. 1) .and. (inst .ne. 9)) then
c since (44) = 1, check if this is a debug event to rerun
        if((nedit .eq. 3) .and. (.not. good)) then
          itest(44) = 100
          goto 30
        endif
      endif
      if((itest(44) .eq. 2) .and. (inst .ne. 9)) then
c since (44) = 2, rerun
        itest(44) = 200
        goto 30
      endif
50    continue
      time1=time2
      if(itest(38) .eq. 1) then
        itest(38) = 11
        knst = 0
        goto 30
      else if(itest(38) .eq. 11) then
        itest(38) = 1
      endif
      if(ipro .ne. 'more') goto 20
c
c read next instruction record
96    read(inpt, '(a)') icard
      phcard(lph) = icard
      if(isa.eq.1) write(1,'(a)') icard
      read(icard, 100)
     *     ipro, ichec,  knst, inst, zres,     ala1, 
     *    isnla,  ala2,    alo1, isnlo,  alo2,      org1, org2
100   format(a4,    a4, 9x,i1,   i1, f5.2, 16x,f2.0,  
     *       a1,  f5.2, 5x,f3.0,    a1,  f5.2, 11x, f2.0, f5.2)
      instset = ' '
      malaz = icard(21:24)
      malla = icard(43:46)
      malor = icard(74:77)
      if(icard(9:9) .ne. ' ') evstat = icard(9:9)
      if(icard(10:10) .ne. ' ') evtype = icard(10:10)
      write(punt,110) ipro, ichec, evstat, evtype,   knst,   inst,
     *     zres,     ala1, isnla, ala2,     alo1, isnlo, alo2, 
     *     org1, org2

110   format(' ipro check evstat evtype knst inst depth   ',
     *' latitude   longitude, origin time', /,
     * 1x,             a4,     a4, 5x,a1, 6x,  a1, 5x, i1, 4x, i1, 
     * 2x, f6.2, 2x, f4.0,    a1, f5.2, 1x, f5.0,    a1, f5.2,
     * 3x, f3.0, f5.2, /, 25x, '--- run again ---  ')

      ipro = dnstrg(ipro)
      ichec = dnstrg(ipro)
      if(itest(38) .eq. 2) knst = 1
      if(itest(38) .eq. 3) knst = 0
c calculate first trial hypocenter
      if(malla .ne. '    ') then
        la = ala1 + .0001
        lo = alo1 + .0001
        call fold2(savla,savlo,la,isnla,ala2,lo,isnlo,alo2)
      endif
      if(malaz .ne. '    ') savez = zres + test(8)
c define starting origin time
      savor = 0.
      freor = .true.
      if(malor .ne. '    ') then
        savor = 60.*(org1 - lbastm) + org2
        freor = .false.
      else if(inst .eq. 8) then
        inst = 0
      endif
c convert inst=7 to inst=9 with origin free - this is
c the option to use for quary blasts with unknown origin time.
      if(inst .eq. 7) then
        inst = 9
        freor = .true.
      endif
      if((dnstrg(evtype) .eq. 't') .or. 
     *   (dnstrg(evtype) .eq. 'n') .or. 
     *   (dnstrg(evtype) .eq. 'r')) then
c       teleseism, nuclear or regional
        call velazm
        iexit = 1
        if((ipun .eq. 2) .or. (ipun .eq. 3)) call npunch('final')
        if(ipro .eq. 'more') goto 96
        goto 20
      endif
      needsum = .false.
      if((ipro .eq. '    ') .or. (ipro .eq. 'more')) goto 30
160   write(punt,170) ipro, ichec
170   format(/' xxxerrorxxx ', 2a4, ' skipped.  must be blank or more,')
      goto 20
c
c after all earthquakes are processed (eoff is true)
180   if(sockets) then
        return
      endif
      call lissum(2)
c     show elapsed time
      call timit(1)
      write(punt, *) 'irelo = ', irelo, ' nreloc = ', nreloc
      if(irelo .gt. 0) then
c relocate events with new station delays irelo times
        nreloc = nreloc + 1
        if(nreloc .gt. irelo) stop
c rewind input(inpt), archive(11), summary(4), output(9)
        write(punt, '(a)') ' rewind files and rerun events'
        if(inpt .eq. 5) then
          write(punt, *) 
     *     ' input is standard input, which can not be rewound'
          write(punt, *) 
     *     ' therefore, the relocate option may not be used'
          stop 'abort from locate'
        endif
        rewind (inpt)
        rewind (11)
        rewind (4)
        rewind (9)
c       reset summary
        call lissum(1)
        eoff = .false.
        time1 = 0.0d0
        call input1
        goto 20
      endif
      return
      end
c end locate
