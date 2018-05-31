c input1.for    []
      subroutine input1
c input station list, crustal model, test and control values
c     to change max number of stations from nsn to some other number,
c       151 for example:
c                1)  change all occurrences of nsn) to 151)
c                2)  change nmax from nsn to 151 in next data statement.
c                3)  note:  be sure the new dimension is a unique number
c                      which has not been previously used so that
c                      subsequent changes in dimension will not be
c                      difficult to make.
c --- added inmain in place of the constant 8.  its now in the /dhip/
c --- common block and also passed to getsta().
      include 'params.inc' 
      parameter (ndly = 11)
      real lat,lon,latr,lonr
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      character*4 iscan
      character*4 iahead*60, msta*5, nsta*5, icard*110, dnstrg*110
      character*50 uacal, uaini, dlyfil, icardup*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /dmost/ ipun,ivlr,blank
      common /dhil/ iq,ilat,kms,icat
      common /dhip/ inpt,isa,ilis,inmain,injump
      common /dbiln/ ioldq
      character*1 iclass
      common /dbio/ iclass(0:4)
      common /dhin/ iglob, zup, zdn
      common /dhio/ nedit,good
      common /dio/ rmsmx,presmx,sresmx,noutmx,nimx,iprun,semx
      character*1 bksrc
      common /dipu/ ipkdly, igsum, bksrc
      logical medmag
      common /dinx/ imag,imagsv,medmag
      common /dit/ tid(lmax,lmmax),did(lmax,lmmax),lowv,modin(mmax+3)
      common /dix/ iexcal, uacal
      common /igl/ nglobalzs, globalzs(20)
      common /imost/ test(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /idt/ v(lmax2)
      integer fmwt, xmwt
      common /ioxl/ fmwt(nsn),xmwt(nsn)
      common /logfil/ logfil
      common /ihfgpq/ itest(100)
      character*1 iqcls
      common /il1/ iqcls
      common /ilpu/ sw(nsn),ndate(nsn),nhr(nsn),mno(nsn),ielv(nsn)
      common /ilmpu/ ns
      common /iclmpq/ lat(nsn),lon(nsn)
      common /ilpx/ calr(5, nsn),fmgc(nsn),xmgc(nsn)
      common /in/ irmo,iryr,odmag
      character*1  magke
      common /in1/ magke
      common /idno/ ksel,ksort
      common /iot/ flt(2,nsn),thks(nsn)
      common /iox/ prr(nsn),iuses
      common /iiq/ wf(51)
      logical fsteq
      character*1 revp, exdly
      common /ip/ latr,lonr,tpdly(2,nsn),infil,iofil,indexs,
     *  iscod(nsn),ipcod(nsn), fsteq, exdly(4, nsn), revp(6, nsn)
      common /ip1/ rsew(4)
      common /int/ thk(lmax+1),lbeg(mmax),lend(mmax),vi(lmax),vsq(lmax),
     *  vsqd(lmmax,lmax),f(lmax,lmmax),kl,ivway,sthk(mmax),sthk1(mmax),
     *  sdly(ndly,nsn)
      common /ilt/ vthk(2,nsn),ipdly(nsn),mod(nsn),ipthk(nsn)
      common /ilv/ c(nsn),e(nsn)
      common /it/ vpvs(lmax2),vpvsm(mmax+3),nttab
      character*4 mtyp*2
      common /iu/ mtyp(mmax)
      common /ix/      ir,qspa(9,40)
      common /lvz/ jref(lmax)
      common /reloc/ irelo, nreloc
      common /rioq/ been,damp,dmpinc,igo
      character*4 inote(100)
      dimension atest(100),d(lmax2)
c initialize test variables used throughout the program
      character*1 ins, iew
      integer iostat
      data d/lmax2*0./
      data atest/1.78, 5.0, 0.0, 0.0, -99.0, 0., 10., 0., 0., 50.,
     * 50.0,100.0 ,50.0,50.0, 10.0,50.0, 2.0, 50.0, 0.05,0.05,
     * 9.0, 35., 0.7, 35., 40., 0.0025, 20.0, 0., -.1, 0.0, 
     * -1.15, 2.00, 0.0, 0.0, 0.001, 100.0, 3.0, 0.0, 1.0, 0.007, 
     * 0.0, 75.0, 2*0., 0.1379, 0.0, 0.0, 6.5, 2*0., 1000., 
     * 2800., 1.0, 200., 19., 45*0.0/
      data inote/100*'    '/
      data uaini/' '/, kl/0/
      data nglobalzs/0/, globalzs/20*0.0/
      fsteq = .true.
200   continue
      nkrst = 0
      been = 0.0
205   read(inpt, '(a)', iostat = iostat) icardup
      icard = dnstrg(icardup)
      if (iostat.eq.-1) then
        if (ilis.gt.0) then
          write(punt, 220) inpt
          write(logfil, 220) inpt
220       format(' subroutine input1 found end of file on unit ', i5)
        endif
        if(inpt .eq. inmain) then
          write(logfil, 225)
          write(punt, 225)
225       format(' xxxerrorxxx abnormal termination while ',
     *    'reading control file.')
          stop 'abort from input1'
        else
c         return to reading from unit inmain
          inpt = inmain
          goto 205
        endif
      endif
      iscan = icard(1:4)
      if (icard .eq. ' ') goto 205
      if (iscan(1:4) .eq. 'stop') then
        write(punt, '(a)') ' stop processing with stop instruction'
        stop 'normal termination'
      endif
      if (iscan(1:2) .eq. 'c*') then
        write(punt, 235) icard
235     format(1x, a)
        goto 205
      endif
      if (iscan .eq. 'head') then
        iahead = icard(19:lentru(icard))
        if (ilis.gt.0) write(punt, '(//35x,a)') iahead
        goto 205
      endif
c
c remove everything on record beginning with an ! mark
      do 2515 i = 1, 110
        if(icard(i:i) .eq. '!') then
          if(i .eq. 1) goto 205
          icard = icard(1:i-1)
          goto 2517
        endif
2515  continue
c
2517  if (iscan .eq. 'arc ') then
        if(ilis .gt. 0)
     *  write(punt, '(a, a)')
     *    ' open archive file named:  ', icardup(19:78)
        close(11)
        call openfl(   11, icardup(19:78), 'unknown', 'null', 
     *  'none', 'noprint', 0)
        goto 205
      endif
c
      if (iscan .eq. 'uofa') then
        uacal = icardup(19:78)
        goto 205
      endif
      if (iscan .eq. 'jump') then
        if (inpt .eq. inmain) then
          close (unit = injump,iostat = iostat)
          if (ilis.gt.0) write(punt, '(1x, a)') icardup
          call openfl(injump, icardup(6:55), 'old', 'zero', 'readonly',
     *    'none', 0)
          inpt = injump
        else
          write(logfil, 260) inmain
          write(punt, 260) inmain
260       format(' xxxwarningxxx can not nest jumps from control file,'
     *    ,/, ' returning to main input file (unit ',i2,')')
          inpt = inmain
        endif
        goto 205
      endif
c because standard fortran 77 to can not read an internal file with
c free format, file 14 must be used in the following code!
      rewind 14
      write(14, '(a)') icard(19:80)
      rewind 14
      if (iscan .eq. 'rese') then
c       read(icard(19:80), *, err = 5243) j, ax
        read(14, *, iostat = iostat) j, ax
        if (iostat.ne.0) then
          ierror = 1
          goto 5243
        endif
        if (( j .gt. 100) .or. ( j .lt. 1 )) then
          ierror = 1
          goto 530
        endif
        inote(j) = '****'
        test(j) = ax
        goto 205
      endif
c
      if ((iscan .eq. 'crus') .or. (iscan .eq. 'velo')) then
c       read(icard(19:80), *, err = 5243)  ax, bx, cx
        read(14, *, iostat = iostat)  ax, bx, cx
        if (iostat.ne.0) then
          ierror = 2
          goto 5243
        endif
        if (nkrst .eq. 0) then
          lh = lmax + 2
          do  290 k = 1,lh
            d(k) = 0.0
            v(k) = -1.
290       continue
          do 295 k = 1, mmax
            mtyp(k) = 'ps'
295       continue
        endif
        nkrst = nkrst + 1
        if (nkrst .gt. lmax) then
          write(logfil, 310) icard
          write(punt, 310) icard(1:30)
310       format(' too many velocity layers. ',
     *    ' layer ', a, ' was not used.')
        else
          v(nkrst) = ax
          d(nkrst) = bx
          vpvs(nkrst) = cx
        endif
        goto 205
      endif
c
      if (iscan .eq. 'weig') then
c       read(icard(19:80), *, err = 5243) ax, bx, cx
        read(14, *, iostat = iostat) ax, bx, cx
        if (iostat.ne.0) then
          ierror = 3
          goto 5243
        endif
        if ((ax .lt. 1.).or.(bx .lt. 1.).or.(cx .lt. 1.)) then
          if (ilis.gt.0) write(punt, 320) icard
320       format(' weight option parameters represent the ratio of', /,
     *    ' the standard error of weight code 1, 2, and 3 ', /,
     *    ' readings to the standard error of 0 weight code', /,
     *    ' readings.  none can be less than 1. so stop.', /,
     *    1x, a)
          stop 'abort from input1.'
        endif
        rsew(2) = ax
        rsew(3) = bx
        rsew(4) = cx
        goto 205
      endif
c
      if (iscan .eq. 'vari') then
c       read(icard(19:80), *, err = 5243) ivlr, ivway, lowv
        read(14, *, iostat = iostat) ivlr, ivway, lowv
        if (iostat.ne.0) then
          ierror = 4
          goto 5243
        endif
        goto 205
      endif
c
      if (iscan .eq. 'miss') then
c       read(icard(19:80), *, err = 5243) kms
        read(14, *, iostat = iostat) kms
        if (iostat.ne.0) then
          ierror = 5
          goto 5243
        endif
        if(ilis .gt. 0) then
          if (kms .eq. 0) write(punt,335)
335       format(' scan for missing stations.  code = 0')
          if (kms .ne. 0) write(punt,340) kms
340       format(' do not scan for missing stations.  code = ', i3)
        endif
        goto 205
      endif
c
      if (iscan .eq. 'prin') then
c       read(icard(19:80), *, err = 5243) iprn
        read(14, *, iostat = iostat) iprn
        if (iostat.ne.0) then
          ierror = 6
          goto 5243
        endif
        if ((iprn .lt. -2) .or. (iprn .gt. 5)) then
          ierror = 6
          goto 530
        endif
        goto 205
      endif
c
      if ((iscan .eq. 'punc') .or. (iscan .eq. 'summ')) then
c       read(icard(19:80), *, err = 5243) ipun
        read(14, *, iostat = iostat) ipun
        if (iostat.ne.0) then
          ierror = 7
          goto 5243
        endif
        if ((ipun .lt. -4) .or. (ipun .gt. 4)) then
          ierror = 7
          goto 530
        endif
        goto 205
      endif
c
      if (iscan .eq. 'magn') then
c       read(icard(19:80), *, err = 5243) j
        read(14, *, iostat = iostat) j
        if (iostat.ne.0) then
          ierror = 8
          goto 5243
        endif
        iabj = iabs(j)
        if (((iabj .gt. 4) .and. (iabj .lt. 10)) .or.
     *       (iabj .gt. 14)) then
          ierror = 8
          goto 530
        endif
        imagsv = j
        imag = iabj
        medmag = .false.
        if(imag .gt. 9) then
          medmag = .true.
	  imag = imag - 10
        endif
        iuses = 0
        if (j .lt. 0) iuses = 1
        goto 205
      endif
c
      if ((iscan .eq. 'qual') .or. (iscan .eq. 'tabu')) then
c       read(icard(19:80), *, err = 5243) j
        read(14, *, iostat = iostat) j
        if (iostat.ne.0) then
          ierror = 9
          goto 5243
        endif
        if ((j.lt. -4 ).or.(j.gt. 4)) then
          ierror = 9
          goto 530
        endif
        ioldq = 0
        if (j .lt. 0) ioldq = 1
        iq = iabs(j)
        iqcls = iclass(iq)
        goto 205
      endif
c
365   if (iscan .eq. 'sort') then
c       read(icard(19:80), *, err = 5243) ksort
        read(14, *, iostat = iostat) ksort
        if (iostat.ne.0) then
          ierror = 10
          goto 5243
        endif
        if(ilis .gt. 0) then
          if (ksort .eq. 0) write(punt,370)
370       format(' sort stations by distance.  code = 0' )
          if (ksort .ne. 0) write(punt,375) ksort
375       format(' do not sort stations by distance.  code = ', i3)
        endif
        goto 205
      endif
c
      if (iscan .eq. 'stan') then
        write(punt,395)
395     format(46h change all program options to standard values )
        do 400 i = 1, 100
          inote(i) = '    '
          test(i) = 1.23456
400     continue
        do 405 i = mmax+1, mmax+3
          modin(i) = 0
405     continue
        bksrc = ' '
        iexcal = 0
        ns = 0
        rsew(2) = 5.
        rsew(3) = 10.
        rsew(4) = 20.
        igsum = 1
        ivlr = 0
        lowv = 1
        kms = 1
        ir = 0
        iprn = 1
        ilis = 1
        ipun = 0
        iuses = 0
        imagsv = 0
	medmag = .false.
        imag = 0
        iq = 2
        iqcls = iclass(2)
        ioldq = 0
        ksort = 0
        inpt = inmain
        icat = 1
        nedit = 0
        iprun = 1
        ipkdly = 1
        irelo = 0
        iglob = 1
        uacal = uaini
        nglobalzs = 0
	do i = 1, 20
	  globalzs(i) = 0.0
	enddo
        goto 205
      endif
      if (iscan .eq. 'blan') then
c source code to use if phase record has blank field
        do 412 i = 19, 80
          if(icard(i:i) .ne. ' ') goto 413
412     continue
413     bksrc = dnstrg(icard(i:i))
        if (ilis .gt. 0) write(punt, 414) bksrc
414     format(' arrival-time record blank source fields will be',
     *  ' assumed to be source "', a, '"')
        goto 205
      endif
c
      if (iscan .eq. 'comp') then
c       read(icard(19:80), *, err = 5243) ksel
        read(14, *, iostat = iostat) ksel
        if (iostat.ne.0) then
          ierror = 12
          goto 5243
        endif
        if (ilis .gt. 0) then
          if (ksel .ne. 0) write(punt,415)
415       format(42h do not compress printed output.  code = 1)
          if (ksel .eq. 0) write(punt,420) ksel
420       format(33h compress printed output.  code =,i3)
        endif
        goto 205
      endif
c
425   if (iscan .eq. 'igno') then
c       read(icard(19:80), *, err = 5243) igsum
        read(14, *, iostat = iostat) igsum
        if (iostat.ne.0) then
          ierror = 13
          goto 5243
        endif
        if (igsum .eq. 0) then
          if (ilis.gt.0) write(punt, 430) igsum
430       format(/,' ignore sum code = ', i5, /,
     *    ' (ignore starting locations on summary records)')
        else
          if (ilis.gt.0) write(punt, 435) igsum
435       format(/,' ignore sum code = ', i5, /,
     *    ' (use starting locations from summary records)')
        endif
        goto 205
      endif
c
      if (iscan .eq. 'cali') then
        if (ilis.gt.0) write(punt,445)
445     format(/' input extra calibration curves.')
c       read(icard(19:80), *, err = 5243) ir
        read(14, *, iostat = iostat) iexcal
        if (iostat.ne.0) then
          ierror = 14
          goto 5243
        endif
        if (iexcal .lt. 1 .or. iexcal .gt. 9) then
          write(punt, '(a, a, /, a)') ' error in: ', icard,
     *    ' the number of extra calibrations must be between 1 and 9'
          write(logfil, '(a, a, /, a)') ' error in: ', icard,
     *    ' the number of extra calibrations must be between 1 and 9'
          stop 'abort from input1'
        else
          do 460 i = 1,iexcal
            read(inpt,450) (qspa(i,k),k = 1,40)
450         format(20f4.2)
            ip8 = i + 8
            if (ilis.gt.0) write(punt,455) ip8,(qspa(i,k),k = 1,40)
455         format(/,' qspa(', i2, ') ', 20f5.2, /, 10x, 20f5.2)
460       continue
        endif
        goto 205
      endif
c
465   if (iscan .eq. 'begi') goto 540
      if (iscan .eq. 'arri') then
        if(inpt .eq. inmain) then
          goto 580
        else
          write(logfil, 470) inmain
          write(punt, 470) inmain
470       format(' xxxerrorxxx input data structure error,' /,
     *    ' the arrival times next record must be in main', /,
     *    ' input stream (unit ',i2,').')
          stop 'abort from input1'
        endif
      endif
c
      if (iscan .eq. 'debu') then
c       read(icard(19:80), *, err = 5243) nedit
        read(14, *, iostat = iostat) nedit
        if (iostat.ne.0) then
          ierror = 15
          goto 5243
        endif
        if (nedit .eq. 0) then
          if (ilis .gt. 0) write(punt,480) nedit
480       format(33h do not use debug option.  code =,i3)
          goto 205
        endif
        read(inpt,485) icard
485     format(a)
        if (ilis .gt. 0) write(punt,490) icard
490     format(' debug option record is',/1x,a)
        read(icard, 495) rmsmx,presmx,sresmx,noutmx,nimx,semx
495     format(5x,3(5x,f5.2),2(5x,i5),5x,f5.0)
        goto 205
      endif
      if (iscan .eq. 'cal ') then
        if (ilis.gt.0) write(punt, 496)
496     format(' xxxx warning xxxx  calibration option no longer valid')
        goto 205
      endif
      if (iscan .eq. 'resi') then
c       read(icard(19:80), *, err = 5243) iprun
        read(14, *, iostat = iostat) iprun
        if (iostat.ne.0) then
          ierror = 16
          goto 5243
        endif
        goto 205
      endif
      if (iscan .eq. 'sele') then
c       read(icard(19:80), *, err = 5243) ipkdly
        read(14, *, iostat = iostat) ipkdly
        if (iostat .ne. 0) then
          ierror = 17
          goto 5243
        endif
        if(ipkdly .le. 0) then
          if (ilis.gt.0) write(punt,515)
515       format(' select delays based on sub. usedly regions.')
          if(ipkdly .lt. 0) then
            read(inpt, '(a)') dlyfil
c           call openfl(iunit,  ifile, istat,  izero, ishr,
c    *      iform, irecl)
            call openfl(   17, dlyfil, 'old', 'null', 'none',
     *      'none', 0)
            if (ilis.gt.0) write(punt, 5155) dlyfil
5155        format(' delay cylinders defined in the file:', /, 1x, a)
          endif
        else
          write(punt,516)
516       format(' select delays based on closest station.')
        endif
        goto 205
      endif
      if (iscan .eq. 'relo') then
c       read(icard(19:80), *, err = 5243) irelo
        read(14, *, iostat = iostat) irelo
        if (iostat.ne.0) then
          ierror = 18
          goto 5243
        endif
        if (ilis.gt.0) write(punt,522) irelo
522     format(' relocate events ', i2, ' times, each time', /,
     *  ' updating the station delays for delay model 1.')
        goto 205
      endif
      if (iscan .eq. 'newg') then
	read(14, *, iostat = iostat) (globalzs(i), i = 1, 20)
	do i = 20, 1, -1
	  if(globalzs(i) .ne. 0.0) then
	    nglobalzs = i
	    goto 5225
	  endif
	enddo
	write(punt, *) 
     *    ' new global option record included with no non-zero depths'
	write(logfil, *) 
     *    ' new global option record included with no non-zero depths'
	goto 205
5225    write(punt, 5226) nglobalzs, (globalzs(i), i = 1, nglobalzs)
        write(logfil, 5226) nglobalzs, (globalzs(i), i = 1, nglobalzs)
5226	format(' running new global option with ', i3, 
     *    ' starting depths of ', /, 2x, 10f10.2)
	goto 205
      endif
      if (iscan .eq. 'glob') then
c       read(icard(19:80), *, err = 5243) iglob
        read(14, *, iostat = iostat) iglob
        if (iostat.ne.0) then
          ierror = 19
          goto 5243
        endif
        if (ilis .gt. 0) then
          if (iglob .eq. 0) then
            write(punt, 5241)
            write(logfil, 5241)
5241        format(' global minimum search turned on')
          else
            write(punt, 5242)
            write(logfil, 5242)
5242        format(' global minimum search turned off')
          endif
        endif
        goto 205
      endif
      if (iscan .eq. 'cons') then
        read(14, *, iostat = iostat) j
        if (iostat .ne. 0) then
          ierror = 21
          goto 5243
        endif
        ilis = j
        write(logfil, '(a, i5)') ' constants print = ', ilis
        goto 205
      endif
      if (iscan .eq. 'dela') then
c       read an additional delay model
        read(14, *, iostat = iostat) nmodel
        if (iostat .ne. 0) then
          ierror = 22
          goto 5243
        endif
        call adddly(nmodel)
        goto 205
      endif
5243  write(punt,525)  icard, ierror
      write(logfil,525)  icard, ierror
525   format(' xxxerrorxxx control record in improper format.',
     * /,1x, a,/' so stop (error #', i3, ')')
      stop 'abort from input1'
530   write(logfil,535) ierror, icard
      write(punt,535) ierror, icard
535   format(
     * ' this record skipped because j is improper value (error #',
     * i3,'). ', /, 1x, a)
      goto 205
c input station list
540   if (ilis.gt.0) write(punt,550) iahead
550   format(' list of stations available for these solutions',
     *   /,35x,a)
560   continue
c     read(icard(19:80), *, err = 5243) indexs, ibate
      read(14, *, iostat = iostat) indexs, ibate
      if(ibate .lt. 19000000) then
	write(logfil, *) 'xxxerrorxxx century not included on ',
     *    'begin station list record.  date = ', ibate
	write(punt, *) 'xxxerrorxxx century not included on ',
     *    'begin station list record.  date = ', ibate
	stop
      endif
      if (iostat.ne.0) then
        ierror = 20
        goto 5243
      endif
      write(punt, '(1x, a)') icard(1:80)
      ihrmn = 0
c indexs = 0 or 1 -> stations list (index option has been removed)
c negative indexs -> no print
      if (indexs .eq. 0) indexs = 1
      if (ilis.gt.0) then
        if (iabs(indexs) .eq. 1) write(punt,565) indexs
565     format(' station list code =',i5)
        if (iabs(indexs) .ne. 1) then
          write(punt,'(a, i5)')
     *    ' xxxerrorxxx station list code must be 1, not ', indexs
          write(logfil,'(a)')
     *    ' xxxerrorxxx station list code must be 1, not ', indexs
          stop 'abort from input1'
        endif
        write(punt,575) ibate
575     format(' set up for events starting on ',i8,/,
     * ' name latitude  longitude  elev p thickness p p  pdy1 sdy1',
     * '  pdy2 sdy2  pdy3 sdy3  pdy4 sdy4  pdy5 sdy5     calr xmgc',
     * ' mgwt fmgc wt ',/,
     * ' * continuation  record *      thk 1   2  mod dly          ',
     * '                                            sys          ',
     * '           ps',/,
     * '  polarity stawt teldy code altdy cnyrmody hr')
      endif
      rewind 2
      l = 1
      if (test(2) .eq. 1.23456) test(2) = atest(2)
      if (test(53) .eq. 1.23456) test(53) = atest(53)
c     read station list
c     write(logfil, '(a)') ' just about to call getsta - unit logfil'
      call getsta(   indexs, nsta, ielv, mod,
     *         ipthk, vthk, ipdly, dly, sdly, lat, lon, c, e, sw,
     *         klas, calr, xmgc, fmwt, xmwt, fmgc, ipcod, iscod, tpdly,
     *         revp, ndate, nhr, ns,
     *         prr, inpt, nreloc, inmain, injump, exdly, ibate,
     *         test(53))
      infil = 3
      iofil = 2
      goto 205
c----
580   continue
c write test variables
      if (ilis.gt.0) write(punt,595) iahead
595   format(1h ,35x,a)
      call jdate(irmo, irdy, iryr, ihr, imn, isec)
      iseed = irmo*irdy*iryr + imn + isec
      dum = ran3(-iseed)
      do 600 i = 1, 55
        if (test(i).eq.1.23456) test(i) = atest(i)
        itest(i) = test(i) + sign(0.0005,test(i))
600   continue
      if(test(34) .ne. 0.0) test(34) = 1.0
      itest(38) = abs(test(38)) +0.0005
      if ((iglob .eq. 0) .and. (test(47) .ne. 0.0)) then
        write(punt, 590)
        write(logfil, 590)
590     format(' xxxerrorxxx the global option is not compatible with',/
     *         '   the option to fix the hypocenter on a plane. ',/
     *         '   either set test(47) to 0.0 or turn off the'
     *         ' global option')
        stop 'abort from input1'
      endif
c define jeffreys weighting funct.
      if (iprn .ge. 3) write(punt, 605) test(20)
605   format(/,47h jeffreys weighting funct         i      weight,
     *  7h   mu =,f10.3)
      do 615 i = 1,51
        wf(i) = (1.0 + test(20))/(1.0+test(20)*exp(((i-1)*0.1)**2/2.0))
        if (iprn .ge. 3) then
          write(punt,610) i,wf(i)
610       format(30x,i5,f12.3)
        endif
615   continue
      if (iprn .ge. 3) write(punt,595)
      if (ilis.gt.0) then
        write(punt, 620)  ( i,atest(i), test(i), inote(i), i =  1, 10)
        write(punt, 625)  ( i,atest(i), test(i), inote(i), i = 11, 20)
        write(punt, 630)  ( i,atest(i), test(i), inote(i), i = 21, 30)
        write(punt, 635)  ( i,atest(i), test(i), inote(i), i = 31, 40)
        write(punt, 640)  ( i,atest(i), test(i), inote(i), i = 41, 50)
        write(punt, 642)  ( i,atest(i), test(i), inote(i), i = 51, 55)
      endif
620   format(/7x, 'test variables', 14x, 'description', /, 5x,
     *'standard  reset to',
     */i3,2f10.4,a4,'ratio of p-wave velocity to s-wave velocity.',
     */i3,2f10.4,a4,'lt 0 no elev cor/ =0 use 1st vel/ gt 0 use this.',
     */i3,2f10.4,a4,'first trial latitude in degrees.',
     */i3,2f10.4,a4,'first trial longitude in degrees.',
     */i3,2f10.4,a4,'first trial depth in kilometers, unless = -99.',
     */i3,2f10.4,a4,'sphere rad for aux rms values. if neg cont iterat',
     *'ion at most neg point.',
     */i3,2f10.4,a4,'minimum number of first motions required to plot.',
     */i3,2f10.4,a4,'elevation of top of layered models (km).',
     */i3,2f10.4,a4,'if 0 allow neg depths in summary and archive ',
     * 'files.',
     */i3,2f10.4,a4,'apply distance weighting on this iteration.')
625   format(
     *i3,2f10.4,a4,'xnear = greatest distance with weight of 1.0',
     */i3,2f10.4,a4,'xfar = least distnace with weight of 0.0',
     */i3,2f10.4,a4,'apply azimuthal weighting on this iteration.',
     */i3,2f10.4,a4,'weight out large residuals on this iteration.',
     */i3,2f10.4,a4,'give zero weight to residuals gt this.',
     */i3,2f10.4,a4,'apply boxcar weighting on this iteration.',
     */i3,2f10.4,a4,'give zero weight to residuals gt this*stand. dev.',
     */i3,2f10.4,a4,'begin jeffreys weighting on this iteration.',
     */i3,2f10.4,a4,'use jeffreys weighting only if rms gt this.',
     */i3,2f10.4,a4,'mu of jeffreys weighting funct.')
630   format(
     * i3,2f10.4,a4,'maximum number of iterations.',
     */i3,2f10.4,a4,'limit change in focal depth to this amount (km).',
     */i3,2f10.4,a4,'if delz would make z neg, set delz = -this*z ',
     *'(km).',
     */i3,2f10.4,a4,'limit change in epicenter to this. (km).',
     */i3,2f10.4,a4,'fix depth if epicentral change gt this. (km).',
     */i3,2f10.4,a4,'stop iterating if square of adjustment lt this.',
     */i3,2f10.4,a4,'global opt: if deep solution z > this, continue',
     *' with z 1/2 way to surface.',
     */i3,2f10.4,a4,'for fixed hypo on plane, set = plunge azimuth.',
     *'  if neg. continue as free sol.',
     */i3,2f10.4,a4,'set std err of res=+this if degrees of freedom =',
     *'0 or =-this if this lt 0.'
     */i3,2f10.4,a4,'dip of plunge vector for epi. fixed on plane. ',
     *' see test(28) & (47) also.')
635   format(
     * i3,2f10.4,a4,'duration magnitude c1, constant.',
     */i3,2f10.4,a4,'duration magnitude c2, *log((f - p)*fmgc).',
     */i3,2f10.4,a4,'duration magnitude c3, *delta.',
     */i3,2f10.4,a4,'if not 0, scale the normal equations.',
     */i3,2f10.4,a4,'minimum damping of normal equations.  ',
     */i3,2f10.4,a4,'maximum first trial depth if computed from p-',
     *'arrival times.',
     */i3,2f10.4,a4,'if termination occurs before this iteration, set ',
     *'iteration number to this and continue.',
     */i3,2f10.4,a4,'if this =1, run all with and then without s/ =2,',
     *'run with s/ =3, run without s/ =4, fix hypo', /,
     * 27x, '   / neg, use s to fix origin.',
     */i3,2f10.4,a4,'multiply the s and s-p weights by this factor.',
     */i3,2f10.4,a4,'duration magnitude c4, *depth.')
640   format(
     * i3,2f10.4,a4,'if this =1, print opt. ge 1, & summary ',
     *'opt. =+ or -1, then write sum. record each itteration.',
     */i3,2f10.4,a4,'global opt: deep starting z wrt top of model.',
     */i3,2f10.4,a4,'duration magnitude c5, *(log((f - p)*fmgc)**2).',
     */i3,2f10.4,a4,'if =1 rerun debug eqs with critical sta/ =2 ',
     *'continue iter with crit sta.',
     */i3,2f10.4,a4,'x scale factor for focal mechanism plot.',
     */i3,2f10.4,a4,'xfar set ge dist of test(46)th station + 10.  if ',
     *'lt 0 then fill gap.',
     */i3,2f10.4,a4,'weight for fix on plane.  see test(28) and (30).',
     */i3,2f10.4,a4,'half-space velocity for first trial location.',
     */i3,2f10.4,a4,'if .ne. 0 calculate vp/vs ratio; if abs val >1 ma',
     *'ke wadati plot; if neg, use wadati origin in solution.',
     */i3,2f10.4,a4,'for exploring rms space, compute this number of',
     *' fixed depth solutions (up to 22).')
642   format(
     */i3,2f10.4,a4,'for epicentral distance beyond this, use first',
     *' travel-time table.',
     */i3,2f10.4,a4,'Wood Anderson static magnification assumed for',
     *' local magnitude determination.',
     */i3,2f10.4,a4,'if .eq. 1 stations with 4-letter codes ending'
     *' e or n treated as horizontals.',
     */i3,2f10.4,a4,'if 1st computed trial location > this (km) from'
     *' closest station, start at closest station.',
     */i3,2f10.4,a4,'assumed century for events without summary'
     *' record.')

      j = iq
      if (ioldq .eq. 1) j = -iq
      if (ilis.gt.0) then
        write(punt, 645) rsew
645     format(/,
     *  ' weight option - relative standard errors for code:   0'
     *, '      1      2      3', /,
     *         '                                                   ',
     *    4f7.3, /)
        write(punt,650) iprn,ipun,imagsv,j
650     format(' printer option ', i4, 2x, ' summary option  ', i4, 5x,
     * ' magnitude option ', i4, 5x, ' tabulation option ',i2,//
     * ' no event output  -2', /,
     * ' one line/eq      -1', /,
     * ' final solution    0   no sum records     0      use xmag',
     * 12x, '0      no summary        0', /
     * ' one line per iter 1   summary records    1      use fmag',
     * 12x, '1      a                 1', /
     * ' sta res each iter 2   sum + archive file 2      use (xmag+fm'
     *,'ag)/2   2      a + b             2', /
     * 70h regres each iter  3   archive file       3      prefer fmag /
     *xmag   3,
     * 6x, 19ha,b + c           3,/
     * '                       "corrected" input  4  ',
     * '    prefer xmag /fmag   4      a,b,c + d         4',
     * /, 23x, '                          if neg use fms not fmp',
     *  5x, 'positive/q from std errors', /
     * 45x, 31x, 'negative/q from sol+sta')
        if(uacal .ne. ' ') write(punt, 654) uacal
654     format(' u of a cal data file:  ', a)
      endif
      if (nedit .ne. 0 .and. ilis .gt. 0)
     * write(punt,655) nedit,rmsmx,presmx,sresmx,
     *                              noutmx,nimx,semx
655   format(' debug code = ',i1,'.    debug events have:',
     * /,'                                           rms .gt. ',f5.2,
     * /,'                                 or a p res is .gt. ',f5.2,
     * /,'                                or an s res is .gt. ',f5.2,
     * /,'        or the no. of readings weighted out is .gt. ',i5,
     * /,'                or the number of iterations is .gt. ',i5,
     * /,'  or the max single variable standard error is .gt. ',f5.0,
     * /,'                     or the change in depth is .lt. ',f5.2)
c----
      if (nkrst .eq. 0) then
        if (kl .eq. 0) then
          write(punt, '(a,/,a)') 
     *    ' xxx error xxx:  ',
     *    ' at least one layered crustal model must be specified.'
          stop 'abort from input1'
        endif
        goto 790
      endif
c scan and print velocity structure data read in above
      kl = 1
      lbeg(1) = 1
      if (test(1) .le. 0.0) then
        write(logfil, 660) test(1)
        write(punt, 660) test(1)
660     format(' xxx error xxx', /,
     *  ' test(1) is the vp/vs ratio and may not equal ', f10.2)
        stop 'abort from input1'
      endif
      if (ivlr.ne.0.and.ilis.gt.0) write(punt,665) ivlr,ivway
665   format(54h variable layer option is used.  layer to be varied is
     *,7h layer ,i3,9h    vmod=,i3)
      if (lowv .eq. 0.and.ilis.gt.0) write(punt,670)
670   format(47h do not make compensating change in layer below,
     * 16h variable layer.)
      if (lowv .eq. 1.and.ilis.gt.0) write(punt,671)
671   format(' make compensating change in layer below',
     * ' variable layer.')
      if (iprun .ne. 1.and.ilis.gt.0) write(punt,680)
680   format(' residual option is in effect.  for residuals between + '
     *  ,49hand - 2.25 only the absoulte value rounded to the,
     * /,41h      nearest 0.5 second will be printed.)
      if (ilis.gt.0) write(punt,685) kl
685   format(//7x,14hvelocity model ,i3
     */29h   layer  velocity     depth ,
     * 21h  thickness    vpvs  /11x,25hkm/sec       km        km/)
      lp1 = lmax + 1
      do 690 l = 1, nkrst
690   if (vpvs(l) .eq. 0.0) vpvs(l) = test(1)
      do 785 l = 1,lp1
        thk(l) = d(l+1)-d(l)
        if (thk(l).lt.0.0) thk(l) = 1000.000
        if ( (d(l) .ne. 0.) .or. (l .eq. 1) ) goto 770
c found depth of zero, so begin new model
        lend(kl) = l-1
        if (lend(kl) .ne. lbeg(kl)) goto 700
        write(punt,695)
        write(logfil,695)
695     format(//' xxxerrorxxx half space model not allowed.'/
     *         ' so stop.')
        stop 'abort from input1'
c check for variation in vp/vs
700     vpvsm(kl) = vpvs(lbeg(kl))
c        print *, 'vpvs for model ', kl, ' is ', vpvsm(kl)
c        print *, 'lbeg = ', lbeg
c        print *, 'lend = ', lend
c        print *, 'vpvs = ', vpvs
        do 705 i = lbeg(kl) + 1,lend(kl)
          if (vpvsm(kl) .ne. vpvs(i)) goto 710
705     continue
        goto 745
c define s model
c need to define s model
710     vpvsm(kl) = 0.0
        ishft = lend(kl) + 1 - lbeg(kl)
        nkrstn = nkrst + ishft
        if (nkrstn .le. lmax) goto 720
        write(punt,715) nkrstn,lmax
        write(logfil,715) nkrstn,lmax
715     format(//' xxxerrorxxx ',i5,' exceeds',i5,' the max. number'
     *  ,' of layers.',/,' so stop.')
        stop 'abort from input1'
720     ntomv = nkrst - lend(kl)
        nkrst = nkrstn
        if (ntomv .eq. 0) goto 730
c shift down remaining models
        do 725 i = 1,ntomv
          ii = lend(kl) + ntomv + 1 - i
          iii = nkrstn + 1 - i
          v(iii) = v(ii)
          d(iii) = d(ii)
          vpvs(iii) = vpvs(ii)
725     continue
730     do 735 i = 1,ishft
          ii = lbeg(kl) - 1 + i
          iii = lend(kl) + i
          v(iii) = v(ii)/vpvs(ii)
          d(iii) = d(ii)
          vpvs(iii) = 0.0
735     continue
        if (ilis.gt.0) write(punt,740)
740     format(//' the next model is for s only:')
        mtyp(kl) = 'p '
        mtyp(kl+1) = 's '
745     if (v(l) .eq. -1.) goto 790
        if (v(l).lt.0.01) then
          write(logfil, 750) v(l), kl
          write(punt, 750) v(l), kl
750       format(' xxxerrorxxx velocity may not be less than .01',
     *    ' but was ', f10.4, ' in model ', i4, ', so stop!')
          stop 'abort from input1'
        endif
        kl = kl+1
        if (kl.le.mmax) goto 760
        write(punt,755) mmax
        write(logfil,755) mmax
755     format(//' xxxerrorxxx too many velocity models. only ',i3,
     *  ' models allowed. so stop.')
        stop 'abort from input1'
760     lbeg(kl) = l
        if (ilis.gt.0) write(punt,685) kl
        thk(l) = d(l+1) - d(l)
        if (d(l) .eq. 0.0) goto 770
        write(punt,765)
        write(logfil,765)
765     format(//' xxxerrorxxx each structure must begin at a',
     *         ' depth of 0.0 km.  so stop.')
        stop 'abort from input1'
770     vi(l) = 1/v(l)
        vsq(l) = v(l)**2
        if (ilis.gt.0) write(punt,775) l,v(l),d(l),thk(l),vpvs(l)
775     format(1x,i5,2x,4f10.3)
        if ((l-lbeg(kl)+1).le.lmmax) goto 785
        write(punt,780) kl, lmmax
        write(logfil,780) kl, lmmax
780     format(//' xxxerrorxxx velocity model',i3,' has too many ',
     *         'layers.  only',i3,' layers are allowed.',/,
     *         ' so stop.')
        stop 'abort from input1'
785   continue
790   continue
c     take care of travel time tables.
      if( nttab .ne. 0) then
	do 792 i = 1, nttab
          call hycrt(i, i+20, vpvsm(i+mmax) ) 
	  if ( (vpvsm(i+mmax) .lt. 0.) .and. (nttab .lt. i+1) ) then
            write(logfil, '(3a)')
     *        ' negative vp/vs in third travel-time table requires',
     *        ' that a forth table be include for s travel times.',
     *        '  However, program is currently limited to 3 tables.'
            stop 'abort from input1'
	  endif
          modin(i+mmax) = i
          if (vpvsm(i+mmax) .eq. 0.0) vpvsm(i+mmax) = test(1) 
	  if (test(1) .lt. 0.) then
	    write(logfil, '(a)') 
     *        ' test(1) (vp/vs ratio) may not be negative'
            stop 'abort from input1'
	  endif
	  if ((i .gt. 1) .and. 
     *      (vpvsm(i+mmax-1) .lt. 0.)) modin(i+mmax) = 0
792	continue
      endif

      do 810 l = 1, ns
        if (mod(l) .le. kl) then
c         this is fine, unless it is an s model
          if ( mtyp(mod(l)) .eq. 's ') then
            write(punt, 795) nsta(l), mod(l)
            write(logfil, 795) nsta(l), mod(l)
795         format(' xxxerrorxxx velocity model specified for station ',
     *      a, ' was ', i4, ', an s-model, so stop!')
            stop 'abort from input1'
          endif
        else if ( ((mod(l) .gt. kl) .and. (mod(l) .lt. 11)) .or.
     *            (mod(l) .gt. mmax+3) ) then
c         model out of range, so switch to highest model
          msav = mod(l)
          mod(l) = kl
c         if the last model is an s model, use the previous one.
          if (mtyp(kl) .eq. 's ') mod(l) = kl - 1
          write(punt, 800) nsta(l), msav, mod(l)
          write(logfil, 800) nsta(l), msav, mod(l)
800       format(' xxx warning xxx velocity model specified for station',
     *    a, ' was too large (equaled ',i3, ').  reset to ', i3/)
        else if ( modin(mod(l)) .eq. 0 ) then
c         mod(l) is mmax+1, +2, or +3, and it doesn't exist or is not a p model
          write(punt, 801) nsta(l), mod(l)
          write(logfil, 801) nsta(l), mod(l)
801	  format(' xxx error xxx velocity model specified for station',
     *      a, '(', i3, 
     *      ') was not defined or was assigned to an s table.')
          stop 'abort from input1'
	endif
810   continue
      if (nkrst .eq. 0) goto 871
c set up arrays to be used in trvdrv
      do 815 l = 1, lmax
815   jref(l) = 0
      if (iprn .le. 3) goto 825
      if (ilis.gt.0) write(punt, 820)
820   format(//1x, '   leq    m      tid(leq, m)      did(leq, m) '
     * 38x, 'input1 formats 700 and 760.')
825   do 870 imod = 1, kl
c       loop through models, imod
        nlayers = lend(imod) - lbeg(imod)+1
        do 840 leq = lbeg(imod), lend(imod)
c         loop through eq layers, leq
          do 839 mrefr = 1, nlayers
            mref = lbeg(imod) - 1 + mrefr
            if ( (mref .gt. leq) .and. (v(mref) .gt. v(leq)) ) then
              vsqd(mrefr, leq) =
     *          sqrt((v(mref)/v(leq)-1.)*(v(mref)/v(leq)+1.))
            else
              vsqd(mrefr, leq) = 0.
            endif
            if (leq .ge. mref) then
              f(leq, mrefr) = 2.0
            else
              f(leq, mrefr) = 1.0
            endif
839       continue
840     continue
        ivl = lbeg(imod) + ivlr - 1
        do 870 leq = lbeg(imod), lend(imod)
c         loop through eq layers, leq
          leqr = leq - lbeg(imod) + 1
          if (leq .eq. ivl) then
c           preserve original thicknesses of variable layers
            sthk(imod) = thk(leq)
            sthk1(imod) = thk(leq + 1)
          endif
          do 870 mrefr = leqr, nlayers
c           loop through refracter layers, mrefr
            if (mrefr .eq. 1) goto 870
            sumt = 0.0
            sumd = 0.0
            do 850 l = lbeg(imod), lbeg(imod) + mrefr - 2
c             true if mref .le. l .or. v(mref) .le. v(l)
c             skip if refracter velocity .le. any overlaying velocity
c             if (vsqd(mrefr, l) .eq. 0.) goto 860
              if (v(lbeg(imod) + mrefr - 1) .le. v(l)) goto 860
850         continue
            jref(mrefr + lbeg(imod) - 1) = 1
            do 855 l = lbeg(imod), lbeg(imod) + mrefr - 2
c             loop from top layer to layer above refractor
              if (((l.eq.ivl) .or. (l.eq.ivl+1))
     *        .and. (ivlr .gt. 0))goto 855
              sumt = sumt + f(l, leqr)*thk(l)*vsqd(mrefr, l)
              sumd = sumd + f(l, leqr)*thk(l)/vsqd(mrefr, l)
855         continue
860         tid(leq, mrefr) = sumt*vi(mrefr + lbeg(imod) - 1)
            did(leq, mrefr) = sumd
            if (iprn .le. 3) goto 870
            if (ilis.gt.0) write(punt, 865)
     *        leq, mrefr, tid(leq, mrefr), did(leq, mrefr)
865         format(1x, 2i5, 2f15.3)
870   continue
871   do 875 l = 1, ns
875   thks(l) = blank
c set up trial hypocenter read in as test variables
      latr = 0.0
      lonr = 0.0
      if ((abs(test(3))+abs(test(4))).le.0.00001) goto 880
      la = abs(test(3))
      lo = abs(test(4))
      ala = (abs(test(3)) - la)*60.
      alo = (abs(test(4)) - lo)*60.
      ins = 'n'
      iew = 'w'
      if (test(3) .lt. 0.) ins = 's'
      if (test(4) .lt. 0.) iew = 'e'
      call fold2(latr, lonr, la, ins, ala, lo, iew, alo)
880   continue
885   continue
      return
      end
c end input1
