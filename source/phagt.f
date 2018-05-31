c phagt.for    [unix]
      subroutine phagt(phcard, keyph, lph, inpt, injump, eoff, icent2,
     *  itest)
c read the data associated with one earthquake
c
c     phcard(npa)     array of records read
c     keyph(npa)      key to record type
c                       0 --- phase record
c                      -1 --- comment record
c                      -2 --- summary record
c                      -3 --- instruction record
c                             (begins with more, dist, or 4 blanks)
c                      -4 --- bad time
c                      -5 --- reset record
c                      -6 --- save record - no longer used
c                      -7 --- rerun record - no longer used
c                      -8 --- scatter record
c                      -9 --- not on station list
c                     -10 --- jump record
c                     -11 --- decode error
c                     -12 --- sleep record
c                     -13 --- stop record
c                     -14 --- nongaus record
c                     -15 --- master scatter record
c     lph             number of records returned
c     inpt            unit number for reading data
c     eoff            .false. --- end of file not reached
c                     .true. --- found end of file
c     npa            maximum number of phases allowed
c
      save onesav
      include 'params.inc' 
      character*10 begtim, dnstrg*4, pname*4, foocard*115, acent2*2
      integer punt, icentsave, icent2, itest(100)
      common /punt/ punt
      common /logfil/ logfil
      common /dmost/ ipun,ivlr,blank
      character*117 phcard(npa), fname*50
      dimension keyph(npa)
      logical onesav, eoff, fsteq
      data onesav/.false./,fsteq/.true./
      eoff = .false.
      icentsave = 0
      npurep = 0
c
      lph = 1
c     write(punt, *) 'onesav = ', onesav
      if (onesav) then
        phcard(1) = phcard(isv)
        keyph(1) = keyph(isv)
        onesav = .false.
        if ((keyph(1) .ne. -2) .and. (keyph(1) .ne. 0)) then
          return
        endif
        goto 25
      endif
c
c loop from lph = 1 to npa
c     print *,'phaget about to read unit ',inpt
15    read(inpt, 20, end = 70) phcard(lph)
20    format(a)
c
c classify record
25    continue
      pname = dnstrg(phcard(lph)(1:4))
c
c comment record
      if (pname(1:2) .eq. 'c*') then
        if( (ipun .eq. 2 .or. ipun .eq. 3) .and.
     *    (lph .eq. 1) .and. (fsteq) ) then
c comment records prior to the first summary or phase record should
c be written directly to the archive phase file
          write(11, '(a)') phcard(lph)
          goto 15
        endif
        keyph(lph) = -1
        goto 50
c
c summary record in 115 column format
      else if ((phcard(lph)(81:81) .eq. '/') .or.
c* (vax
c* (pc
c     *  (phcard(lph)(81:81) .eq. '\')) then
c* pc)
c* vax)
c* (unix
     *  (phcard(lph)(81:81) .eq. '\\')) then
c* unix)
        keyph(lph) = -2
	icent2 = itest(55)
	write(phcard(lph)(116:117), '(i2)') icent2
	if(icentsave .ne. 0) then
c two summary records with different centuries will bomb out the program
	  if(icentsave .ne. icent2) then
	    write(punt, '(a,/)') 
     *        'xxxerrorxxx in phagt.  Two summary records for the ',
     *        'save event with different centuries. ', phcard(lph)
	    write(logfil, '(a,/)') 
     *        'xxxerrorxxx in phagt.  Two summary records for the ',
     *        'save event with different centuries. ', phcard(lph)
	    stop
	  endif
	endif
        icentsave = icent2

        if (npurep .gt. 0) goto 90
        goto 50
c
c summary record in 117 column format
      else if ((phcard(lph)(83:83) .eq. '/') .or.
c* (vax
c* (pc
c     *  (phcard(lph)(83:83) .eq. '\')) then
c* pc)
c* vax)
c* (unix
     *  (phcard(lph)(83:83) .eq. '\\')) then
c* unix)
        keyph(lph) = -2
c must be in new 117-column format, so switch back for internal use
        read(phcard(lph)(1:2), '(i2, t1, a2)') icent2, acent2
        foocard = phcard(lph)(3:117)
        phcard(lph) = foocard//acent2
        if(icentsave .ne. 0) then
c two summary records with different centuries will bomb out the program
          if(icentsave .ne. icent2) then
            write(punt, '(a,/)')
     *        'xxxerrorxxx in phagt.  Two summary records for the ',
     *        'save event with different centuries. ', phcard(lph)
            write(logfil, '(a,/)')
     *        'xxxerrorxxx in phagt.  Two summary records for the ',
     *        'save event with different centuries. ', phcard(lph)
	    stop
          endif
        endif
        icentsave = icent2
 
        if (npurep .gt. 0) goto 90
        goto 50
c
c instruction record
      else if ((pname .eq. '    ') .or.
     *        (pname .eq. 'more')) then
c    *   .or. (pname .eq. 'dist')) then
        keyph(lph) = -3
        goto 80
c
c reset record
      else if (pname .eq. 'rese') then
        keyph(lph) = -5
        if (lph .gt. 1) goto 90
        return
c
c save record
      else if (pname .eq. 'save') then
        keyph(lph) = -6
        if (lph .gt. 1) goto 90
        return
c
c rerun record
      else if (pname .eq. 'reru') then
        keyph(lph) = -7
        if (lph .gt. 1) goto 90
        return
c
c scatter record
      else if (pname .eq. 'scat') then
        keyph(lph) = -8
        if (lph .gt. 1) goto 90
        return
c
c nongaus record
      else if (pname .eq. 'nong') then
        keyph(lph) = -14
        if (lph .gt. 1) goto 90
        return
c
c master scatter record
      else if (pname .eq. 'mast') then
        keyph(lph) = -15
        if (lph .gt. 1) goto 90
        return
c
c sleep record
      else if (pname .eq. 'slee') then
        keyph(lph) = -12
        if (lph .gt. 1) goto 90
        return
c
c stop record
      else if (pname .eq. 'stop') then
        keyph(lph) = -13
        if (lph .gt. 1) goto 90
        return
c
c jump record
      else if (pname .eq. 'jump') then
        keyph(lph) = -10
        fname = phcard(lph)(6:55)
        begtim = phcard(lph)(56:65)
        if (inpt .eq. injump) then
          close (unit = injump, iostat = iostat)
          write(punt, 40)
          write(logfil, 40)
40        format(' xxxerrorxxx can not jump from a jump file.')
          stop
        else
          inpt = injump
          write(punt, 41) fname
          write(logfil, 41) fname
41        format(' jump to ', a)
          close (unit = injump, iostat = iostat)
c         call openfl( iunit, ifile, istat,  izero, ishr,
c    *    iform, irecl)
          call openfl(injump, fname, 'old', 'zero', 'readonly',
     *    'none', 0)
c          open(unit=injump, file=fname, blank='zero', status='old',
c     *    iostat = iostat)
c          if (iostat .ne. 0) goto 498
          if(begtim .ne. ' ') then
            write(punt, 412) begtim
            write(logfil, 412) begtim
412         format(' begin processing with event:  ', a)
            if(lph .ne. 1) then
              write(punt, 413)
              write(logfil, 413)
413           format(' *** jump record must be between events ***', /,
     *        ' *** instruction record missing, so stop ***')
              stop
            endif
            do 417 i = 10, 1, -1
              if(begtim(i:i) .ne. ' ') goto 418
417         continue
418         lenbt = i
420         read(injump, '(a)', end = 430) phcard(lph)
            if(phcard(lph)(1:lenbt) .eq. begtim(1:lenbt)) goto 25
            goto 420
430         write(punt, 435) begtim
            write(logfil, 435) begtim
435         format(' *** could not find ', a, ' so stop ***')
            stop
          endif
        endif
        if (lph .gt. 1) goto 90
        goto 15
c498     write(punt, 499) fname
c        write(logfil, 499) fname
c499     format(' *** jump file ', a, /, ' could not be opened',
c     *  ' *** so stop ***')
c        stop
      endif
c
c must be regular phase record
      keyph(lph) = 0
      npurep = npurep + 1
50    fsteq = .false.
      lph = lph + 1
      if (lph .le. npa) goto 15
c
55    write(logfil, 60) npa
      write(punt, 60) npa
60    format(///,' xxxxx exceeded maximum of ',i4,
     *           ' records per event, so stop!  xxxxx')
      stop
c
c come here on eof detected
70    eoff = .true.
c     write(punt, *) 'just set eoff to true in phagt.  eoff = ', eoff
      onesav = .false.
      if (lph .gt. 1) then
        phcard(lph) = ' '
        keyph(lph) = -3
        write(logfil, 72)
        write(punt, 72)
72      format(//, ' detected end of file prior to final instruction',
     *  ' record.', /, ' check input data for completeness.')
        return
      else
        lph = lph - 1
        return
      endif
c
c come here after an instruction record or other special record
80    continue
      return
c
c come here if missing an instruction record
c     ('add' blank instruction record)
90    isv = lph + 1
      phcard(isv) = phcard(lph)
      keyph(isv) = keyph(lph)
      phcard(lph) = ' '
      keyph(lph) = -3
      onesav = .true.
      write(logfil, 92) phcard(isv)
      write(punt, 92) phcard(isv)
92    format(/, ' xxxxx warning - missing instruction record - xxxxx',
     *       /, ' prior to this record:', /, ' ', a)
      return
      end
c end phagt
