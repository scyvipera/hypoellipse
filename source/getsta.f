c getsta.for    []
      subroutine getsta(   indexs, nsta, ielv, mod,
     *         ipthk, vthk, ipdly, dly, sdly, lat, lon, c, e, sw,
     *         klas, calr, xmgc, fmwt, xmwt, fmgc, ipcod, iscod, tpdly,
     *         revp, ndate, nhr, ns,
     *         prr, inpt, nreloc, inmain, injump, exdly, ibate, 
     *         test53)
c cole sonafrank notes of august, 1988:
c restructured the code a bit while trying to follow the logic...
c first reading of station list for parameters in effect on ibate
      include 'params.inc' 
      parameter (ndly = 11)
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      parameter (irecsz = 132)
      integer punt
      common /punt/ punt
      real lat,lon
      dimension ielv(nsn),nsta(nsn),dly(ndly,nsn),mod(nsn),ipthk(nsn)
      dimension vthk(2,nsn),ipdly(nsn),sdly(ndly,nsn),lat(nsn),lon(nsn)
      dimension c(nsn),e(nsn),sw(nsn),ndate(nsn),nhr(nsn),klas(5, nsn)
      dimension calr(5, nsn),fmgc(nsn),xmgc(nsn),ipcod(nsn),iscod(nsn)
      integer fmwt, xmwt
      dimension fmwt(nsn),xmwt(nsn),tpdly(2,nsn),prr(nsn)
      character*1 exdly(4, nsn), revp(6, nsn)
      character icard*(irecsz), icardup*(irecsz), dnstrg*(irecsz)
      character*4 nsta4, nsta*5, name, iast*1, rshft, nsta5
      character*1 ins, iew, izne
      integer iostat
      l = 1
      logfil = punt
c     write(logfil, '(a)') 'begin getsta.for - unit logfil'
  312 read(inpt, '(a)', end=999) icardup
      icard = dnstrg(icardup)
      name = rshft(icard(1:4))
      if(name(1:2) .eq. 'c*') goto 312
      if(name .eq. 'jump') then
        if(inpt .eq. inmain) then
          inpt = injump
c	  write (logfil, '(2a)') 'jump to station list: ', icardup(6:55)
          call openfl( injump, icardup(6:55), 'old', 'zero', 'readonly',
     *    'none', 0)
          goto 312
        else
          write(punt, 319) icard(1:55)
  319     format(' xxxerrorxxx can not nest jump statements ', /, 1x, a)
          stop
        endif
      endif
c
c ---   read primary station data values.
  320 continue
      if(nreloc .eq. 0) then
c        write (logfil,
c    *   '('' reading primary station values.  nreloc='',i2)')
c    *    nreloc
c        write (logfil, '(1x, a)') icard
        read(icard, 325)
     *    nsta4,llat,ins,alat,llon,iew,alon,ielv(l),
     *    mod(l),ipthk(l),vthk(1,l),vthk(2,l),ipdly(l),(dly(i,l),
     *    sdly(i,l),i=1,5), izne
  325   format(a4,i2,a1,f5.3,1x,i3,a1,f5.3,i5,i2,i1,2f4.2,i1,
     *    10f4.2,a1)
c ---   shift name to right and add component
	nsta(l)(1:4) = rshft(nsta4)

c for 4-letter codes, if the last letter is 'e' or 'n' then
c assume this is a horizontal component station!


        if(test53 .eq. 1.0) then
          if(nsta4(1:1) .ne. ' ') then
            if((nsta4(4:4) .eq. 'n') .or. (nsta4(4:4) .eq. 'e')) then
              izne = nsta4(4:4)
            endif
          endif
        endif

	if((izne .ne. 'e') .and. (izne .ne. 'n')) then
	  nsta(l)(5:5) = 'z'
	else
	  nsta(l)(5:5) = izne
	endif
        
c zero the extra delay models
        do 3255 i = 6, ndly-1
          dly(i,l) = 0.0
          sdly(i,l) = 0.0
3255    continue
      else
c        write (logfil,
c    *  '('' reading primary values without delays.  nreloc='', i2)')
c    *  nreloc
c ---   do not read delays if job is now recycling.
        read(icard, 326)
     *  nsta4,llat,ins,alat,llon,iew,alon,ielv(l),
     *  mod(l),ipthk(l),vthk(1,l),vthk(2,l),ipdly(l),izne
  326   format(a4,i2,a1,f5.3,1x,i3,a1,f5.3,i5,i2,i1,2f4.2,i1,40x,a1)
c ---   shift name to right and add component
	nsta(l)(1:4) = rshft(nsta4)

c for 4-letter codes, if the last letter is 'e' or 'n' then
c assume this is a horizontal component station!

        if(test53 .eq. 1.0) then
          if(nsta4(1:1) .ne. ' ') then
            if((nsta4(4:4) .eq. 'n') .or. (nsta4(4:4) .eq. 'e')) then
              izne = nsta4(4:4)
            endif
          endif
        endif

	if((izne .ne. 'e') .and. (izne .ne. 'n')) then
	  nsta(l)(5:5) = 'z'
	else
	  nsta(l)(5:5) = izne
	endif
      endif
c --- just read primary record
      if (nsta(l)(1:4) .eq. ' end') goto 991
      if (mod(l) .lt. 1) mod(l) = 1
      prr(l) = 0.1
      call fold2(lat(l),lon(l),llat,ins,alat,llon,iew,alon)
      if (l .eq. 1) then
        c(1) = 0.0
        e(1) = 0.0
      else
        call delaz(lat(1),lon(1),delt,deldeg,azz,lat(l),lon(l))
        c(l) = delt*sin(azz*rad)
        e(l) = delt*cos(azz*rad)
      endif
      if (ipthk(l) .ne. 2) ipthk(l) = 1
      if ((ipdly(l) .lt. 1) .or. (ipdly(l) .gt. 10)) ipdly(l) = 1
c
c --- read time dependent station data.
330   read(inpt, '(a)', iostat=iostat) icard
      icard = dnstrg(icard)
      if(icard(1:2) .eq. 'c*') goto 330
      if (iostat.eq.-1) then
        if(inpt .eq. injump) then
          inpt = inmain
          icard(1:4) = ' end'
          goto 390
        else
          goto 999
        endif
      endif
c decode a time dependent station record (icard) for station number l
      if(icard(5:5) .ne. '*') goto 395
      call rdtmdp(l, icard, nsta, ielv,
     *         lat, lon, c, e, sw,
     *         klas, calr, xmgc, fmwt, xmwt, fmgc, ipcod, iscod, tpdly,
     *         revp, ndate, nhr,
     *         exdly)
      if (ndate(l) .lt. ibate) goto 350
c
c --- station not expired, so read next record
  340 read(inpt, '(a)', iostat=iostat) icard
      icard = dnstrg(icard)
      if(icard(1:2) .eq. 'c*') goto 340
c --- shift name to right 
      nsta4 = rshft(icard(1:4))
      if (iostat.eq.-1) then
        if(inpt .eq. injump) then
          inpt = inmain
          icard(1:4) = ' end'
          iast = ' '
          goto 990
        else
          goto 999
        endif
      endif
      iast = icard(5:5)
      izne = icard(80:80)
c     write(logfil, '(a, 1x, a, 1x, a, 1x, i5)') 
c    *  nsta4, iast, nsta(l), l
      if (icard(1:4) .eq. ' end') goto 990
      if ((iast .ne. '*') .or. (nsta4 .ne. nsta(l)(1:4))) then
c --- this is NOT another time-dependent record
	if (iast .ne. '*') then
c         this SHOULD be a new station

c for 4-letter codes, if the last letter is 'e' or 'n' then
c assume this is a horizontal component station!

          if(test53 .eq. 1.0) then
            if(nsta4(1:1) .ne. ' ') then
              if((nsta4(4:4) .eq. 'n') .or. (nsta4(4:4) .eq. 'e')) then
                izne = nsta4(4:4)
              endif
            endif
          endif

	  if((izne .ne. 'e') .and. (izne .ne. 'n')) then
	    nsta5 = nsta4//'z'
	  else
	    nsta5 = nsta4//izne
	  endif
          if (nsta5 .ne. nsta(l))  then
c           this IS a new station
            goto 360
          else
c	    the same station AGAIN!  A clear error.
	    goto 392
	  endif
	else
c	  it is an error to have an * with a different station name
	  goto 392
	endif
      endif
c --- write extra time dependent record
      write(2,'(2a)') nsta(l), icard(6:lentru(icard))
      goto 340

c --- station expired so far, look for unexpired one.
  350 continue
c     write (logfil, '('' station expired so far'')')
      read(inpt, '(a)', iostat=iostat) icard
      icard = dnstrg(icard)
c     write (logfil,'(x,a)') icard
      if(icard(1:2) .eq. 'c*') goto 350
      nsta4 = rshft(icard(1:4))
      if (iostat.eq.-1) then
        if (inpt .eq. injump) then
          inpt = inmain
          goto 991
        else
          goto 999
        endif
      endif
      iast = icard(5:5)
      izne = icard(80:80)
      if (nsta4 .eq. ' end') goto 991
      if ((iast .ne. '*') .or. (nsta4 .ne. nsta(l)(1:4))) then
c --- this is NOT another time-dependent record
	if (iast .ne. '*') then
c         this SHOULD be a new station

c for 4-letter codes, if the last letter is 'e' or 'n' then
c assume this is a horizontal component station!

          if(test53 .eq. 1.0) then
            if(nsta4(1:1) .ne. ' ') then
              if((nsta4(4:4) .eq. 'n') .or. (nsta4(4:4) .eq. 'e')) then
                izne = nsta4(4:4)
              endif
            endif
          endif

	  if((izne .ne. 'e') .and. (izne .ne. 'n')) then
	    nsta5 = nsta4//'z'
	  else
	    nsta5 = nsta4//izne
	  endif
          if (nsta5 .ne. nsta(l))  then
c           this IS a new station
            goto 320
          else
c	    the same station AGAIN!  A clear error.
	    goto 393
	  endif
	else
c	  it is an error to have an * with a different station name
	  goto 392
	endif
      endif
      if(icard(5:5) .ne. '*') goto 395
      call rdtmdp(l, icard, nsta, ielv,
     *         lat, lon, c, e, sw,
     *         klas, calr, xmgc, fmwt, xmwt, fmgc, ipcod, iscod, tpdly,
     *         revp, ndate, nhr,
     *         exdly)
      if (ndate(l) .lt. ibate) goto 350

c --- found non-expired data for this one, save extra data.
      goto 340

  360 if (indexs .gt. 0) call iprst(l,llat,ins,alat,llon,iew,alon)
      l = l + 1
c     write (logfil, '('' 360: l='',i3)') l
      if (l .le. nsn) goto 320
      write(punt,370) nsn
  370 format(///,' xxxerrorxxx  station list exceeds max of ', 
     *  i5, ' stations', /, ' so stop.')
      stop
  390 write(punt,380) l,nsta(l),name,iast
  380 format(///,'  xxxerrorxxx 390 in station list format, so stop',/,
     *  1x,i5,'th station ',2('"',a5,'" '),a1)
      stop
  392 write(punt,382) l, nsta(l), name, iast
  382 format(///,'  xxxerrorxxx 392 in station list format, so stop',/,
     *  1x,i5,'th station ',2('"',a5,'" '),a1)
      stop
  393 write(punt,383) l,nsta(l),name,iast
  383 format(///,'  xxxerrorxxx 393 in station list format, so stop',/,
     *  1x,i5,'th station ',2('"',a5,'" '),a1)
      stop
  394 write(punt,384) l,nsta(l),name,iast
  384 format(///,'  xxxerrorxxx 394 in station list format, so stop',/,
     *  1x,i5,'th station ',2('"',a5,'" '),a1)
      stop
  395 write(punt, 385) l, nsta(l), name, iast
  385 format(///,'  xxxerrorxxx in station list format, so stop',/,
     *  1x,i5,'th station ',2('"',a5,'" '),a1, 
     *  ' missing time-dependent record')
 990  if (indexs .gt. 0) call iprst(l,llat,ins,alat,llon,iew,alon)
      l = l + 1
      if (iast .eq. '*') goto 394
 991  ns = l - 1
      write(2, 992)
 992  format(' end')
      return
 999  write(punt, 389)
 389  format (' xxxerrorxxx unexpected end of file on station list')
      stop 'error - getsta'
      end
c end getsta
