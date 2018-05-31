c update.for    []
      subroutine update(indexs, nsta, ielv,
     *         lat, lon, c, e, sw,
     *         klas, calr, xmgc, fmwt, xmwt, fmgc, ipcod, iscod, tpdly,
     *         revp, ndate, nhr, ns,
     *         exdly, icent2, kdate, ihrmn,
     *         infil, iofil)
c given a current kdate and ihrmn, revise the station list arrays
c for all stations
      include 'params.inc' 
      parameter (ndly = 11)
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      parameter (irecsz = 132)
      real lat,lon
      dimension ielv(nsn),nsta(nsn)
      dimension lat(nsn),lon(nsn)
      dimension c(nsn),e(nsn),sw(nsn),ndate(nsn),nhr(nsn),klas(5, nsn)
      dimension calr(5, nsn),fmgc(nsn),xmgc(nsn),ipcod(nsn),iscod(nsn)
      integer fmwt, xmwt
      dimension fmwt(nsn),xmwt(nsn),tpdly(2,nsn)
      character*1 exdly(4, nsn), revp(6, nsn)
      character icard*(irecsz), dnstrg*(irecsz)
      character*4 nsta*5, nsta5*5
      character*1 ins, iew
      integer punt
      common /punt/ punt
cd    write(punt,5001) infil, iofil
cd5001 format(' sub update', 2i5)
      ihr = ihrmn/100
      rewind infil
      rewind iofil
      insv = infil
      infil = iofil
      iofil = insv
      skrd = 0
      in1 = 0
      l = 0
      kfnd = 0
cd    write(punt, '(a)') ' loop to find expired stations'
  200 l = l + 1
      if (l .gt. ns) goto 940
      call diftim(icent2, kdate,ihr,ndate(l),nhr(l),iporm,difhr)
      if (difhr .le. 0.) goto 200
  350 continue
cd    write(punt, '(a)') ' stat. l expired, so loop for current parameters'
      if (skrd .eq. 1) goto 432
      if ((in1 .eq. 0) .and. (indexs .gt. 0))
     *  write(punt,420) icent2,kdate,ihr
  420 format(' stations must be updated ', /,
     * ' date and time of next event:  ',i2,i6.6,1x,i2)
c
c main loop begins here ***************************************
c
  400 read(infil,425) icard
  425 format(a)
      icard = dnstrg(icard)
      nsta5 = icard(1:5)
      read(icard, 426) icymd, ih
  426 format(37x, i8, i2)
c     write(punt,5000) nsta5,icymd,ih,icard
c 5000 format(1x,a5,1x,i6,1x,i4,/,1x,a)
      if (nsta5(1:4) .eq. ' end') then
        write(iofil, '(a)') icard
        rewind infil
        rewind iofil
        insv = infil
        infil = iofil
        iofil = insv
        goto 800
      endif
c     write(punt, '(4a)') 'compare ', nsta5, ' with ', nsta(l)
      if ((kfnd .eq. 1) .and. (nsta5 .ne. nsta(l))) goto 750
  432 skrd = 0
      if (nsta5 .ne. nsta(l)) goto 480
cd    write(punt, '(a)') ' found correct station'
      kfnd = 1
      if (icymd .eq. 0) icymd = 99999999
      if (icymd .eq. 0) ih = 99
      if (icymd .lt. icent2*1000000+kdate) goto 400
      if ((icymd .eq. icent2*1000000+kdate) .and.
     *   (ih .lt. ihr)) goto 400
cd    write(punt, '(a)') ' found current parameters, so update information'
      call rdtmdp(l, icard, nsta, ielv,
     *         lat, lon, c, e, sw,
     *         klas, calr, xmgc, fmwt, xmwt, fmgc, ipcod, iscod, tpdly,
     *         revp, ndate, nhr,
     *         exdly)
      if ((in1 .eq. 0) .and. (indexs .gt. 0)) write(punt,450)
  450 format(' name  stawt sys calr xmgc mgwt fmgc wght tl- code ',
     * 'alt-  pol   expiration   latitude    longitude  elev',/,
     *       '                                      p s dly      ',
     * 'dly        cnyrmody hr')
      in1 = 1
      if (indexs .gt. 0) then
        call unfold2(lat(l),lon(l),llat,ins,alat,llon,iew,alon)
        write(punt,475) nsta(l),  sw(l), klas(1, l),calr(1, l),
     *  xmgc(l), fmwt(l), xmwt(l), fmgc(l), ipcod(l), iscod(l), 
     *  tpdly(1,l),
     *  (exdly(i, l), i = 1, 4), tpdly(2,l), (revp(i,l), i = 1, 6),
     *  ndate(l), nhr(l),
     *  llat,   ins, alat, llon,   iew, alon, ielv(l)
  475   format(       1x,a5, 1x,f5.2,        i3,    1x,f5.2,
     *     f5.2,      i3,     i2,     f5.2,   1x,i2,       i2, 
     *        f5.2,
     *                      4a1,       f5.2,                   6a1,
     *        i9,       i3,
     *    i4, 1x,a1, f6.2,   i5, 1x,a1, f6.2,      i5)
      endif
      kfnd = 0
      goto 200
  480 continue
cd    write(punt, '(a)') ' write out - wrong station'
      write(iofil, '(a)') icard
      goto 400
  750 continue
cd    write(punt, '(a)') ' station has expired'
      skrd = 1
  800 if (indexs .gt. 0) write(punt,850) l, nsta(l)
  850 format(' the ',i4,'th station:',1x,a5,' has expired.')
      ndate(l) = 99999998
      kfnd = 0
      goto 200
  940 continue
cd    write(punt, '(a)') ' put rest of infil on iofil'
      if (skrd .eq. 1) goto 975
  950 read(infil, '(a)', end=1000) icard
  975 write(iofil, '(a)') icard
      goto 950
 1000 return
      end
c end update
