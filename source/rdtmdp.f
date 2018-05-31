c rdtmdp.for    []
      subroutine rdtmdp(l, icard, nsta, ielv,
     *         lat, lon, c, e, sw,
     *         klas, calr, xmgc, fmwt, xmwt, fmgc, ipcod, iscod, tpdly,
     *         revp, ndate, nhr,
     *         exdly)
c decode a time dependent station record (icard) for station number l
      include 'params.inc' 
      parameter (ndly = 11)
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      parameter (irecsz = 132)
      integer punt
      common /punt/ punt
      common /logfil/ logfil
      real lat,lon
      dimension ielv(nsn),nsta(nsn)
      dimension lat(nsn),lon(nsn)
      dimension c(nsn),e(nsn),sw(nsn),ndate(nsn),nhr(nsn),klas(5, nsn)
      dimension calr(5, nsn),fmgc(nsn),xmgc(nsn),ipcod(nsn),iscod(nsn)
      integer fmwt, xmwt
      dimension fmwt(nsn),xmwt(nsn),tpdly(2,nsn)
      character*1 exdly(4, nsn), revp(6, nsn)
      character icard*(irecsz) 
      character*4 nsta*5, rshft, nsta4*4
      character*1 ins, iew, dnstrg

      read(icard, 334) nsta4, sw(l), klas(1, l), calr(1, l),
     * xmgc(l), xmwt(l), fmwt(l), fmgc(l), ipcod(l), iscod(l),
     * ndate(l), nhr(l), tpdly(1, l), exdly(1, l), exdly(2, l),
     * exdly(3, l), exdly(4, l),  tpdly(2, l), revp(1,l),  revp(2,l),
     * revp(3,l),  revp(4,l), revp(5,l), revp(6,l),
     * klas(2, l), calr(2, l), klas(3, l), calr(3, l),
     * klas(4, l), calr(4, l), klas(5, l), calr(5, l)
  334 format(            a4,   1x, f4.2,          i2,    4x,f5.2,
     *    f4.2,      i1, 1x, i1,    f4.2,       i1,       i1,
     *    4x,i8,     i2,        f4.2,         a1,          a1,
     *          a1,          a1,        f4.2,          a1,          a1,
     *          a1,          a1,        a1,     a1,6x,
     *  4(i2, 4x, f5.2, 1x))
      do 445 kk = 1, 4
	  exdly(kk,l) = dnstrg(exdly(kk,l))
c reduce all sources equivalent to usgs develocorder to 'v'
          if ( exdly(kk,l) .eq. '*' .or. exdly(kk,l) .eq. '1' .or.
     *         exdly(kk,l) .eq. '4')
     *         exdly(kk,l) = 'v'
c reduce all sources equivalent to uagi develocorder to 'f'
          if ( exdly(kk,l) .eq. '%' .or. exdly(kk,l) .eq. 'a')
     *         exdly(kk,l) = 'f'
c reduce all sources equivalent to usgs mag tape to 's'
          if ( exdly(kk,l) .eq. 'e' .or. exdly(kk,l) .eq. '2')
     *         exdly(kk,l) = 's'
c reduce all sources equivalent to Dan/Daq or pc's to 'd'
c         if ( exdly(kk,l) .eq. 'j' .or. exdly(kk,l) .eq. 'x'
c do not include 'p' because yakutat pc is 'p' and has no satellite delay. jcl 6/3/94
c    *       .or. exdly(kk,l) .eq. 'o'
c    *       .or. exdly(kk,l) .eq. 'g' .or. exdly(kk,l) .eq. 'k'
c    *       .or. exdly(kk,l) .eq. 'i' .or. exdly(kk,l) .eq. 'u')
c    *         exdly(kk,l) = 'd'
c reduce all sources equivalent to uagi daq/dan 'd'
          if (exdly(kk,l) .eq. 'j' .or. exdly(kk,l) .eq. 'x') 
     *      exdly(kk,l) = 'd'
c reduce all sources equivalent to uagi pc's to 'g'
          if (exdly(kk,l) .eq. 'o'
     *      .or. exdly(kk,l) .eq. 'k' .or. exdly(kk,l) .eq. 'i'
     *      .or. exdly(kk,l) .eq. 'u') exdly(kk,l) = 'g'

445   continue
      if (icard(119:123) .ne. ' ') then
        read(icard, 335) alat, alon, ielv(l)
  335   format(118x, f5.3, f5.3, i4)
        call unfold2(lat(l),lon(l),llat,ins,dum1,llon,iew,dum2)
        call fold2(lat(l),lon(l),llat,ins,alat,llon,iew,alon)
        call delaz(lat(1),lon(1),delt,deldeg,azz,lat(l),lon(l))
        c(l) = delt*sin(azz*rad)
        e(l) = delt*cos(azz*rad)
      endif
      nsta4 = rshft(nsta4)
      if (nsta4 .ne. nsta(l)(1:4)) then
        write(punt,381) l,nsta(l)(1:4),nsta4
        write(logfil,381) l,nsta(l)(1:4),nsta4
  381   format(///,'  xxxerrorxxx 391 in station list format, so stop',
     *  /, 1x, i5, 'th station ', 2('"', a4, '" '))
        stop 'abort from rdtmdp'
      endif
c fix up default parameters for blank fields on the station record
c station weight
      if (icard(6:9) .eq. '    ') sw(l) = 1.0
c system type
      if (icard(10:11) .eq. '  ') klas(1, l) = 1
c fmag weight
      if (icard(25:25) .eq. ' ') xmwt(l) = 1.
c xmag weight
      if (icard(27:27) .eq. ' ') fmwt(l) = 1.
c fmag coda multiplier
      if (fmgc(l) .eq. 0.) fmgc(l) = 1.
c  pcode weight-code replacement
      if (icard(32:32) .eq. ' ') ipcod(l) = 10
c scode weight-code replacement
      if (icard(33:33) .eq. ' ') iscod(l) = 10
c expiration date
      if (ndate(l) .eq. 0) ndate(l) = 99999999
      if (ndate(l) .eq. 99999999) nhr(l) = 99
c reversal indicators
      do 332 i = 1, 6
        if((revp(i, l) .ne. ' ') .and.
     *     (revp(i, l) .ne. 'n') .and.
     *     (revp(i, l) .ne. 'r') .and.
     *     (revp(i, l) .ne. '+') .and.
     *     (revp(i, l) .ne. '-') .and.
     *     (revp(i, l) .ne. '?')) then
          write(logfil, '(3a, /, a, /, a)')
     *    ' polarity reversal indicator must be n, r, +, -, or ?,',
     *    ' not ', revp(i, l), icard,
     *    ' indicator is being reset to ?'
          revp(i, l) = '?'
        endif
332   continue
      return
      end
c end rdtmdp
