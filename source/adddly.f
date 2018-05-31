c adddly.for  []
      subroutine adddly(nmodel)
c read in delays for model number greater than 5
      include 'params.inc'
      parameter (ndly = 11)
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt, phaind
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /dhip/ inpt,isa,ilis,inmain,injump
      common /ilmpu/ ns
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /int/ thk(lmax+1),lbeg(mmax),lend(mmax),vi(lmax),vsq(lmax),
     *  vsqd(lmmax,lmax),f(lmax,lmmax),kl,ivway,sthk(mmax),sthk1(mmax),
     *  sdly(ndly,nsn)
      common /logfil/ logfil
      character*5 stard, dnstrg, rshft
      if(ns .eq. 0) then
        write(punt, 18)
        write(logfil, 62)
18      format('xxxerrorxxx delay models may not preceed the ',
     *  'station list')
        stop 'abort from adddly'
      endif
20    read(inpt, '(a)', end=60) icard
      stard = dnstrg(rshft(icard(1:5)))
      if(stard .eq. '  end') return
c get station number (i) for station stard
      if(phaind(stard, nsta, ns, i, ierr) .ne. 0) then
        write(punt, 34) stard, icard
        write(logfil, 34) stard, icard
34      format(' ***>', a5, ' is not on station list, so these delays ',
     *  ' will not be used:', /, a)
        goto 20
      endif
c because standard fortran 77 to can not read an internal file with
c free format, file 14 must be used in the following code!
      rewind 14
      write(14, '(a)') icard(6:110)
      rewind 14
c     read(icard(5:110), *) dly(nmodel, i), sdly(nmodel, i)
      read(14, *) dly(nmodel, i), sdly(nmodel, i)
      goto 20
60    write(punt, 62)
      write(logfil, 62)
62    format('xxxerrorxxx end of file found while reading additional',
     * ' delays, so stop')
      stop 'abort from adddly'
      end
c end adddly
