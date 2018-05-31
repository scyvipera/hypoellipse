c iprst.for    []
      subroutine iprst(l,llat,ins,alat,llon,iew,alon)
c     print out station data
      include 'params.inc' 
      parameter (ndly = 11)
      character*1 ins, iew
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /dhip/ inpt,isa,ilis,inmain,injump
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      integer fmwt, xmwt
      common /ioxl/ fmwt(nsn), xmwt(nsn)
      common /ilpu/ sw(nsn),ndate(nsn),nhr(nsn),mno(nsn),ielv(nsn)
      common /ilpx/ calr(5, nsn),fmgc(nsn),xmgc(nsn)
      logical fsteq
      character*1 revp, exdly
      common /ip/ latr,lonr,tpdly(2,nsn),infil,iofil,indexs,
     *  iscod(nsn),ipcod(nsn), fsteq, exdly(4, nsn), revp(6, nsn)
      common /int/ thk(lmax+1),lbeg(mmax),lend(mmax),vi(lmax),vsq(lmax),
     *  vsqd(lmmax,lmax),f(lmax,lmmax),kl,ivway,sthk(mmax),sthk1(mmax),
     *  sdly(ndly,nsn)
      common /ilt/ vthk(2,nsn),ipdly(nsn),mod(nsn),ipthk(nsn)
c
      if (ilis .gt. 0)
     * write(punt,100) nsta(l), llat, ins,   alat, llon, iew,    alon,
     *  ielv(l), ipthk(l), vthk(1,l), vthk(2,l), mod(l), ipdly(l),
     * (dly(i,l), sdly(i,l),i=1,5), klas(1, l), calr(1, l), xmgc(l),
     * xmwt(l), fmwt(l), fmgc(l), ipcod(l), iscod(l), (revp(i,l),i=1,6),
     * sw(l), tpdly(1,l), (exdly(ii, l), ii = 1, 4),
     * tpdly(2,l), ndate(l), nhr(l)
  100 format(        1x,a5, 1x,i2,  a1,1x,f5.2,1x,i3,   a1, 1x,f5.2,
     *      i5,        i2,             1x,2f4.2,     i3,       i2,
     * 1x,             5(2f5.2,1x),        i2,    1x,f5.2,  1x,f4.2,
     *      i2,      i2, 1x,f4.2,             1x,2i2,      /,' * ', 6a1,
     *  3x,f4.1, 1x, f5.2,                      1x,4a1,
     *   1x,f5.2,   1x, i8,  1x, i2)
      return
      end
c end iprst
