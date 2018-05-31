c riorbk.for    []
      subroutine riorbk(x, ix, jfmt, n, nsig)
c     converts a real number (x) to an integer (ix) that may be printed
c       with no decimal point and later read with fn.nsig format
c     if x = '    ' then ix is set = '    ', and an a format is output
c     jfmt is the format to use in writing ix
c     n is field length and can be from 1 to 7
c
      integer punt
      common /punt/ punt
      common /logfil/ logfil
      character*4 ifmt(7), jfmt
      data ifmt/'(i1)','(i2)','(i3)','(i4)','(i5)','(i6)','(i7)'/
      if((n .gt. 7) .or. (n .lt. 1)) goto 1000
      if((nsig .gt. n) .or. (nsig .lt. 0)) goto 2000
      ix = x*10.**nsig + sign(0.50001,x)
      imax = 10**n
      if(ix .ge. imax) ix = imax - 1
      if(ix .le. (-imax/10)) ix = -imax/10 + 1
      jfmt = ifmt(n)
      return
 1000 write(punt,1010) n
      write(logfil,1010) n
 1010 format(' *** the format i',i5,' is not allowed, so stop.')
      goto 3000
 2000 write(punt,2010) n,nsig
      write(logfil,2010) n,nsig
 2010 format(' *** the format f',i5,'.',i5,' is not allowed, so stop.')
 3000 continue
      stop
      end
c end riorbk
