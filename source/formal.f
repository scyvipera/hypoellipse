c formal.for    []
      subroutine formal(x, ix, n, nsig, fmit, xout)
c test program for subroutine formal
c      character*6 fmit
c      print *, 'test formal'
c30    n = iaskk ('give number of columns for number', n)
c      print *, 'format f3.1 uses 1 decimal digit.'
c      nsig = iaskk ('give number of decimal digits in read statement',
c     1nsig)
c      x = raskk ('give number to be printed out', x)
c      call formal(x, ix, n, nsig, fmit, xout)
c      if (fmit .eq. ' ') then
c        print *, 'write ', ix, ' with format i',n
c      else
c        print *, 'write ', xout, ' with format ', fmit
c      endif
c      goto 30
c      end
c---- converts real number x into the integer ix to be
c---- written with format(in) and read with format(fn.nsig)
c---- corrections by willy aspinall, principia testing  july 1983
c
c---- if the number is too large to write as an integer and
c---- n - nsig is greater than 1, then find xout and fmit
c---- so that the number may be written as xout with fmit.
c
      character*6 fmit
      ix = x*(10.**nsig) + sign(0.50001,x)
      imax = 10**n
      fmit = '      '
      if(x .gt. 0.) then
c       positive number
        if(ix .ge. imax) then
          if(nsig .lt. 2) then
            ix = imax - 1
            return
          else
c           in this case pass back a format to be used in writing
            call formit(x, xout, fmit, n, 1)
            return
          endif
        endif
        if(ix .le. imax/100) then
          if((n - nsig) .gt. 1) then
c           in this case pass back a format to be used in writing
            call formit(x, xout, fmit, n, 1)
            return
          endif
        endif
      else
c       negative number
        if(ix .le. -imax/10) then
          if(nsig .lt. 2) then
            ix = -imax/10 + 1
            return
          else
c           in this case pass back a format to be used in writing
            call formit(x, xout, fmit, n, 1)
            return
          endif
        endif
        if(ix .ge. -imax/1000) then
          if((n - nsig) .gt. 2) then
c           in this case pass back a format to be used in writing
            call formit(x, xout, fmit, n, 1)
            return
          endif
        endif
      endif
      end
c end formal
