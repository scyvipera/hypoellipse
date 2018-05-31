c formf.for    []
      subroutine formf(x, ix, n, nsig)
c converts real number x into the integer ix to be
c written with format(in) and read with format(fn.nsig)
c corrections by willy aspinall, principia testing  july 1983
      ix = x*(10.**nsig) + sign(0.50001,x)
      imax = 10**n
      if(ix .ge. imax) ix = imax - 1
      if(ix .le. -imax/10) ix = -imax/10 + 1
      return
      end
c end formf
