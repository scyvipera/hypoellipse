c dubl.for    [unix]
      double precision function dubl(x)
c     the wadati sub uses p and s arrival times that are entered
c     to the nearest .01 sec in single precision.  the purpose of this
c     sub is to convert these numbers to the nearest double
c     precision number, setting less significant decmial digits to zero.
c     this version is 16 times faster than dubl1, which uses internal
c     write and read statements.
c     lahr & stephens   feb 1986
c
c* (vax
c      double precision dfact
c      if (x .eq. 0.) then
c        dubl = 0.d0
c        return
c      endif
c      al = alog10(abs(x))
c      if (al .lt. 0.) al = al - .9999995
c      i = al
c      dfact = 10.d0**(7-i)
c      ix = x*dfact + sign(.5, x)
c      dubl = dflotj(ix)/dfact
c* vax)
c* (pc
c* (unix
      dubl = x
c* unix)
c* pc)
      return
      end
c end dubl
