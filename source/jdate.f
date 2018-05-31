c jdate.for    [unix]
      subroutine jdate(irmo, irdy, iryr, ihr, imn, isec)
c* (vax
c      character*8 atime
c      call idate(irmo,irdy,iryr)
c      call time(atime)
c      read(atime, 10)  ihr, imn, isec
c10    format(i2, 1x, i2, 1x, i2)
c* vax)
c* (unix
      integer time
      character*24 ctime, nowtime
      character*3 month(12), dnstrg, mo
      data month/'jan', 'feb', 'mar', 'apr', 'may', 'jun',
     *           'jul', 'aug', 'sep', 'oct', 'nov', 'dec'/
c* unix)
c* (pc
c      call getdat(iryr, irmo, irdy)
c      iryr = iryr - (iryr/100)*100
c      call gettim(ihr, imn, isec, i100th)
c* pc)
c* (unix
      itime = time()
      nowtime = ctime(itime)
      print *, nowtime
      read(nowtime, 20) mo, irdy, ihr, imn, isec, iryr
20    format(4x, a3, 1x, i2, 1x, i2, 1x, i2, 1x, i2, 3x, i2)
      do 30 i = 1, 12
        if (dnstrg(mo) .eq. month(i)) irmo = i
30    continue
c* unix)
      return
      end
c end jdate
