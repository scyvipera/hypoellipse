c diftim.for    []
      subroutine diftim(icent1, iymd1,ihr1,icymd2,ihr2,iporm,difhr)
c calculate first time minus second time
c iporm  -1  first cnyrmody is smaller than second, set difhr = -1.0
c             0  first cnyrmody is equal to second, calculate difhr
c            +1  first cnyrmody is larger, set difhr = +1.0
c difhr       this is the time difference in hours, if dates are =
c
      if (icent1*1000000+iymd1 .lt. icymd2) goto 100
      if (icent1*1000000+iymd1 .gt. icymd2) goto 200
c equal dates
      iporm = 0
      difhr = ihr1 - ihr2
      return
c smaller first date
  100 iporm = -1
      difhr = -1.
      return
c larger first date
  200 iporm = +1
      difhr = +1.
      return
      end
c end diftim
