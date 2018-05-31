c convert a station file to the y2k complient format
	character*132 record
cabf58 22.88 134 38.60    3 11   1  011   0                              26.463
cabf*         0 0.00    1          00800720   0        0.00
20	read(5, '(a)', end=90) record
	if(record(5:5) .eq. '*') then
	  read(record(38:45), '(i6, i2)') idate, ihr
	  if(idate .eq. 0) then
	    write(record(38:47), '(a2, i6.6, a2)') '  ', idate, '  '
	  else
	    write(record(38:47), '(a2, i6.6, i2)') '19', idate, ihr
	  endif
	endif
	write(6, '(a)') record(1:lentru(record))
	goto 20
90	stop
	end
	  
c     FINDS THE TRUE LENGTH OF A CHARACTER VARIABLE
      integer function lentru(alph)
      character alph*(*)
      l = len(alph)
      do 100 i = l, 1, -1
      if ((alph(i:i) .ne. ' ') .and. 
     *    (alph(i:i) .ne. '\0')) goto 200
  100 continue
  200 lentru = i
      return 
      end
