c adderr.for    [unix]
      subroutine adderr(zup, zdn)
c adds zup and zdn to the summary record and adds the event
c to file 4, and file 11
      character*117 sumrec
      character*117 arcrec
      character*6 fmit
      common /an/ n14, n15
c* (unix
      call flush (14)
      call flush (15)
c* unix)
c* (pc
c* pc)
c* (vax
c* vax)
      rewind 14
      rewind 15
      if(n14 .gt. 0) then
        if(n14 .gt. 1) then
          print *, ' n14 = ', n14
          print *, ' logic error: n14 should be 0 or 1'
        endif
        do 20 i = 1, n14
          read(14, '(a)') sumrec
          call formal(zup, izup, 2, 0, fmit, azup)
          if (fmit .eq. ' ') then
c           write(sumrec(101:102), '(i2)') izup
            write(sumrec(103:104), '(i2)') izup
          else
c           write(sumrec(101:102), fmit) azup
            write(sumrec(103:104), fmit) azup
          endif
          call formal(zdn, izdn, 2, 0, fmit, azdn)
          if (fmit .eq. ' ') then
c           write(sumrec(103:104), '(i2)') izdn
            write(sumrec(105:106), '(i2)') izdn
          else
c           write(sumrec(103:104), fmit) azdn
            write(sumrec(105:106), fmit) azdn
          endif
          write(4, '(a)') sumrec(1:lentru(sumrec))
20      continue
      endif
      if(n15 .gt. 1) then
        do 30 i = 1, n15
          read(15, '(a)') arcrec
c         if(arcrec(81:81) .ne. '/') then
          if(arcrec(83:83) .ne. '/') then
            write(11, '(a)') arcrec(1:lentru(arcrec))
          else
c summary record
            sumrec = arcrec
            call formal(zup, izup, 2, 0, fmit, azup)
            if (fmit .eq. ' ') then
c             write(sumrec(101:102), '(i2)') izup
              write(sumrec(103:105), '(i2)') izup
            else
c             write(sumrec(101:102), fmit) azup
              write(sumrec(103:105), fmit) azup
            endif
            call formal(zdn, izdn, 2, 0, fmit, azdn)
            if (fmit .eq. ' ') then
c             write(sumrec(103:104), '(i2)') izdn
              write(sumrec(105:106), '(i2)') izdn
            else
c             write(sumrec(103:104), fmit) azdn
              write(sumrec(105:106), fmit) azdn
            endif
c	    print *, 'in adderr, about to write summary rec to unit 11'
c	    print *, 'sumrec = ', sumrec
c	    print *, 'lentru = ', lentru(sumrec)
            write(11, '(a)') sumrec(1:lentru(sumrec))
          endif
30      continue
      endif
c* (unix
      call flush(4)
      call flush(11)
c* unix)
c* (pc
c* pc)
c* (vax
c* vax)
      return
      end
c end adderr
