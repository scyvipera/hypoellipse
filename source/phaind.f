c phaind.for    []
      integer function phaind(msta,nsta,ns,itarg,ierr)
c     returns itarg, the index of nsta that equals target.
      character*5 msta, nsta(*)
      ierr = 0
      i = 1
      j = ns
   10 mid = (i + j)/2
      if (msta .eq. nsta(mid) ) then
        itarg = mid
        goto 50
      else
        if (msta .lt. nsta(mid) ) then
          j = mid - 1
          if (j .lt. i) goto 20
          goto 10
        else
          i = mid + 1
          if (i .gt. j) goto 20
          goto 10
        end if
      end if
   20 ierr = 1
   50 phaind = ierr
      return
      end
cnch
cnch      integer function phaind(msta,nsta,ns,i,ierr)
c     returns i, the index of nsta that equals msta.
c     9 June 1994 jas/vtso small corrections
cnch      dimension nsta(*)
cnch      character*4 msta, nsta
chch      ierr = 0
cnch      do 40 i=1,ns
cnch      if (msta .eq. nsta(i)) goto 50
cnch   40 continue
cnch      ierr = 1
cnch   50 phaind = ierr
cnch      return
cnch      end
cnch
c end phaind
