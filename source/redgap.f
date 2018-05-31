c redgap.for    []
      subroutine redgap(iuse,w,wt,kwr,az,keyd,ldx,nrp)
c reweights stations that geduce a gap of more
c than 60 by 30 or more degrees.
      include 'params.inc' 
      character*4 iahead*60, msta*5, nsta*5, icard*110
      common /char/ iahead, msta(npa), nsta(nsn), icard
      dimension key(npa), ldx(npa)
      dimension iuse(npa),w(npa),wt(npa),az(npa)
      character*1 kwr(npa)
      dimension demp(npa), keyd(npa)
      data amingap /30./
      nst = 1
      noaz = 0
c loop through stations, collecting those with data to be used
      do 8 i = 1, nrp
        k = ldx(i)
        if (wt(i) .eq. 0.) then
          if (k .eq. 0) then
            goto 8
          else if (wt(k) .eq. 0.) then
            goto 8
          endif
        endif
c       found a station with either p or s used
        noaz = noaz + 1
        demp(noaz) = az(i)
    8 continue
    4 call sort(demp,key,noaz)
      nj = noaz + 1
      demp(nj) = demp(1) + 360.
c loop through stations in order of distance, considering
c     any that were weighted out due to distance
      do 30 n = nst, nrp
        i = keyd(n)
        k = ldx(i)
c       check if p arrival was weighted out due to distance
        if (kwr(i) .eq. 'd') goto 5
c       check if s arrival was weighted out due to distance
        if ( (k .ne. 0) .and. (kwr(k) .eq. 'd') ) goto 5
        goto 30
c       found a station that was weighted out due to distance
    5   azi = az(i)
        if(azi .lt. demp(1)) azi = azi + 360.
        do 10 j = 2,nj
          if(azi .lt. demp(j)) goto 20
   10   continue
        j = nj
   20   exgap = demp(j) - demp(j-1)
        if(exgap .le. 2*amingap) goto 30
        rdgap = demp(j) - azi
        tgap = azi - demp(j-1)
        if(tgap .lt. rdgap) rdgap = tgap
        if(rdgap .lt. amingap) goto 30
        wt(i) = w(i)*iuse(i)*.5
        if(wt(i) .gt. 0.) kwr(i) = 'g'
   25   if(k .ne. 0) then
          wt(k) = w(k)*iuse(k)*.5
          if(wt(k) .gt. 0.) kwr(k) = 'g'
        endif
        if(kwr(i) .eq. 'g') then
          noaz = noaz + 1
          demp(noaz) = az(i)
          nst = n + 1
          goto 4
        endif
        if(k .ne. 0) then
          if(kwr(k) .eq. 'g') then
            noaz = noaz + 1
            demp(noaz) = az(i)
            nst = n + 1
            goto 4
          endif
        endif
   30 continue
      return
      end
c end redgap
