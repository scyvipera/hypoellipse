c critic.for    []
      subroutine critic(delta,az,w,iuse,nrp,nr,ldx)
c select critical stations
c   1) closest 4 with p-phase readings.
c   2) additional with p- or s-phase readings if they reduce the a gap > 72 deg
c      by 5 deg or more.
c   3) s is used at critical stations.  if there are no s phases selected,
c      then s is used from the closest non-critical station.
      include 'params.inc' 
      logical swtchp, swtchs
      dimension dtemp(npa),key(npa)
      dimension delta(npa),az(npa),w(npa),iuse(npa),ldx(npa)
      integer punt
      common /punt/ punt
      write(punt,20)
20    format(' determining which stations are critical.')
      do 40 j = 1, nr
        iuse(j) = 0
        if (w(j) .eq. 0.) iuse(j) = 1
40    continue
      do 50 j = 1, nrp
        dtemp(j) = delta(j)
50    continue
      call sort(dtemp,key,nrp)
      ns = 0
      nwt = 0
c always use first 4 with p-wt .gt. 0.0
      do 100 i = 1, nrp
        j = key(i)
c check for p
        if (w(j) .gt. 0.0) then
          nwt = nwt + 1
          iuse(j) = 1
c add s only if p is selected
          if (ldx(j) .ne. 0) then
            k = ldx(j)
            if (w(k) .gt. 0.0) then
              iuse(k)  = 1
              ns = ns + 1
            endif
          endif
        endif
        if (nwt .ge. 5) goto 125
100   continue
c fall through if there are 4 or fewer p phases that have wt <> 0
125   if (nwt .eq. 0) then
        sge72 = 400.
        nge72 = 0.
      else
        call sumgap(iuse, az, nr, sge72, nge72, w)
      endif
c try one at a time for stations that reduce gap.
      do 200 i = 1, nrp
        j = key(i)
        k = ldx(j)
        if (iuse(j) .eq. 1) then
c p already in use
          if (k .gt. 0) then
            if (iuse(k) .eq. 1) then
c   s also in use, so skip on to next phase
              goto 200
            endif
          else
c   no s
            goto 200
          endif
        endif
c either p and/or s are not now being used - check weights
        if (w(j) .eq. 0.) then
          if (k .gt. 0) then
c there is an s phase
            if (w(k) .eq. 0.) then
c p and s weights are 0, so skip to next phase
              goto 200
            endif
          endif
        endif
c either p and/or s has non zero weight and is not yet being used
c try adding the p or s to test gap reduction
        swtchp = .false.
        swtchs = .false.
        if ((iuse(j) .eq. 0) .and. (w(j) .gt. 0.)) then
          swtchp = .true.
          iuse(j) = 1
        else
          swtchs = .true.
          iuse(k) = 1
        endif
        call sumgap(iuse, az, nr, sgtr, ngtr, w)
c check if the number of gaps larger than 72 has been increased
        if (ngtr .gt. nge72) then
          nge72 = ngtr
c check if the reduction in sum of gaps > 72 has been reduced by 5 or more deg
        else if (sgtr .le. (sge72 - 5.)) then
          nge72 = ngtr
          sge72 = sgtr
        else
c there was no or not enough improvement
          if(swtchp) iuse(j) = 0
          if(swtchs) iuse(k) = 0
          goto 200
        endif
c this phase reduced the gap, so use it!
        if (swtchp) then
          nwt = nwt + 1
c add s if available
          if (k .ne. 0) then
            if (w(k) .gt. 0.0) then
              iuse(k)  = 1
              ns = ns + 1
            endif
          endif
        endif
        if (swtchs) then
          ns = ns + 1
        endif
200   continue
400   if (ns .gt. 1) goto 500
c use closest s if none found at any critical station
      do 350 i = 1,nrp
        j = key(i)
        if (ldx(j) .eq. 0) goto 350
        k = ldx(j)
        if (w(k) .le. 0.) goto 350
        iuse(k) = 1
        iuse(i) = 1
        goto 500
350   continue
500   return
      end
c end critic
