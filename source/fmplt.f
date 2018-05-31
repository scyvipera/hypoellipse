c fmplt.for    []
      subroutine fmplt(msta, az, ain, fm, lph, xscale, repeat, punt,
     *                 cp, sp)
c plot first-motion directions on the lower focal hemisphere
c------- in equal area projection
      include 'params.inc' 
      integer punt
      character*1 fm(npa), jtem, igrap(95, 95)
      character*5 msta(npa)
      dimension az(npa), ain(npa)
      dimension cp(72), sp(72)
      logical repeat
      data nox, noy, iy, noy1, noy2/95, 59, 24, 57, 30/
      data rmax, yscale, add/3.937008, 0.169643, 4.75/
      ntin = 0
    6 ntin = ntin + 1
      nfmr = 0
c zero graph
      do 10 i = 1, nox
      do 10 j = 1, noy
   10 igrap(i, j) = ' '
c make circle of *'s
      do 20 i = 1,72
        jx = (rmax*cp(i)+add)/xscale + 1.5
        jy = (rmax*sp(i)+add)/yscale + 0.5
        jy = noy - jy - 1
        ii = i - (i/2)*2
        igrap(jx, jy) = '*'
        if (ii .eq. 0) igrap(jx, jy) = ' '
   20 continue
      nox2 = add/xscale + 1.5
      it = (-rmax + add)/xscale + 1.5
      igrap(it, noy2) = '-'
      it = (rmax + add)/xscale + 2.5
      igrap(it, noy2) = '-'
      it = noy2 - iy - 1
      igrap(nox2, it) = 'i'
      it = noy2 + iy + 1
      igrap(nox2, it) = 'i'
c     center is nox2, noy2
      igrap(nox2, noy2) = '*'
      do 50 i = 1, lph
        if (fm(i) .eq. ' ') goto 50
        if (ain(i) .gt. 90.) goto 30
        ann = ain(i)
        azz = az(i)*.0174533
        goto 32
   30   ann = 180. - ain(i)
        azz = (180. + az(i) )*.0174533
   32   r = rmax*1.414214*sin(ann*.0087266)
        x = r*sin(azz) + add
        y = r*cos(azz) + add
        jx = x/xscale + 1.5
        jy = y/yscale + .5
        jy = noy - jy - 1
        if (ntin .eq. 2) goto 52
        jtem = igrap(jx, jy)
c if previous symbol is weak, overwrite it.
        if ((jtem.eq.' ').or.(jtem.eq.'*').or.(jtem.eq.'+')
     *  .or.(jtem.eq.'-').or.(jtem.eq.' ').or.(jtem.eq.'?')) goto 47
c if new symbol is weak, do not overwrite strong symbol.
        if ((fm(i).eq.'+').or.(fm(i).eq.'-').or.(fm(i).eq.'?'))
     *  goto 50
        if (fm(i) .eq. 'c' ) goto 40
c plot d on top of previous strong symbol
        if (igrap(jx, jy) .ne. 'd') goto 35
        igrap(jx, jy) = 'e'
        goto 50
   35   if (igrap(jx, jy) .ne. 'e') goto 37
        igrap(jx, jy) = 'f'
        goto 50
   37   if (igrap(jx, jy) .eq. 'f') goto 50
        igrap(jx, jy) = 'x'
        goto 50
c plot c on top of privious strong symbol
   40   if (igrap(jx, jy) .ne. 'c') goto 43
        igrap(jx, jy) = 'b'
        goto 50
   43   if (igrap(jx, jy) .ne. 'b') goto 45
        igrap(jx, jy) = 'a'
        goto 50
   45   if (igrap(jx,jy) .eq. 'a') goto 50
        igrap(jx, jy) = 'x'
        goto 50
   47   igrap(jx, jy) = fm(i)
        goto 50
c write station name on focal sphere
   52   njx = jx - 1
        do 56 j = 1, 4
          if (msta(i)(j:j) .ne. ' ') igrap(njx, jy) = msta(i)(j:j)
          njx = njx + 1
   56   continue
   50 continue
      nm1 = noy1 - 1
      do 80 i = 4, nm1
        write(punt, 55) (igrap(j, i), j = 1, nox)
   55   format(21x, 95a1)
   80 continue
      if ((repeat) .and. (ntin .eq. 1)) then
        write(punt, 82)
   82   format(1h1, /)
        goto 6
      endif
      return
      end
c end fmplt
