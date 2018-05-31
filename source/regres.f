c regres.for    []
      subroutine regres
c compute geiger adjustments by regression
c    first rotate coordinates so that the cross-correlation
c    matrix is diagonalized.  output includes orientation and
c    size of error ellipse.
      include 'params.inc' 
      parameter (ndly = 11)
      character*4 iahead*60, msta*5, nsta*5, icard*110
      integer punt
      common /punt/ punt
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /dmost/ ipun,ivlr,blank
      common /imost/ test(100)
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /oqr/ y(4),kz,at(3),tl(3),pdrms,b(4)
      common /orz/ onf
      common /phoqn/ inst,knst
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /qgnotx/ delta(npa)
      common /qmost/ wt(npa),z
      common /zqr/ fno
      common /rfgnoq/ se(4)
      common /rbno/ idip(3),iaaz(3),a(3,3)
      common /rioq/ been,damp,dmpinc,igo
      common /ro/ yse,seorg,phi
      common /rob/ v(4,4),noaxp
      common /rq/ xmean(4),avwt,aveaj
      common /tmost/ x(4,npa)
      common /tonxb/ t(npa),fms(npa)
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      dimension r(3),prch(npa),h(3,3),hh(3,3)
      dimension xsum(4),sigma(4),std(4)
      dimension svx3(npa), svx2(npa)
      dimension asm1(6), ev(3,3), eval(3)
      dimension cent(4,4), sscol(4), xm(3)

c test(29) is the default vlaue of standard error used when there
c    are no degrees of freedom.  if test(29) is negative it is always 
c    used regardless of the number of degrees of freedom.
c
c nrwt is the total number of readings with weight .gt. zero.
c
c fno is the sum of p, s, and s-p weights.
c     fno may not equal nrwt because the weights are not normalized.
c
c npors is the number of p and s phases.
c
c onf is the sum of p and s wieghts.
c
c kz is the number of the fixed component, if any.
c
c if solution diverged, increase damping. in this case there is no 
c   need to recompute the sum of squares matrix.
c
      nrsave = nr
      if(inst .eq. 9) yse = abs(test(29))
      if(test(34) .ne. -1.0) then
c fix epicenter if test(30) is negative and if not final call
c for error ellipsoid (test(34) = -1.0)
        if(test(30) .lt. 0.) then
	  if(test(30) .eq. -13.) then
c in this unadvertised, temporary test option, fix long and z
            do 101 i = 1, nr
              svx2(i) = x(1, i)
              svx3(i) = x(3, i)
              x(1, i) = 0.0
	      x(3, i) = 0.0
101         continue
	  else if(test(30) .eq. -23.) then
c in this unadvertised, temporary test option, fix long and z
            do 102 i = 1, nr
              svx2(i) = x(2, i)
              svx3(i) = x(3, i)
              x(2, i) = 0.0
	      x(3, i) = 0.0
102         continue
          else
            do 11 i = 1, nr
              svx2(i) = x(1, i)
              svx3(i) = x(2, i)
              x(1, i) = 0.0
              x(2, i) = 0.0
11          continue
          endif
        else

c if kz > 0, fix the component specified by kz
          if(kz .gt. 0) then
            do 12 i = 1, nr
              svx3(i) = x(kz, i)
              x(kz, i) = 0.0
12          continue

c if test(47) ne 0, igo = 1, and kz <= 0,
c then add extra equation to fix on plane
          else if((test(47) .ne. 0.) .and. (igo .eq. 1)) then
            nr = nr + 1
            call rplain
          endif
        endif
      endif
      phi = nrwt

      npors = 0
      do 13 i = 1, nr
        npors = npors + ksmp(i)
13    continue

c reduce deg of freedom by one if origin time will be calculated
      if(npors .ge. 1) phi = phi - 1.
c compute means of p and s data.
      do 14 j = 1, 4
        xsum(j) = 0.0
        xmean(j) = 0.0
        sscol(j) = 0.0
        do 14 k = 1, 4
          v(j, k) = 0.0
14    continue

      if( (npors .ne. 0) .and. (onf .ne. 0.0) .and.
     *  (inst .ne. 8) ) then
        do 15 j = 1, 4
        do 15 i = 1, nr
          xsum(j) = xsum(j) + wt(i)*x(j, i)*ksmp(i)
15      continue
        do 16 j = 1, 4
          xmean(j) = xsum(j)/onf
16      continue
      endif

      if(test(34) .gt. 0) then
c       compute sum of squares of each column, corrected for mean,
c       for purposes of scaling.  skip if test(34) = 0.0 or if on
c       final call to get error ellipsoid (test(34) = -1.0)
        do 18 j = 1, 4
          do 17 i = 1, nr
            sscol(j) = sscol(j) + wt(i)*(x(j, i)-xmean(j)*ksmp(i))*
     *      (x(j, i)-xmean(j)*ksmp(i))
17        continue
          if(sscol(j) .lt. .00000001) sscol(j) = 1.0
18      continue

c       compute inverse of sqrt of sum of products of squares of 
c         each column, to be used in scaling.
        do 19 j = 1, 4
          do 19 k = j, 4
            cent(j, k) = 1./(sqrt(sscol(j))*sqrt(sscol(k)))
19      continue
      else
        do 21 j = 1, 4
          sscol(j) = 1.0
          do 20 k = 1, 4
            cent(j, k) = 1.0
20        continue
21      continue
      endif

c compute normal equations: v is corrected for mean and scaled by cent
      do 23 j = 1, 4
      do 23 k = j, 4
        do 22 i = 1, nr
          v(j, k) = v(j, k) + 
     *             wt(i)*(x(j, i)-xmean(j)*ksmp(i))*
     *                   (x(k, i)-xmean(k)*ksmp(i))*cent(j, k)
22      continue
        v(k, j) = v(j, k)
23    continue

c output original data, means, std. deviations.
c compute sigma and standard deviations.
      if(iprn .ge. 3) then
        do 24 j = 1, 4
          sigma(j) = sqrt(v(j, j))
          if(phi .gt. 0.0) then
            std(j) = sigma(j)/sqrt(fno)
          else
            std(j) = 0.0
          endif
24      continue

        write(punt, '(a, e15.7)') ' depth = ', z
        write(punt, 25)
25      format(
     *   45x, 'original data', /, '  delta(km)     t(sec)',
     *   '    x(1, i)        x(2, i)        ',
     *   'x(3, i)        x(4, i)          wt(i)      ksmp(i)  name', /)
        do 27 i = 1, nr
          write(punt, 26) delta(i), t(i),
     *      (x(j, i), j = 1, 4), wt(i), ksmp(i), msta(i)
26        format(1x, 2e11.4, 5e15.7, i10, 1x, a5)
27      continue
        write(punt, 28) sscol
28      format(' scale ss=', 13x, 4e15.7)
        write(punt, 29) xmean, onf, sigma, std
29      format('   xmean =', 13x, 5e15.7, /'   sigma =', 13x, 4e15.7, /
     *   ' std dev =', 13x, 4e15.7)
      endif

c find solution to upper left 2x2 by determinates
c  scaled
c      dlons = v(1, 4)*v(2, 2) - v(2, 4)*v(1, 2)
c      dlons = dlons/(v(1, 1)*v(2, 2) - v(1, 2)**2)
c      print *, 'dlons, still scaled, = ', dlons
c      dlons = dlons*sqrt(sscol(4)/sscol(1))
c      print *, '2x2 dlon = ', dlons
c      write(punt, '(a, 2f12.4)') ' 2x2 dlon = ', dlons

c find eigenvalues [diagonal elements of asm1(6)] and eigenvectors
c     [ev(3, 3)] for the upper left 3x3 portion of v(4, 4) [sum of squares]
c	print *, 'v(1,1), (1,2), (1,3) ', v(1,1), v(1,2), v(1,3)
c	print *, 'v(2,1), (2,2), (2,3) ', v(2,1), v(2,2), v(2,3)
c	print *, 'v(3,1), (3,2), (3,3) ', v(3,1), v(3,2), v(3,3)
c	print *, 'v(1,4), v(2,4), v(3,4) ', v(1,4), v(2,4), v(3,4)
30    call eigen1(asm1, 3, 4, 3, ev, eval, v, damp)
      do 31 j = 1, 3
      do 31 i = 1, j
        ni = i + (j*j-j)/2
        a(i, j) = asm1(ni)
        a(j, i) = a(i, j)
31    continue
c	print *, 'a(1,1), (1,2), (1,3) ', a(1,1), a(1,2), a(1,3)
c	print *, 'a(2,1), (2,2), (2,3) ', a(2,1), a(2,2), a(2,3)
c	print *, 'a(3,1), (3,2), (3,3) ', a(3,1), a(3,2), a(3,3)
c transform xmean(k) into principle axis system.
c transform the response vector v(j, 4), j = 1, 3 to r(j)
      do 32 j= 1, 3
        xm(j) = 0.0
        r(j) = 0.0
        do 32 k = 1, 3
          xm(j) = xm(j) + ev(k, j)*xmean(k)
          r(j) = r(j) + ev(k, j)*v(k, 4)
32    continue
c	print *, 'r(1), r(2), r(3) ', r(1), r(2), r(3)

c calculate azmuth and dip of principle directions.
      if(test(34) .eq. -1.0) then
        do 33 j = 1, 3
          if(ev(1, j) .eq. 0.) then 
            iaaz(j) = 0
          else
            azq = atan2(-ev(1, j), ev(2, j))*57.29578
            call formf(azq, iaaz(j), 4, 0)
          endif
          d = sqrt(ev(1, j)**2+ev(2, j)**2)
          dpq = atan2(ev(3, j), d)*57.29578
          call formf(dpq, idip(j), 4, 0)
          if(idip(j) .lt. 0.) then
            idip(j) = -idip(j)
            iaaz(j) = iaaz(j) + 180
          endif
          if(iaaz(j) .lt. 0.) then
            iaaz(j) = iaaz(j) + 360.
          endif
33      continue
      endif

c calculate solution vector in transformed coordinates.
      do 34 j = 1, 3
        if(a(j, j) .gt. damp) then
          at(j) = (r(j)/a(j, j))
        else
          at(j) = blank
        endif
34    continue

c calculate final step in original coordinates.
      do 35 j = 1, 3
        b(j) = 0.0
        do 35 k = 1, 3
          if(at(k) .eq. blank) goto 35
          b(j) = b(j) + ev(j, k)*at(k)
35    continue
      do 36 j = 1, 3
        b(j) = b(j)*sqrt(sscol(4)/sscol(j))
36    continue

c calculate origin time correction.
      b(4) = xmean(4) - b(1)*xmean(1)-b(2)*xmean(2)-b(3)*xmean(3)

      do 37 j = 1, 4
        y(j) = b(j)
37    continue

c calculate predicted sum of squares of residuals after move.
c     write(punt, '(a, /, (1x, 4f15.7))') ' cent(i,j) = ', cent
      pss = v(4, 4)/cent(4, 4)
      do 38 j = 1, 3
        phi = phi - 1.
        pss = pss - b(j)*v(j, 4)/cent(j, 4)
38    continue

c calculate predicted rms.
      pdrms = 10.
      if(fno .gt. 0.) pdrms = sqrt(abs(pss/fno))
      if (iprn .ge. 5) write(punt, '(a, /, 4e15.7)')
     *'           damp            pss          pdrms            phi',
     *damp, pss, pdrms, phi

c estimate standard error, yse.
      if(test(29) .lt. 0.0) then
c       always use test(29) for yse if it is negative.
        yse = -test(29)
      else
        if(phi .ge. 1.) then
          yse = sqrt( v(4, 4)*nrwt / (fno*phi) )
          if(yse .lt. test(29)) then
            yse = test(29)
          endif
        else
          yse = test(29)
        endif
      endif

      if (iprn .ge. 5) write(punt, '(a, /, i15, 3e15.7)')
     *'           nrwt            fno            onf            yse',
     *nrwt, fno, onf, yse

c calculate standard errors and ratios of steps to standard error
c at(j) goes into common
      aveaj = 0.0
      do 39 j = 1, 3
        se(j) = 99.0
        if(at(j) .eq. blank) then
          at(j) = 0.0
        else
          se(j) = 1.87*yse/sqrt(a(j, j))
          if(se(j) .gt. 99.0) se(j) = 99.0
        endif
        aveaj = aveaj + a(j, j)
39    continue
      aveaj = aveaj/3.

c calculate se of origin time.  max is 10.
      seorg = 10.
      if ((inst .ne. 8) .and. (onf .gt. 0.0)) then
        vari = 1./onf
        do 40 i = 1, 3
          if(a(i, i) .lt. 0.000001) goto 41
          vari = vari + xm(i)**2/a(i, i)
40      continue
        seorg = yse*sqrt(vari)
        if(seorg .gt. 10.) seorg = 10.
      endif
41    continue

c for expanded output write out eigenvectors (transformation
c    matrix) and transformed equations.
      if(iprn .ge. 5) then
        do 42 i = 1, 3
          do 42 j = 1, 3
            h(i, j)  = 0.0
            hh(i, j) = 0.0
42      continue

        write(punt, 43)
43      format(/, ' normal equations, scaled and corrected for mean')
        do 45 i = 1, 3
          write(punt, 44) (v(i, j), j = 1, 4)
44        format(10x, e15.7, 8hxdlon + , e15.7, 8hxdlat + ,
     *    e15.7, 6hxdz = , e15.7)
45      continue

        write(punt, 49) v(4, 4)
49      format(32x, 
     *   ' corrected & scaled sum of sq of residuals = ', e15.7)
        write(punt, 50)
50      format(/, ' transformation matrix, cols are eigenvectors')
        do 52 i = 1, 3
          write(punt, 51) (ev(i, j), j = 1, 3)
51        format (10x, 3e15.7)
52      continue

c transform the normal equation coeficients to hh(3, 3).
        do 53 i = 1, 3
        do 53 j = 1, 3
        do 53 k = 1, 3
          h(i, j) = h(i, j) + v(i, k)*ev(k, j)
53      continue
        do 54 i = 1, 3
        do 54 j = 1, 3
        do 54 k = 1, 3
          hh(i, j) = hh(i, j) + ev(k, i)*h(k, j)
54      continue
        write(punt, 55)
55      format(/, ' transformed coeficients')
        do 57 i = 1, 3
          write(punt, 56) (hh(i, j), j = 1, 3)
56        format(10x, 3e15.7)
57      continue
c for expanded output write out transformed equations.
        write(punt, 58)  (iaaz(j), idip(j), j = 1, 3)
58      format(/15x, ' azmuth and dip of principal directions ',
     *  /, 10x, 3(5x, i5, 1h/, i3))
        write(punt, 59)
59      format(/10x, ' steps in directions of eigenvectors',
     *  '  step      std error              t')
        do 61 i = 1, 3
          write(punt, 60) i, r(i), a(i,i), at(i), se(i)
60        format
     *    (10x, ' step(', i1, ') = ', e15.7, '/', e15.7, ' = ', 3e15.7)
61      continue

c for expanded output calculate predicted change in residual, (prch(i))
c   these are based on the step to be taken.
        write(punt, 62) b
62      format(/8h dlon = , e15.7, 8h dlat = , e15.7, 6h dz = , e15.7,
     *    6h dt = , e15.7)
        write(punt, 63)
63      format('         current        predicted      predicted      ',
     *  'weight     name', /,
     *       '         residual       tt change      new residual   ')
        xsum(1) = 0.0
        do 66 i = 1, nr
          prch(i) = -b(4)*ksmp(i)
          do 64 j = 1, 3
            prch(i) = prch(i) - x(j, i)*b(j)
64        continue
          anewr = x(4, i) + prch(i)
          xsum(1) = xsum(1) + wt(i)*anewr**2
          write(punt, 65) i, x(4,i), prch(i), anewr, wt(i), msta(i)
65        format(1x, i2, 4f15.7, 1x, a5)
66      continue
        write(punt, 67) v(4, 4), pss, xsum(1)
67      format(/,
     *  ' sum of squares:    original       predicted       linear',
     *  /, 16x, 3e15.7)
      endif

      nr = nrsave
      if(test(30) .lt. 0.) then
        if(test(30) .eq. -13.) then
          do 675 i = 1, nr
            x(1, i) = svx2(i)
            x(3, i) = svx3(i)
675       continue
        else if(test(30) .eq. -23.) then
          do 676 i = 1, nr
            x(2, i) = svx2(i)
            x(3, i) = svx3(i)
676       continue
        else
          do 68 i = 1, nr
            x(1, i) = svx2(i)
            x(2, i) = svx3(i)
68        continue
        endif
      else
        if(kz .gt. 0) then
          do 69 i = 1, nr
            x(kz, i) = svx3(i)
69        continue
        endif
      endif

      if(test(34) .eq. -1.0) then
        do 692 j = 1, 4
          y(j) = 0.0
692     continue
      endif

      return
      end
c end regres
