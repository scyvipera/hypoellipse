c sumgap.for    []
      subroutine sumgap (iuse, az, nr, sge72, nge72, w)
c used by critic to compute sum of gaps > 72 deg and the number of
c gaps > 72 deg
      include 'params.inc' 
      dimension iuse(npa),az(npa),aztemp(npa),key(npa),w(npa)
      j = 0
      do 100 i = 1,nr
        if ((iuse(i) .eq. 0) .or. (w(i) .le. 0.)) goto 100
        j = j + 1
        aztemp(j) = az(i)
  100 continue
      call sort(aztemp,key,j)
      k = j + 1
      aztemp(k) = aztemp(1) + 360.
      sge72 = 0.
      nge72 = 0
      do 200 i = 1, j
        gap = aztemp(i+1) - aztemp(i)
        if (gap .le. 72) goto 200
        sge72 = sge72 + gap
        nge72 = nge72 + 1
  200 continue
      return
      end
c end sumgap
