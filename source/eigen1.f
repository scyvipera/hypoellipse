c eigen1.for    []
      subroutine eigen1(a, mev, mv, n, ev, s, v, damp)
c used to find eigenvalues and eigenvectors for the upper left nxn
c     portion of v(mv, mv)
c
c modified from an ibm sub. by j. c. lahr
c
c     dimension of r must be greater than n**2,
c     so n may not exceed 10 in this version
c              mev              (input) actual dimensions of ev
      integer  mev
c              mv               (input) actual dimensions of v
      integer  mv
c              n                (input) tensor dimension
      integer  n
c              a(n + (n*n-n)/2) input v transformed to storage mode 1
      real     a(n + (n*n-n)/2)
c              damp             (input) term to be added to diagonal elements
      real     damp
c              ev(mev, mev)     (output) eigenvectors
      real     ev(mev, mev)
c              r(100)           eigenvectors as linear array
      real     r(100)
c              s(n)             (output) eignevalues
      real     s(n)
c              v(mv, mv)        (input) symetric tensor to be diagonalized
      real     v(mv, mv)
c
      do 10 j = 1, n
      do 10 i = 1, j
        ni = i + (j*j-j)/2
        a(ni) = v(i, j)
        if (i .eq. j) a(ni) = a(ni) + damp
10    continue
      iq = -n
      do 20 j = 1, n
        iq = iq+n
        do 20 i = 1, n
          ij = iq+i
          r(ij) = 0.0
          if (i-j) 20, 15, 20
15        r(ij) = 1.0
20    continue
      anorm = 0.0
      do 35 i = 1, n
      do 35 j = i, n
        if (i-j) 30, 35, 30
30      ia = i+(j*j-j)/2
        anorm = anorm+a(ia)*a(ia)
35    continue
      if (anorm) 165, 165, 40
40    anorm = 1.414*sqrt(anorm)
      anrmx = anorm*1.0e-6/float(n)
      ind = 0
      thr = anorm
45    thr = thr/float(n)
50    l = 1
55    m = l+1
60    mq = (m*m-m)/2
      lq = (l*l-l)/2
      lm = l+mq
      if ( abs(a(lm))-thr) 130, 65, 65
65    ind = 1
      ll = l+lq
      mm = m+mq
      x = 0.5*(a(ll)-a(mm))
      y = -a(lm)/ sqrt(a(lm)*a(lm)+x*x)
      if (x) 70, 75, 75
70    y = -y
75    sinx = y/sqrt(2.0*(1.0+(sqrt((1.-y)*(1.+y)))))
      sinx2 = sinx*sinx
      cosx = sqrt((1.-sinx)*(1.+sinx))
      cosx2 = cosx*cosx
      sincs  = sinx*cosx
      ilq = n*(l-1)
      imq = n*(m-1)
      do 125 i = 1, n
        iq = (i*i-i)/2
        if (i-l) 80, 120, 80
80      if (i-m) 85, 120, 90
85      im = i+mq
        goto 95
90      im = m+iq
95      if (i-l) 100, 105, 105
100     il = i+lq
        goto 110
105     il = l+iq
110     x = a(il)*cosx-a(im)*sinx
        a(im) = a(il)*sinx+a(im)*cosx
        a(il) = x
120     ilr = ilq+i
        imr = imq+i
        x = r(ilr)*cosx-r(imr)*sinx
        r(imr) = r(ilr)*sinx+r(imr)*cosx
        r(ilr) = x
125   continue
      x = 2.0*a(lm)*sincs
      y = a(ll)*cosx2+a(mm)*sinx2-x
      x = a(ll)*sinx2+a(mm)*cosx2+x
      a(lm) = (a(ll)-a(mm))*sincs+a(lm)*(cosx-sinx)*(cosx+sinx)
      a(ll) = y
      a(mm) = x
130   if (m-n) 135, 140, 135
135   m = m+1
      goto 60
140   if (l-(n-1)) 145, 150, 145
145   l = l+1
      goto 55
150   if (ind-1) 160, 155, 160
155   ind = 0
      goto 50
160   if (thr-anrmx) 165, 165, 45
165   iq = -n
      do 185 i = 1, n
        iq = iq+n
        ll = i+(i*i-i)/2
        jq = n*(i-2)
        do 185 j = i, n
          jq = jq+n
          mm = j+(j*j-j)/2
          if (a(ll)-a(mm)) 170, 185, 185
170       x = a(ll)
          a(ll) = a(mm)
          a(mm) = x
          do 180 k = 1, n
            ilr = iq+k
            imr = jq+k
            x = r(ilr)
            r(ilr) = r(imr)
180       r(imr) = x
185   continue
      do 190 j = 1, n
        ni = j + (j*j-j)/2
        s(j) = a(ni)
190   continue
      ni = 0
      do 200 i = 1, n
        do 200 j = 1, n
          ni = ni + 1
          ev(j, i) = r(ni)
200   continue
      return
      end
c end eigen1
